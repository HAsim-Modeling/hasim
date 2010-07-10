import GetPut::*;
import ClientServer::*;
import Vector::*;
import List::*;
import FIFO::*;
import ModuleCollect::*;
import Connectable::*;

import soft_connections::*;

//Instantiate a top-level module where dangling connections are errors

module instantiateWithConnections#(Connected_Module#(inter_T) m) (inter_T);

  match {.m2, .col} <- liftModule(getCollection(m));
  
  connectTopLevel(col, m);
  
  return m2;
  
endmodule

//Connect things at the top level.
//Danglings are an error, and chains are "closed"
//No wires come out of this

//connectTopLevel :: [ConnectionData] -> Module ()

module connectTopLevel#(List#(ConnectionData) ld, inter_T i) ();

  //Group connections by type  
  match {.sends, .recvs, .chns} = splitConnections(ld);
  
  match {.dsends, .drecvs} <- connect(sends, recvs);

  //Error out if there are dangling connections
  Bool error_occurred = False;

  //Final Dangling sends
  for (Integer x = 0; x < length(dsends); x = x + 1)
  begin
    let cur = dsends[x];
    if (!cur.optional)
    begin
        messageM(strConcat("ERROR: Dangling Send: ", cur.cname));
        error_occurred = True;
    end
  end
  
  //Final Dangling recs
  for (Integer x = 0; x < length(drecvs); x = x + 1)
  begin
    let cur = drecvs[x];
    if (!cur.optional)
    begin
        messageM(strConcat("ERROR: Dangling Receive: ", cur.cname));
        error_occurred = True;
    end
  end
    
  let mychains <- connectChains(chns);
  
  //Close the chains
  for (Integer x = 0; x < valueof(CON_NumChains); x = x + 1)
  begin
    if(!sameFamily(mychains[x].incoming.clk,mychains[x].outgoing.clk))
    begin
      messageM("CrossDomain@Top");
    end
    mkConnection(mychains[x], mychains[x]);
  end

  if (error_occurred)
    error("Error: dangling connections at top-level.");
  
endmodule


//The main connection algorithm 

//connect :: [CSend_Info] -> [CRecv_Info] -> Module ([CSend_Info], [CRecv_Info])

module connect#(List#(CSend_Info) sends, List#(CRecv_Info) recvs) (Tuple2#(List#(CSend_Info), List#(CRecv_Info)));
  
  //Duplicates within a synthesis boundary are an error
  let nDupSends <- checkDuplicateSends(sends);
  let nDupRecvs <- checkDuplicateRecvs(recvs);
  
  if (nDupSends != 0 || nDupRecvs != 0)
    error("Duplicate connection names detected.");
  
  //Okay no duplicates
  
  //Group things together by name. Leftovers are "dangling"
  match {.dsends, .drecvs, .cncts} = groupByName(sends, recvs);
  
  let nCncts = length(cncts);
  
  //Actually Connect the Connections
  for (Integer x = 0; x < nCncts; x = x + 1)
  begin
  
    match {.cout, .cin} = cncts[x];
    
    //Type-Checking: we perform a rudimentary sanity check here
    //But not across synthesis boundaries yet
    
    // Make sure connections match or consumer is receiving it as void
    if (cin.ctype != cout.ctype)
    begin
      messageM(strConcat("Detected send type: ", cout.ctype));
      messageM(strConcat("Detected receive type: ", cin.ctype));
      error(strConcat("ERROR: data types for Connection ", strConcat(cin.cname, " do not match.")));
    end
    else
    begin  //Actually do the connection
      if(sameFamily(cin.conn.clk,cout.conn.clk))
      begin
        messageM(strConcat("Connecting dangling port: ", cin.cname));
      end
      else
      begin
        messageM(strConcat("CrossDomain@", cin.cname));
      end

      mkConnection(cout.conn, cin.conn);
    end
  
  end
  
  return tuple2(dsends, drecvs);

endmodule
  

//Connect local chains and expose the head and tail at synthesis boundaries

//connectChains :: [CChain_Info] -> [CON_Chain]

module connectChains#(List#(CChain_Info) chns) (Vector#(CON_NumChains, CON_Chain));

  Vector#(CON_NumChains, CON_Chain) mychains = newVector();
    
  Vector#(CON_NumChains, List#(CON_Chain)) tmp_cs = replicate(Nil);
  
  let cs = groupChains(chns);

  for (Integer x = 0; x < valueOf(CON_NumChains); x = x + 1)
  begin
  
    Integer nLinks = length(cs[x]);
  
    CON_Chain tmp <- (nLinks == 0) ? mkPassThrough(x) : connectLocalChain(cs[x]);

    mychains[x] = tmp;
    
  end
  
  return mychains;
  
endmodule

//Connect the local links of a chain connection together.

//connectLocalChain :: [CChain_Info] -> CON_Chain

module connectLocalChain#(List#(CChain_Info) l) (CON_Chain);

  case (l) matches
    tagged Nil:
      return error("Internal Chain Connection failed");
    default:
    begin
      CChain_Info c = l[0];
      messageM(strConcat(strConcat("Adding Link Chain [", integerToString(c.cnum)), "]"));
      CChain_Info cbegin = c;
      let nLinks = length(l);
      //Connect internal chains
      for (Integer x = 1; x < nLinks; x = x + 1)
      begin
        CChain_Info c2 = l[x];
        //Sanity-check the types
        if ((c.ctype != c2.ctype) && (c.ctype != "") && (c2.ctype != ""))
        begin
          messageM(strConcat("ERROR: data types for Chain #", strConcat(integerToString(c.cnum), " do not match.")));
          messageM(strConcat("Detected chain type: ", c.ctype));
          messageM(strConcat("Detected link type: ", c2.ctype));
        end
        else  //Connect 'em up
        begin
          if(sameFamily(c.conn.incoming.clk,c2.conn.outgoing.clk))
          begin
            messageM(strConcat(strConcat("Adding Chain Link [", integerToString(c.cnum)), "]"));
          end
          else
          begin
            messageM(strConcat("CrossDomain@Chain",integerToString(c.cnum)));
          end

          mkConnection(c.conn, c2.conn);

          c = c2;
        end
      end
      CChain_Info cend = c;
      
      //The final chain enqueues to the head and dequeues from the tail
      return (interface CON_Chain;
                interface incoming = cbegin.conn.incoming;
                interface outgoing = cend.conn.outgoing;
              endinterface);
    end
  endcase

endmodule


//If there are no links then it's just a pass-through queue

module mkPassThrough#(Integer chainNum)
    //interface:
                (CON_Chain);

  messageM(strConcat(strConcat("Making Pass-Through Chain [", integerToString(chainNum)), "]"));

  Clock clock <- exposeCurrentClock;
  Reset reset <- exposeCurrentReset;

  FIFO#(CON_CHAIN_Data) passQ <- mkFIFO();
  PulseWire en_w <- mkPulseWire();
  
  interface CON_CHAIN_In incoming;

    method Action get_TRY(CON_CHAIN_Data x);
      passQ.enq(x);
      en_w.send();
    endmethod

    method Bool get_SUCCESS();
      return en_w;
    endmethod

    interface clk = clock;
    
    interface rst = reset;

  endinterface

  interface CON_CHAIN_Out outgoing;

    method CON_CHAIN_Data try() = passQ.first();

    method Action success = passQ.deq();

    interface clk = clock;
    
    interface rst = reset;

  endinterface
endmodule
