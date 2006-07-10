///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// HASim.bsv                                                                 //
//                                                                           //
// Top-level interfaces for the HASim partitioned simulator. Almost every    //
// package will import this file.                                            //
//                                                                           //
// Note that all interfaces are polymorphic.                                 //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

//BSV Library imports
import GetPut::*;
import ClientServer::*;
import RegFile::*;
import Vector::*;
import List::*;
import FIFO::*;
import ModuleCollect::*;
import Connectable::*;

// A Model is the global top-level, with no wires in or out.

typedef Empty Model;

//------------ Controller Interface ------------//

// A Controller is the part of the Model which controls the
// simulation. All communication with the "outside world" passes
// through it. It switches between normal simulation and special
// modes like Debug/Events/Stats.

// Eventually this could be mostly implemented as software on the
// FPGA PowerPC.

// In the future I expect this to become more interesting with
// Bus interfaces.

typedef Empty Controller;

//------------ TModule Interface ------------//

// A TModule is a Timing Partition Module. 
// This is a module which is controlled by a controller. It has the
// capability to execute a "tick" (a simulated clock cycle). 
// This may take a number of actual host cycles to do.

// Note that this protocol may have to be changed in the future
// In order to reduce sequentialization. The RAMP project could
// play a role here.

interface TModule#(type com_T, type resp_T);
  
  method Action               exec(com_T c);
  method ActionValue#(resp_T) response();

endinterface

//------------------------- Connections --------------------------//
//                                                                //
// Connections are the plumbing of HASim. They represent basic	  //
// point-to-point communication. The advantage over traditional   //
// Bluespec Connectables is that they are easier to use, are	  //
// connected automatically, and can easily be extended to include //
// model latency (ASim Ports).  				  //
// 								  //
// These might eventually be donated to the Bluespec library.	  //
// 								  //
//                                                                //
//----------------------------------------------------------------//


//The basic sending half of a connection.

interface Connection_Send#(type msg_T);
  
  method Action send(msg_T data);
  
endinterface


//The basic receiving connection.

interface Connection_Receive#(type msg_T);
  
  //For the user
  method ActionValue#(msg_T) receive();

endinterface


// A client sends requests and receives responses
// (which may not come instantly)

interface Connection_Client#(type req_T, type resp_T);

  //For the user
  method Action               makeReq(req_T data);
  method ActionValue#(resp_T) getResp;
  
endinterface


// A server receives requests and gives back responses
// It can take any amount of time, and there is no assumption
// that the responses are FIFO.

interface Connection_Server#(type req_T, type resp_T);

  //For the user
  method ActionValue#(req_T) getReq();
  method Action              makeResp(resp_T data);
  
endinterface

`define CON_WIDTH 200

`define CON_Addr 10

typedef Bit#(`CON_WIDTH) CON_Data;

//Change to BypassFIFO here.
function m#(FIFO#(a)) mkCON_FIFO() provisos (Bits#(a, a_SZ), IsModule#(m, m2)) = mkFIFO();

interface WithConnections;

  interface Vector#(`CON_Addr, CON_In)  incoming;
  interface Vector#(`CON_Addr, CON_Out) outgoing;

endinterface

interface CON_In;

  method Action get_TRY(CON_Data x);
  method Bool   get_SUCCESS();

endinterface

interface CON_Out;

  method CON_Data try();
  method Action success();

endinterface

instance Connectable#(CON_Out, CON_In);

  function m#(Empty) mkConnection(CON_Out cout, CON_In cin)
    provisos (IsModule#(m, c));
  
    return connectInToOut(cout, cin);
    
  endfunction

endinstance

instance Connectable#(CON_In, CON_Out);

  function m#(Empty) mkConnection(CON_In cin, CON_Out cout)
    provisos (IsModule#(m, c));
  
    return connectInToOut(cout, cin);
    
  endfunction

endinstance
module connectInToOut#(CON_Out cout, CON_In cin) ();

  rule trySend (True);
  
    let x = cout.try();
    cin.get_TRY(x);
  
  endrule

  rule success (cin.get_SUCCESS());
  
    cout.success();
    
  endrule

endmodule

typedef union tagged
{
  Tuple2#(String, CON_Out) LSend;
  Tuple2#(String, CON_In) LRec;
}
  ConnectionData;

typedef ModuleCollect#(ConnectionData) Connected_Module;

typedef Connected_Module HASim_Module;


typeclass Transmittable#(type any_T);

  function CON_Data marshall(any_T data);
  
  function any_T unmarshall(CON_Data data);
  
endtypeclass

instance Transmittable#(any_T)
      provisos
              (Bits#(any_T, any_SZ),
	       Add#(any_SZ, k_TMP, `CON_WIDTH));

  function CON_Data marshall(any_T data);
    return zeroExtend(pack(data));
  endfunction
  
  function any_T unmarshall(CON_Data data);
    return unpack(truncate(data));
  endfunction
  
endinstance


module [Connected_Module] mkConnection_Send#(String portname)
    //interface:
                (Connection_Send#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Transmittable#(msg_T));

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.
  
  FIFO#(msg_T) q <- mkCON_FIFO();
  
  //Bind the interface to a name for convenience
  let outg = (interface CON_Out;
  
	       method CON_Data try() = marshall(q.first());
	       
	       method Action success = q.deq();

	     endinterface);

  //Add our interface to the ModuleCollect collection
  addToCollection(LSend tuple2(portname, outg));

  method Action send(msg_T data);
    q.enq(data);
  endmethod

endmodule

module [Connected_Module] mkConnection_Receive#(String portname)
    //interface:
                (Connection_Receive#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Transmittable#(msg_T));

  VRWire#(Bool)  en_w    <- vMkRWire();
  VRWire#(msg_T) data_w  <- vMkRWire();
  
  //Bind the interface to a name for convenience
  let inc = (interface CON_In;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w.whas() && en_w.wget();
	       endmethod

	     endinterface);

  //Add our interface to the ModuleCollect collection
  addToCollection(LRec tuple2(portname, inc));
  
  method ActionValue#(msg_T) receive() if (data_w.whas());
    en_w.wset(data_w.whas());
    return data_w.wget();
  endmethod

endmodule

module [Connected_Module] mkConnection_Client#(String portname)
    //interface:
                (Connection_Client#(req_T, resp_T))
    provisos
            (Bits#(req_T,  req_SZ),
	     Bits#(resp_T, resp_SZ),
	     Transmittable#(req_T),
	     Transmittable#(resp_T));

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.
  
  FIFO#(req_T)    q      <- mkCON_FIFO();
  VRWire#(Bool)   en_w   <- vMkRWire();
  VRWire#(resp_T) data_w <- vMkRWire();
  	      
  //Bind the interfaces to names for convenience
  let inc = (interface CON_In;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w.whas() && en_w.wget();
	       endmethod

	     endinterface);

  let outg = (interface CON_Out;
  
	        method CON_Data try() = marshall(q.first());
	       
	        method Action success() = q.deq();

	     endinterface);
  
  let sendname = strConcat(portname, "_req");
  let recname = strConcat(portname, "_resp");

  //Add our interfaces to the ModuleCollect collection
  addToCollection(LSend tuple2(sendname, outg));
  addToCollection(LRec tuple2(recname, inc));

  method Action makeReq(req_T data);
    q.enq(data);
  endmethod
  
  method ActionValue#(resp_T) getResp() if (data_w.whas());
    en_w.wset(data_w.whas());
    return data_w.wget();
  endmethod

endmodule

module [Connected_Module] mkConnection_Server#(String portname)
    //interface:
                (Connection_Server#(req_T, resp_T))
    provisos
            (Bits#(req_T,  req_SZ),
	     Bits#(resp_T, resp_SZ),
	     Transmittable#(req_T),
	     Transmittable#(resp_T));

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.
  
  FIFO#(resp_T) q <- mkCON_FIFO();
  VRWire#(Bool)  en_w    <- vMkRWire();
  VRWire#(req_T) data_w  <- vMkRWire();
  
  //Bind the interfaces to names for convenience
  let inc = (interface CON_In;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w.whas() && en_w.wget();
	       endmethod

	     endinterface);
	     
  let outg = (interface CON_Out;
  
	        method CON_Data try() = marshall(q.first());
	       
	        method Action success() = q.deq();

	     endinterface);
  let sendname = strConcat(portname, "_resp");
  let recname = strConcat(portname, "_req");
  
  //Add our interfaces to the ModuleCollect collection
  addToCollection(LSend tuple2(sendname, outg));
  addToCollection(LRec tuple2(recname, inc));

  method Action makeResp(resp_T data);
    q.enq(data);
  endmethod
  
  method ActionValue#(req_T) getReq() if (data_w.whas());
    en_w.wset(data_w.whas());
    return data_w.wget();
  endmethod

endmodule

//************** Hookup functions **************//

//lookup :: Eq a => a -> [(a, b)] -> Maybe b

function Maybe#(b) lookup (a data, List#(Tuple2#(a, b)) l)
  provisos (Eq#(a));
  
  case (l) matches
    tagged Nil:
      return Invalid;
    default:
    begin
      match {.d, .v} = List::head(l);
      return (d == data) ? (Valid v) : lookup(data, List::tail(l));
    end
  endcase

endfunction

//removeItem :: Eq a => a -> [(a, b)] -> [(a,b)]  

function List#(Tuple2#(a, b)) removeItem(a s, List#(Tuple2#(a, b)) l)
  provisos (Eq#(a));

  case (l) matches
    tagged Nil: return Nil;
    default:
    begin
      match {.nm, .v} = List::head(l);
      if (nm == s) 
	return List::tail(l);
      else
	return List::cons(tuple2(nm, v), removeItem(s, List::tail(l)));
    end
  endcase
endfunction

//groupByName :: Eq a => [(a, b)] -> [(a, c)] -> ([(a, b)], [(a, c)], [(a, b, c)])

function Tuple3#(List#(Tuple2#(a, b)),
	         List#(Tuple2#(a, c)),
		 List#(Tuple3#(a, b, c))) groupByName(List#(Tuple2#(a, b)) xs,
		                                      List#(Tuple2#(a, c)) ys)
     provisos
             (Eq#(a));

  if (isNull(xs) || isNull(ys))
    return tuple3(xs, ys, List::nil); //Return dangling items
  else
    begin
      match {.try, .x} = List::head(xs);

      //XXX Add duplicate name check

      case (lookup(try, ys)) matches
	tagged Valid .y:
	begin
	  match {.das, .dbs, .gs} = groupByName(List::tail(xs), removeItem(try, ys));
	  return tuple3(das, dbs, List::cons(tuple3(try, x, y), gs));
	end
	default:
	begin
	  match {.das, .dbs, .gs} = groupByName(List::tail(xs), ys);
	  return tuple3(List::cons(tuple2(try, x), das), dbs, gs);
	end
      endcase
    end
    
endfunction

//splitConnections :: [ConnectionData] -> ([(String, CON_Out)], [(String, CON_In)])

function Tuple2#(List#(Tuple2#(String, CON_Out)), 
                 List#(Tuple2#(String, CON_In))) splitConnections(List#(ConnectionData) l);

  case (l) matches
    tagged Nil: return tuple2(Nil, Nil);
    default:
    begin
      match {.sends, .recs} = splitConnections(List::tail(l));
      case (List::head(l)) matches
	tagged LSend .t:
	  return tuple2(List::cons(t,sends), recs);
	tagged LRec .t:
	  return tuple2(sends, List::cons(t, recs));
      endcase
    end
  endcase

endfunction

typedef Tuple2#(String, Integer) ConMap;

//               child           sends          recs
typedef Tuple3#(WithConnections, List#(ConMap), List#(ConMap)) DanglingInfo;

 
module [Module] connectDangling#(List#(ConnectionData) ld, 
                                 inter_T i,
                                 List#(DanglingInfo) children)       (WithConnections);
    
  match {.sends, .recs} = splitConnections(ld);
  
  
  //match {.dsends, .drecs, .cncts} = groupByName(sends, recs);
  let tup = groupByName(sends, recs);
  List#(Tuple2#(String, CON_Out)) dsends = tpl_1(tup);
  List#(Tuple2#(String, CON_In))  drecs = tpl_2(tup);
  match {.*, .*, .cncts} = tup;
  
  let numout = length(dsends);
  let numin  = length(drecs);
  
  let nCncts = length(cncts);
  
  //Internal Connections
  for (Integer x = 0; x < nCncts; x = x + 1)
  begin
    match {.nm, .cin, .cout} = cncts[x];
    
    messageM(strConcat("Connecting: ", nm));
    mkConnection(cin, cout);
  
  end
  
  //Children's connections
  let nChildren = length(children);
  Vector#(`CON_Addr, CON_Out) outs = newVector();
  Vector#(`CON_Addr, CON_In) ins = newVector();
  Integer cur_in = 0;
  Integer cur_out = 0;
  
  
  for (Integer x = 0; x < nChildren; x = x + 1)
  begin
    match {.child, .osends, .orecs} = children[x];
    
    //Child's send connections
    let nOSends = length(osends);
    
    for (Integer y = 0; y < nOSends; y = y + 1)
    begin
    
      match {.cnm, .caddr} = osends[y];
      
      //First check the current module for a rec
      
      case (lookup(cnm, drecs)) matches
        tagged Valid .cin:
	begin
          messageM(strConcat("Connecting Send to Child: ", cnm));
	  mkConnection(child.outgoing[caddr], cin);
	  drecs = removeItem(cnm, drecs);
	end
	tagged Invalid: //Then check the other children
        begin
	
	  Bool found = False;
	  for (Integer z = x; (z < nChildren) && (!found); z = z + 1)
	  begin
	    match {.c2, .ss, .rs} = children[z];
	    case (lookup(cnm, rs)) matches
	      tagged Valid .inaddr:
	      begin
	        messageM(strConcat("Connecting Child to Child: ", cnm));
		mkConnection(child.outgoing[caddr], c2.incoming[inaddr]);
		found = True;
	      end
	    endcase
	  end
	  if (!found) //It's a pass-through
	  begin
	    outs[cur_out] = (interface CON_Out;
		               method CON_Data try() = child.outgoing[caddr].try();
			       method Action success() = child.outgoing[caddr].success();
			     endinterface);
            messageM(strConcat(strConcat(strConcat("Dangling Send [", integerToString(cur_out)), "]: "), cnm));
	    cur_out = cur_out + 1;
	  end
	end
      endcase
    end
    
    //Child's Rec connections
    let nORecs = length(orecs);
    
    for (Integer y = 0; y < nORecs; y = y + 1)
    begin
    
      match {.cnm, .caddr} = orecs[y];
      
      //First check the current module for a send
      
      case (lookup(cnm, dsends)) matches
        tagged Valid .cout:
	begin
          messageM(strConcat("Connecting Rec to Child: ", cnm));
	  mkConnection(cout, child.incoming[caddr]);
	  dsends = removeItem(cnm, dsends);
	end
	tagged Invalid: //Then check the other children
        begin
	
	  Bool found = False;
	  for (Integer z = x; (z < nChildren) && (!found); z = z + 1)
	  begin
	    match {.c2, .ss, .rs} = children[z];
	    case (lookup(cnm, ss)) matches
	      tagged Valid .outaddr:
	      begin
	        messageM(strConcat("Connecting Child to Child: ", cnm));
		mkConnection(c2.outgoing[outaddr], child.incoming[caddr]);
		found = True;
	      end
	    endcase
	  end
	  if (!found) //It's a pass-through
	  begin
	    ins[cur_in] = (interface CON_In;
		               method Action get_TRY(CON_Data x) = child.incoming[caddr].get_TRY(x);
			       method Bool get_SUCCESS() = child.incoming[caddr].get_SUCCESS();
			     endinterface);
            messageM(strConcat(strConcat(strConcat("Dangling Rec [", integerToString(cur_in)), "]: "), cnm));
	    cur_in = cur_in + 1;
	  end
	end
      endcase
    end
    
    
  end
  
  //Final Dangling sends
  for (Integer x = 0; x < length(dsends); x = x + 1)
  begin
    match {.cnm, .cout} = dsends[x];
    messageM(strConcat(strConcat(strConcat("Dangling Send [", integerToString(cur_out)), "]: "), cnm));
    outs[cur_out] = cout;
    cur_out = cur_out + 1;
  end
  
  for (Integer x = cur_out; x < valueOf(`CON_Addr); x = x + 1)
    outs[x] = ?;
  
  //Final Dangling recs
  for (Integer x = 0; x < length(drecs); x = x + 1)
  begin
    match {.cnm, .cin} = drecs[x];
    messageM(strConcat(strConcat(strConcat("Dangling Rec [", integerToString(cur_in)), "]: "), cnm));
    ins[cur_in] = cin;
    cur_in = cur_in + 1;
  end
  
  for (Integer x = cur_in; x < valueOf(`CON_Addr); x = x + 1)
    ins[x] = ?;
  
  interface outgoing = outs;
  interface incoming = ins;
  
endmodule

//connectTopLevel :: [ConnectionData] -> Module () 

module [Module] connectTopLevel#(List#(ConnectionData) ld,
                                 List#(DanglingInfo) children) ();
  /*
  match {.sends, .recs} = splitConnections(ld);
  
  match {.dsends, .drecs, .cncts} = groupByName(sends, recs);
  
  let nCncts = length(cncts);
  
  for (Integer x = 0; x < nCncts; x = x + 1)
  begin
    match {.nm, .cin, .cout} = cncts[x];
    
    messageM(strConcat("Connecting: ", nm));
    mkConnection(cin, cout);
  
  end
  
  let nDSends = length(dsends);
  
  for (Integer x = 0; x < nDSends; x = x + 1)
  begin
    match {.nm, .cout} = dsends[x];
    messageM(strConcat(strConcat(strConcat("Dangling Send [", integerToString(x)), "]: "), nm));
  end
  
  let nDRecs = length(drecs);
  
  for (Integer x = 0; x < nDRecs; x = x + 1)
  begin
    match {.nm, .cin} = drecs[x];
    messageM(strConcat(strConcat(strConcat("Dangling Rec [", integerToString(x)), "]: "), nm));
  end
  
  if ((nDSends != 0) || (nDRecs != 0))
    error("Dangling connections at top-level!");
  */
  
  return ?;
endmodule
 
module [Module] instantiateDangling#(Connected_Module#(inter_T) m,
                                     List#(DanglingInfo) children) (WithConnections);

  match {.m, .col} <- getCollection(m);
  
  let x <- connectDangling(col, m, children);
  return x;

endmodule

module [Module] synth#(Connected_Module#(inter_T) m) (inter_T);

  match {.m, .col} <- getCollection(m);
  
  return m;

endmodule

module [Module] instantiateTopLevel#(Connected_Module#(Empty) m,
                                     List#(DanglingInfo) children) (Empty);

  match {.m, .col} <- getCollection(m);
  
  connectTopLevel(col, children);

endmodule
