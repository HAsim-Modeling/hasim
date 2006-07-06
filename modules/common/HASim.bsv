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

typedef Bit#(8) CON_Addr;
typedef Bit#(`CON_WIDTH) CON_Data;

//Change to BypassFIFO here.
function m#(FIFO#(a)) mkCON_FIFO() provisos (Bits#(a, a_SZ), IsModule#(m, m2)) = mkFIFO();

interface WithConnections#(type orig_T);

  interface orig_T original;

  interface AntiGet#(Tuple2#(CON_Addr, CON_Data)) incoming;
  interface Get#(Tuple2#(CON_Addr, CON_Data)) outgoing;

endinterface

interface AntiGet#(type a);

  method Action get_TRY(a x);
  method Bool   get_SUCCESS();

endinterface

instance Connectable#(FIFO#(a), AntiGet#(a));

  function m#(Empty) mkConnection(FIFO#(a) q, AntiGet#(a) p)
    provisos (IsModule#(m, c));
  
    return connectFIFOtoAntiGet(q, p);
    
  endfunction

endinstance

module connectFIFOtoAntiGet#(FIFO#(a) q, AntiGet#(a) g) ();

  rule trySend (True);
  
    let x = q.first();
    g.get_TRY(x);
  
  endrule

  rule success (g.get_SUCCESS());
  
    q.deq();
    
  endrule

endmodule

typedef union tagged
{
  Tuple2#(String,    FIFO#(CON_Data)) LSend;
  Tuple2#(String, AntiGet#(CON_Data)) LRec;
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
  
  FIFO#(CON_Data) q <- mkCON_FIFO();
    
  //Add our interface to the ModuleCollect collection
  addToCollection(LSend tuple2(portname, q));

  method Action send(msg_T data);
    q.enq(marshall(data));
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
  let inc = (interface AntiGet;
  
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
  
  FIFO#(CON_Data) q <- mkCON_FIFO();
  VRWire#(Bool)  en_w    <- vMkRWire();
  VRWire#(resp_T) data_w  <- vMkRWire();
  	      
  //Bind the interface to a name for convenience
  let inc = (interface AntiGet;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w.whas() && en_w.wget();
	       endmethod

	     endinterface);
  
  let sendname = strConcat(portname, "_req");
  let recname = strConcat(portname, "_resp");

  //Add our interfaces to the ModuleCollect collection
  addToCollection(LSend tuple2(sendname, q));
  addToCollection(LRec tuple2(recname, inc));

  method Action makeReq(req_T data);
    q.enq(marshall(data));
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
  
  FIFO#(CON_Data) q <- mkCON_FIFO();
  VRWire#(Bool)  en_w    <- vMkRWire();
  VRWire#(req_T) data_w  <- vMkRWire();
  
  //Bind the interface to names for convenience
  let inc = (interface AntiGet;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w.whas() && en_w.wget();
	       endmethod

	     endinterface);
	     
  let sendname = strConcat(portname, "_resp");
  let recname = strConcat(portname, "_req");
  
  //Add our interfaces to the ModuleCollect collection
  addToCollection(LSend tuple2(sendname, q));
  addToCollection(LRec tuple2(recname, inc));

  method Action makeResp(resp_T data);
    q.enq(marshall(data));
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

//splitConnections :: [ConnectionData] -> ([(String, Get)], [(String, AntiGet)])

function Tuple2#(List#(Tuple2#(String, FIFO#(CON_Data))), 
                 List#(Tuple2#(String, AntiGet#(CON_Data)))) splitConnections(List#(ConnectionData) l);

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

//danglingIns :: [AntiGet a] -> Module (AntiGet (addr, a))

module danglingIns#(List#(AntiGet#(CON_Data)) gs) (AntiGet#(Tuple2#(CON_Addr, CON_Data)));

  VRWire#(Bool) en_w <- vMkRWire();
  
  method Action get_TRY(Tuple2#(CON_Addr, CON_Data) tup);
  
    match {.a, .d} = tup;
    
    List::select(gs, a).get_TRY(d);
    en_w.wset(List::select(gs, a).get_SUCCESS());
  
  endmethod
  
  method Bool get_SUCCESS() = en_w.whas() && en_w.wget();
  
endmodule

//danglingOuts :: [FIFO a] -> Module (Get (addr, a))

module danglingOuts#(List#(FIFO#(CON_Data)) fs) (Get#(Tuple2#(CON_Addr, CON_Data)));

  Integer n = length(fs);
  FIFO#(Tuple2#(CON_Addr, CON_Data)) q <- mkCON_FIFO();
  
  for (Integer k = 0; k < n; k = k + 1)
  begin
  
    rule arbitrate (True);
  
      q.enq(tuple2(fromInteger(k), List::select(fs, k).first()));
      List::select(fs, k).deq();
  
    endrule
  
  end

  method ActionValue#(Tuple2#(CON_Addr, CON_Data)) get();
    q.deq();
    return q.first();
  endmethod
  
endmodule


//connectDangling :: [ConnectionData] -> ifc -> Module (WithConnections in out ifc)
 
module [Module] connectDangling#(List#(ConnectionData) ld, inter_T i) (WithConnections#(inter_T));
    
  match {.sends, .recs} = splitConnections(ld);
  
  
  match {.dsends, .drecs, .cncts} = groupByName(sends, recs);
    
  let numout = length(dsends);
  let numin  = length(drecs);
  
  let nCncts = length(cncts);
  
  for (Integer x = 0; x < nCncts; x = x + 1)
  begin
    match {.nm, .cin, .cout} = cncts[x];
    
    messageM(strConcat("Connecting: ", nm));
    mkConnection(cin, cout);
  
  end
  
  let nDSends = length(dsends);
  
  List#(FIFO#(CON_Data)) outs = List::nil;
  for (Integer x = 0; x < nDSends; x = x + 1)
  begin
    match {.nm, .cout} = dsends[x]; 
    messageM(strConcat(strConcat(strConcat("Dangling Send [", integerToString(x)), "]: "), nm));
    outs = List::cons(cout, outs);
  end
  
  let nDRecs = length(drecs);
  
  List#(AntiGet#(CON_Data)) ins = List::nil;
  for (Integer x = 0; x < nDRecs; x = x + 1)
  begin
    match {.nm, .cin} = drecs[x]; 
    messageM(strConcat(strConcat(strConcat("Dangling Rec [", integerToString(x)), "]: "), nm));

    ins = List::cons(cin, ins);
  end
  
  let outg <- danglingOuts(outs);
  let inc <- danglingIns(ins);
  
  interface original = i;
  interface outgoing = outg;
  interface incoming = inc;
  
endmodule

//connectTopLevel :: [ConnectionData] -> Module () 

module [Module] connectTopLevel#(List#(ConnectionData) ld) ();
  
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
  
endmodule
 
module [Module] instantiateDangling#(Connected_Module#(inter_T) m) (WithConnections#(inter_T));

  match {.m, .col} <- getCollection(m);
  
  let x <- connectDangling(col, m);
  return x;

endmodule

module [Module] synth#(Connected_Module#(inter_T) m) (inter_T);

  match {.m, .col} <- getCollection(m);
  
  return m;

endmodule

module [Module] instantiateTopLevel#(Connected_Module#(Empty) m) (Empty);

  match {.m, .col} <- getCollection(m);
  
  connectTopLevel(col);

endmodule
