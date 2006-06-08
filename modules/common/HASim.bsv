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

interface TModule#(type tick_T, type command_T, type result_T);

  
  method Action tick(tick_T cc);
  method Bool   done();

  //Commands are an infrastructure for the future.
  //Might include things like entering debug mode.
  method Action   exec(command_T c);
  method result_T exec_response();

endinterface

//------------ Timing Partition Heirarchy ------------//

// Top-level TModule is a System.
// Currently it includes magic links to the IMem/DMem 
// so that the controller can load the program

typedef TModule#(tick_T, command_T, result_T) 
        System#(type tick_T, type command_T, type result_T);

// A System contains Boards

typedef TModule#(tick_T, command_T, result_T) 
        Board#(type tick_T, type command_T, type result_T);

// A Board contains Chips

typedef TModule#(tick_T, command_T, result_T) 
        Chip#(type tick_T, type command_T, type result_T);

// A Chip contains one or more CPU models

typedef TModule#(tick_T, command_T, result_T) 
        CPU#(type tick_T, type command_T, type result_T);

// A CPU contains a Timing Partition

typedef TModule#(tick_T, command_T, result_T) 
        TimingPartition#(type tick_T, type command_T, type result_T);

// Other things that could go here might include Caches, Memory Controllers, etc.



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

`define ConnectionWidth 200

interface WithConnections#(type in_W, type out_W, type orig_T);

  interface orig_T original;

  interface Vector#(in_W, Put#(Bit#(`ConnectionWidth))) incoming;
  interface Vector#(out_W, Get#(Bit#(`ConnectionWidth))) outgoing;

endinterface


typedef union tagged
{
  Tuple2#(String, Get#(Bit#(`ConnectionWidth))) LSend;
  Tuple2#(String, Put#(Bit#(`ConnectionWidth))) LRec;
}
  ConnectionData;

typedef ModuleCollect#(ConnectionData) Connected_Module;

typedef Connected_Module HASim_Module;


typeclass Transmittable#(type any_T);

  function Bit#(`ConnectionWidth) marshall(any_T data);
  
  function any_T unmarshall(Bit#(`ConnectionWidth) data);
  
endtypeclass

instance Transmittable#(any_T)
      provisos
              (Bits#(any_T, any_SZ),
	       Add#(any_SZ, k_TMP, `ConnectionWidth));

  function Bit#(`ConnectionWidth) marshall(any_T data);
    return zeroExtend(pack(data));
  endfunction
  
  function any_T unmarshall(Bit#(`ConnectionWidth) data);
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
  
  FIFO#(msg_T) q <- mkFIFO();
  
  //Bind the interface to a name for convenience.
  let outg = (interface Get;
		method ActionValue#(Bit#(`ConnectionWidth)) get();
		  q.deq();
		  return marshall(q.first());
		endmethod
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

  Wire#(Bit#(`ConnectionWidth)) w <- mkWire();
  let inc = (interface Put;
	       method Action put(Bit#(`ConnectionWidth) data);
	         w <= data;
	       endmethod
	     endinterface);

  //Bind the interface to a name for convenience
  addToCollection(LRec tuple2(portname, inc));
  
  method ActionValue#(msg_T) receive();
    noAction;
    return unmarshall(w);
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
  
  FIFO#(req_T) q <- mkFIFO();
  Wire#(Bit#(`ConnectionWidth)) w <- mkWire();
  
  //Bind the interfaces to names.
  let outg = (interface Get;
		method ActionValue#(Bit#(`ConnectionWidth)) get();
		  q.deq();
		  return marshall(q.first());
		endmethod
	      endinterface);
	      
  let inc = (interface Put;
	       method Action put(Bit#(`ConnectionWidth) data);
	         w <= data;
	       endmethod
	     endinterface);
  
  //Add our interface to the ModuleCollect collection
  addToCollection(LSend tuple2(portname, outg));

  method Action makeReq(req_T data);
    q.enq(data);
  endmethod
  
  method ActionValue#(resp_T) getResp;
    noAction;
    return unmarshall(w);
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
  
  FIFO#(resp_T) q <- mkFIFO();
  Wire#(Bit#(`ConnectionWidth)) w <- mkWire();
  
  //Bind the interface to names for convenience
  let outg = (interface Get;
		method ActionValue#(Bit#(`ConnectionWidth)) get();
		  q.deq();
		  return marshall(q.first());
		endmethod
	      endinterface);
	      
  let inc = (interface Put;
	       method Action put(Bit#(`ConnectionWidth) data);
	         w <= data;
	       endmethod
	     endinterface);
  
  //Add our interface to the ModuleCollect collection
  addToCollection(LSend tuple2(portname, outg));

  method Action makeResp(resp_T data);
    q.enq(data);
  endmethod
  
  method ActionValue#(req_T) getReq;
    noAction;
    return unmarshall(w);
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

//splitConnections :: [ConnectionData] -> ([(String, Get)], [(String, Put)])

function Tuple2#(List#(Tuple2#(String, Get#(Bit#(`ConnectionWidth)))), 
                 List#(Tuple2#(String, Put#(Bit#(`ConnectionWidth))))) splitConnections(List#(ConnectionData) l);

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
 
//connectDangling :: [ConnectionData] -> ifc -> Module (WithConnections in out ifc)
 
module [Module] connectDangling#(List#(ConnectionData) ld, inter_T i) (WithConnections#(in_W, out_W, inter_T));
    
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
  
  List#(Get#(Bit#(`ConnectionWidth))) outg = List::nil;
  for (Integer x = 0; x < nDSends; x = x + 1)
  begin
    match {.nm, .cout} = dsends[x];
    messageM(strConcat("Dangling Send: ", nm));
    outg = List::cons(cout, outg);
  end
  
  let nDRecs = length(drecs);
  
  List#(Put#(Bit#(`ConnectionWidth))) inc = List::nil;
  for (Integer x = 0; x < nDRecs; x = x + 1)
  begin
    match {.nm, .cin} = drecs[x];
    messageM(strConcat("Dangling Rec: ", nm));
    inc = List::cons(cin, inc);
  end
  
  interface original = i;
  interface outgoing = Vector::toVector(outg);
  interface incoming = Vector::toVector(inc);
  
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
    messageM(strConcat("Dangling Send: ", nm));
  end
  
  let nDRecs = length(drecs);
  
  for (Integer x = 0; x < nDRecs; x = x + 1)
  begin
    match {.nm, .cin} = drecs[x];
    messageM(strConcat("Dangling Rec: ", nm));
  end
  
  if ((nDSends != 0) || (nDRecs != 0))
    error("Dangling connections at top-level!");
  
endmodule
 
module [Module] instantiateDangling#(Connected_Module#(inter_T) m) (WithConnections#(in_W, out_W, inter_T));

  match {.m, .col} <- getCollection(m);
  
  //connectDangling(col, m);

endmodule

module [Module] synth#(Connected_Module#(inter_T) m) (inter_T);

  match {.m, .col} <- getCollection(m);
  
  return m;

endmodule

module [Module] instantiateTopLevel#(Connected_Module#(Empty) m) (Empty);

  match {.m, .col} <- getCollection(m);
  
  connectTopLevel(col);

endmodule
