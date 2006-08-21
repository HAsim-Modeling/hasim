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

//Note: make these ASim parameters
typedef Bit#(8) TokIndex;
typedef Bit#(1) TokEpoch;
typedef Bit#(0) TokContext;

// A Token has support for associated data like epochs and contexts
typedef struct
{
  TokEpoch   epoch;
  TokContext ctxt;
}
  TokInfo deriving (Eq, Bits);

typedef struct
{
  TokIndex   index;
  TokInfo    info;
}
  Token deriving (Eq, Bits);

typedef struct 
{
  Bit#(1) b_up; 
  Bit#(1) b_down; 
  Bit#(1) b_left; 
  Bit#(1) b_right;
  Bit#(1) b_center;
}
  ButtonInfo deriving (Eq, Bits);
  
interface TopLevel;
  (* always_ready *)
  (* result = "LED" *)
  method Bit#(4) leds();
  (* always_ready, always_enabled *)
  (* prefix = "" *)
  method Action  switches((* port = "SWITCH" *) Bit#(4) sw);
  (* always_ready, always_enabled *)
  (* prefix = "" *)
  method Action  button_left((* port = "BUTTON_LEFT" *) Bit#(1) bl);
  (* always_ready, always_enabled *)
  (* prefix = "" *)
  method Action  button_right((* port = "BUTTON_RIGHT" *) Bit#(1) br);
  (* always_ready, always_enabled *)
  (* prefix = "" *)
  method Action  button_up((* port = "BUTTON_UP" *) Bit#(1) bu);
  (* always_ready, always_enabled *)
  (* prefix = "" *)
  method Action  button_down((* port = "BUTTON_DOWN" *) Bit#(1) bd);
  (* always_ready, always_enabled *)
  (* prefix = "" *)
  method Action  button_center((* port = "BUTTON_CENTER" *) Bit#(1) bc);

endinterface

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

//------------------ Connection Information ----------------------//
//                                                                //
// We gather information about each module's connections using the//
// ModuleCollect library. The connections are then hooked together//
// using this info with the algorithms in Connections.bsv         //
//                                                                //
//----------------------------------------------------------------//


typedef 200 CON_Width;

typedef 32 CON_Addr;

typedef Bit#(32) TimeStamp;

typedef Bit#(CON_Width) CON_Data;
typedef 2 CON_NumChains;

//Change to BypassFIFO here.
function m#(FIFO#(a)) mkCON_FIFO() provisos (Bits#(a, a_SZ), IsModule#(m, m2)) = mkFIFO();

//The interface of a module with Connections
interface WithConnections;

  interface Vector#(CON_Addr, CON_In)  incoming;
  interface Vector#(CON_Addr, CON_Out) outgoing;
  interface Vector#(CON_NumChains, CON_Chain) chains;

endinterface

//An incoming connection
interface CON_In;

  method Action get_TRY(CON_Data x);
  method Bool   get_SUCCESS();

endinterface

//An outgoing connection
interface CON_Out;

  method CON_Data try();
  method Action success();

endinterface

//A scanchain
typedef FIFO#(CON_Data) CON_Chain;

//Connections can be hooked up using the standard mkConnection function

instance Connectable#(CON_Out, CON_In);

  function m#(Empty) mkConnection(CON_Out cout, CON_In cin)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout, cin);
    
  endfunction

endinstance

instance Connectable#(CON_In, CON_Out);

  function m#(Empty) mkConnection(CON_In cin, CON_Out cout)
    provisos (IsModule#(m, c));
  
    return connectOutToIn(cout, cin);
    
  endfunction

endinstance

module connectOutToIn#(CON_Out cout, CON_In cin) ();

  rule trySend (True);
    //Try to move the data
    let x = cout.try();
    cin.get_TRY(x);
  
  endrule

  rule success (cin.get_SUCCESS());
    //We succeeded in moving the data
    cout.success();
    
  endrule

endmodule

//Chains can also be hooked up with mkConnection

instance Connectable#(CON_Chain, CON_Chain);

  function m#(Empty) mkConnection(CON_Chain cout, CON_Chain cin)
    provisos (IsModule#(m, c));
  
    return connectChains(cout, cin);
    
  endfunction

endinstance

module connectChains#(CON_Chain cout, CON_Chain cin) ();

  rule scanmove (True);
    //Move the data
    cin.enq(cout.first());
    cout.deq();
  
  endrule

endmodule

interface EventRecorder;
  method Action recordEvent(Maybe#(Bit#(32)) mdata);
endinterface

typedef union tagged
{
  Tuple2#(String, CON_Out) LSend;
  Tuple2#(String, CON_In)  LRec;
  Tuple2#(Integer, CON_Chain) LChain;
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
	       Add#(any_SZ, k_TMP, CON_Width));

  function CON_Data marshall(any_T data);
    return zeroExtend(pack(data));
  endfunction
  
  function any_T unmarshall(CON_Data data);
    return unpack(truncate(data));
  endfunction
  
endinstance

//Connection Implementations


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

typedef union tagged
{
  Bit#(32) EVT_Boundary;
  void EVT_NoEvent;
  Bit#(32) EVT_Event;
}
  EventData deriving (Eq, Bits);

module [Module] mkPassThrough#(Integer chainNum)
    //interface:
                (CON_Chain);

  FIFO#(EventData) passQ <- mkFIFO();

  method CON_Data first() = marshall(passQ.first());
  method Action deq() = passQ.deq();
  method Action clear() = passQ.clear();
  method Action enq(CON_Data x) = passQ.enq(unmarshall(x));

endmodule
