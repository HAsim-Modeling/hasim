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
typedef Bit#(7) CON_ChainAddr;
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

interface CON_Chain_Out;
  method Maybe#(Tuple2#(CON_ChainAddr, CON_Data)) dout;
endinterface

interface CON_Chain_In;
  method Action din(Maybe#(Tuple2#(CON_ChainAddr, CON_Data)) t);
endinterface

//A scanchain
interface CON_Chain;
  
  interface CON_Chain_In chain_in;
  interface CON_Chain_Out chain_out;

endinterface

interface CON_Chain_Local;

  method Maybe#(CON_Data) dout;
  method Action din(Maybe#(CON_Data) t);

endinterface

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
  
    return connectChains(cout.chain_out, cin.chain_in);
    
  endfunction

endinstance

instance Connectable#(CON_Chain_Out, CON_Chain_In);

  function m#(Empty) mkConnection(CON_Chain_Out cout, CON_Chain_In cin)
    provisos (IsModule#(m, c));
  
    return connectChains(cout, cin);
    
  endfunction

endinstance

instance Connectable#(CON_Chain_In, CON_Chain_Out);

  function m#(Empty) mkConnection(CON_Chain_In cin, CON_Chain_Out cout)
    provisos (IsModule#(m, c));
  
    return connectChains(cout, cin);
    
  endfunction

endinstance

module connectChains#(CON_Chain_Out cout, CON_Chain_In cin) ();

  rule scanmove (True);
    //Move the data
    let x = cout.dout();
    cin.din(x);
  
  endrule

endmodule

module localToChain#(CON_Chain_Local chn, CON_ChainAddr my_addr) (CON_Chain);

  Reg#(CON_ChainAddr) addr <- mkRegU();
 
  interface CON_Chain_Out chain_out;
 
    method Maybe#(Tuple2#(CON_ChainAddr, CON_Data)) dout;

      case (chn.dout()) matches
	tagged Invalid:
          return Invalid;
	tagged Valid .d:
          return Valid tuple2(my_addr, d);
      endcase

    endmethod

  endinterface
  
  interface CON_Chain_In chain_in;
  
    method Action din(Maybe#(Tuple2#(CON_ChainAddr, CON_Data)) mt);

      case (mt) matches
	tagged Invalid:
          chn.din(Invalid);
	tagged Valid .t:
	begin 
	  match {.a, .d} = t;

	  addr <= a;
	  chn.din(Valid d);
	end
      endcase

    endmethod

  endinterface

endmodule

interface EventRecorder#(type t);
  method Action recordEvent(TimeStamp c, t data);
endinterface

interface DebugRecorder#(type t);
  method Action debug(TimeStamp c, t data);
endinterface

typedef union tagged
{
  Tuple2#(String, CON_Out) LSend;
  Tuple2#(String, CON_In)  LRec;
  Tuple2#(Integer, CON_Chain_Local) LChain;
  Tuple2#(Integer, CON_Chain_In) LSink;
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

//A pass-through link in the chain
module [Module] mkPassThrough
    //interface:
                (CON_Chain);

  Reg#(Maybe#(Tuple2#(CON_ChainAddr, CON_Data))) r <- mkReg(Invalid);

  interface CON_Chain_Out chain_out;
    method Maybe#(Tuple2#(CON_ChainAddr, CON_Data)) dout = r;
  endinterface

  interface CON_Chain_In chain_in;
    method Action din(Maybe#(Tuple2#(CON_ChainAddr, CON_Data)) t);
      r <= t;
    endmethod
  endinterface

endmodule

module [Connected_Module] mkEventRecorder#(String eventname)
    //interface:
                (EventRecorder#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Transmittable#(Tuple2#(TimeStamp, msg_T)));

  Reg#(Maybe#(CON_Data)) r <- mkReg(Invalid);
  VRWire#(Bool)  en_w    <- vMkRWire();
  
  //Bind the interface to a name for convenience
  let chn = (interface CON_Chain_Local;
  
	       method Maybe#(CON_Data) dout() = r;
	       
	       method Action din(Maybe#(CON_Data) d) if (!en_w.whas() && !en_w.wget());
	         r <= d;
	       endmethod

	     endinterface);

  //Add our interface to the ModuleCollect collection
  addToCollection(LChain tuple2(0, chn));

  method Action recordEvent(TimeStamp cc, msg_T data);
    r <= Valid marshall(tuple2(cc, data));
    en_w.wset(True);
  endmethod

endmodule

module [Connected_Module] mkSink_Software#(String chainname, Integer x,
                                           function String conv(CON_ChainAddr a, CON_Data d))
    //interface:
                ();

  let nm = strConcat("[", strConcat(chainname, "]: "));

  //Bind the interface to a name for convenience
  let snk = (interface CON_Chain_In;
  
	       method Action din(Maybe#(Tuple2#(CON_ChainAddr, CON_Data)) data);
	         case (data) matches
		   tagged Invalid:
		     noAction;
		   tagged Valid {.a, .d}:
		   begin
		     String str = conv(a, d);
		     $display(strConcat(nm, str));
		   end
		  endcase
	       endmethod

	     endinterface);

  //Add our interface to the ModuleCollect collection
  addToCollection(LSink tuple2(x, snk));

endmodule
