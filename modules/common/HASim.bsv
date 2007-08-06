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
import Vector::*;
import FIFO::*;
import ModuleCollect::*;
import Connectable::*;

typedef Bit#(`HASIM_TOK_INDEX_BITS) TokIndex;

typedef Bit#(`HASIM_TIMEP_EPOCH_BITS) TIMEP_Epoch;
typedef Bit#(`HASIM_TIMEP_SCRATCHPAD_BITS) TIMEP_Scratchpad;

typedef Bit#(`HASIM_FUNCP_EPOCH_BITS) FUNCP_Epoch;
typedef Bit#(`HASIM_FUNCP_SCRATCHPAD_BITS)FUNCP_Scratchpad;

typedef struct
{
  TIMEP_Epoch   epoch;
  TIMEP_Scratchpad scratchpad;
}
  TIMEP_TokInfo 
    deriving (Eq, Bits);

typedef struct
{
  FUNCP_Epoch   epoch;
  FUNCP_Scratchpad scratchpad;
}
  FUNCP_TokInfo 
    deriving (Eq, Bits);

typedef struct
{
  TokIndex	    index;
  TIMEP_TokInfo     timep_info;
  FUNCP_TokInfo     funcp_info;
}
  Token 
    deriving (Eq, Bits);

//isOlder: predicated on the idea that only half the tokens are in flight at once.

function Bool isOlder(TokIndex t1, TokIndex t2);

  return (t1 - t2) > (t2 - t1);

endfunction

typedef struct 
{
  Bit#(1) b_up; 
  Bit#(1) b_down; 
  Bit#(1) b_left; 
  Bit#(1) b_right;
  Bit#(1) b_center;
}
  ButtonInfo deriving (Eq, Bits);
  
//------------------------- Connections --------------------------//
//                                                                //
// Connections are the plumbing of HAsim. They represent basic	  //
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
  
  method Action deq();
  method msg_T  receive();

endinterface


// A client sends requests and receives responses
// (which may not come instantly)

interface Connection_Client#(type req_T, type resp_T);

  method Action makeReq(req_T data);
  method resp_T getResp();
  method Action deq();
  
endinterface


// A server receives requests and gives back responses
// It can take any amount of time, and there is no assumption
// that the responses are FIFO.

interface Connection_Server#(type req_T, type resp_T);

  method req_T  getReq();
  method Action deq();
  method Action makeResp(resp_T data);
  
endinterface

// A chain is a link which has a previous to receive from and
// a next to send to.

interface Connection_Chain#(type msg_T);

  method ActionValue#(msg_T) receive_from_prev();
  method Action              send_to_next(msg_T data);
  
endinterface

//------------------ Connection Information ----------------------//
//                                                                //
// We gather information about each module's connections using the//
// ModuleCollect library. The connections are then hooked together//
// using this info with the algorithms in Connections.bsv         //
//                                                                //
//----------------------------------------------------------------//


typedef `HASIM_CON_CWIDTH CON_Width;

typedef `HASIM_CON_NUMBER CON_Addr;

typedef Bit#(32) TimeStamp;

typedef Bit#(CON_Width) CON_Data;
typedef `HASIM_CON_NUMCHAINS CON_NumChains;

//Chain 0: Events
//Chain 1: Stats
//Chain 2: Commands
//Chain 3: Responses

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
  addToCollection(tagged LSend tuple2(portname, outg));

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

  PulseWire      en_w    <- mkPulseWire();
  VRWire#(msg_T) data_w  <- vMkRWire();
  
  //Bind the interface to a name for convenience
  let inc = (interface CON_In;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w;
	       endmethod

	     endinterface);

  //Add our interface to the ModuleCollect collection
  addToCollection(tagged LRec tuple2(portname, inc));
  
  method msg_T receive() if (data_w.whas());
    return data_w.wget();
  endmethod

  method Action deq() if (data_w.whas());
    en_w.send();
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
  PulseWire       en_w   <- mkPulseWire();
  VRWire#(resp_T) data_w <- vMkRWire();
  	      
  //Bind the interfaces to names for convenience
  let inc = (interface CON_In;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w;
	       endmethod

	     endinterface);

  let outg = (interface CON_Out;
  
	        method CON_Data try() = marshall(q.first());
	       
	        method Action success() = q.deq();

	     endinterface);
  
  let sendname = strConcat(portname, "_req");
  let recname = strConcat(portname, "_resp");

  //Add our interfaces to the ModuleCollect collection
  addToCollection(tagged LSend tuple2(sendname, outg));
  addToCollection(tagged LRec tuple2(recname, inc));

  method Action makeReq(req_T data);
    q.enq(data);
  endmethod
  
  method resp_T getResp() if (data_w.whas());
    return data_w.wget();
  endmethod

  method Action deq() if (data_w.whas());
    en_w.send();
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
  PulseWire      en_w    <- mkPulseWire();
  VRWire#(req_T) data_w  <- vMkRWire();
  
  //Bind the interfaces to names for convenience
  let inc = (interface CON_In;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w;
	       endmethod

	     endinterface);
	     
  let outg = (interface CON_Out;
  
	        method CON_Data try() = marshall(q.first());
	       
	        method Action success() = q.deq();

	     endinterface);

  let sendname = strConcat(portname, "_resp");
  let recname = strConcat(portname, "_req");
  
  //Add our interfaces to the ModuleCollect collection
  addToCollection(tagged LSend tuple2(sendname, outg));
  addToCollection(tagged LRec tuple2(recname, inc));

  method Action makeResp(resp_T data);
    q.enq(data);
  endmethod
  
  method req_T getReq() if (data_w.whas());
    return data_w.wget();
  endmethod
  
  method Action deq() if (data_w.whas());
    en_w.send();
  endmethod

endmodule

//A Connection in a Chain

module [Connected_Module] mkConnection_Chain#(Integer chain_num)
    //interface:
		(Connection_Chain#(msg_T))
    provisos
	    (Bits#(msg_T, msg_SZ),
	   Transmittable#(msg_T));

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.

  VRWire#(msg_T) data_w  <- vMkRWire();
  FIFO#(msg_T) outQ <- mkCON_FIFO();

  let chn = (interface FIFO;

	      method CON_Data first() = marshall(outQ.first());
	      method Action deq() = outQ.deq();
	      method Action clear() = noAction;
	      method Action enq(CON_Data x) = data_w.wset(unmarshall(x));

	   endinterface);

  addToCollection(tagged LChain tuple2(chain_num, chn));

  method Action send_to_next(msg_T data);
    outQ.enq(data);
  endmethod

  method ActionValue#(msg_T) receive_from_prev() if (data_w.whas());

    return data_w.wget();

  endmethod

endmodule
