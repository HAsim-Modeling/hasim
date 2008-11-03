import FIFO::*;

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
  
  method Bool notEmpty(); 
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

//Connection Implementations

//Change Connection FIFO to BypassFIFO if desired:
function m#(FIFO#(a)) mkCON_FIFO() provisos (Bits#(a, a_SZ), IsModule#(m, m2)) = mkFIFO();
//function m#(FIFO#(a)) mkCON_FIFO() provisos (Bits#(a, a_SZ), IsModule#(m, m2)) = mkBypassFIFO();


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

  //Figure out my type for typechecking
  msg_T msg = ?;
  String mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let info = CSend_Info {cname: portname, ctype: mytype, conn: outg};
  addToCollection(tagged LSend info);

  method Action send(msg_T data);
    q.enq(data);
  endmethod

endmodule
/*
module [Connected_Module] mkConnection_Send_Bypassed#(String portname)
    //interface:
                (Connection_Send#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Transmittable#(msg_T));

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.
  
  FIFO#(msg_T) q <- mkBypassFIFO();
  
  //Bind the interface to a name for convenience
  let outg = (interface CON_Out;
  
	       method CON_Data try() = marshall(q.first());
	       
	       method Action success = q.deq();

	     endinterface);

  //Figure out my type for typechecking
  msg_T msg = ?;
  String mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let info = CSend_Info {cname: portname, ctype: mytype, conn: outg};
  addToCollection(tagged LSend info);

  method Action send(msg_T data);
    q.enq(data);
  endmethod

endmodule
*/

module [Connected_Module] mkConnection_Receive#(String portname)
    //interface:
                (Connection_Receive#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Transmittable#(msg_T));

  PulseWire      en_w    <- mkPulseWire();
  RWire#(msg_T)  data_w  <- mkRWire();
  
  //Bind the interface to a name for convenience
  let inc = (interface CON_In;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w;
	       endmethod

	     endinterface);

  //Figure out my type for typechecking
  msg_T msg = ?;
  String mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let info = CRecv_Info {cname: portname, ctype: mytype, conn: inc};
  addToCollection(tagged LRecv info);
  
  method msg_T receive() if (data_w.wget() matches tagged Valid .val);
    return val;
  endmethod

  method Bool notEmpty();
    return isValid(data_w.wget());
  endmethod

  method Action deq() if (data_w.wget() matches tagged Valid .val);
    en_w.send();
  endmethod

endmodule

// ========================================================================
//
// mkConnection_Client & mkConnection_Server --
//     A convenience which bundles up sending and receiving.
//
// ========================================================================

//
// First define functions for generating connection names for clients wishing
// to use separate mkConnection_Send and Receive to talk to a mkConnection_Server.
//
    
function String genConnectionClientSendName(String portname);
    return strConcat(portname, "_req");
endfunction

function String genConnectionClientReceiveName(String portname);
    return strConcat(portname, "_resp");
endfunction

module [Connected_Module] mkConnection_Client#(String portname)
    //interface:
                (Connection_Client#(req_T, resp_T))
    provisos
            (Bits#(req_T,  req_SZ),
	     Bits#(resp_T, resp_SZ),
	     Transmittable#(req_T),
	     Transmittable#(resp_T));

  let sendname = genConnectionClientSendName(portname);
  let recvname = genConnectionClientReceiveName(portname);
  
  Connection_Send#(req_T) reqconn <- mkConnection_Send(sendname);
  Connection_Receive#(resp_T) respconn <- mkConnection_Receive(recvname);

  method Action makeReq(req_T data);
    reqconn.send(data);
  endmethod
  
  method resp_T getResp();
    return respconn.receive();
  endmethod

  method Action deq();
    respconn.deq();
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

  let sendname = genConnectionClientReceiveName(portname);
  let recvname = genConnectionClientSendName(portname);
  
  Connection_Receive#(req_T) reqconn <- mkConnection_Receive(recvname);
  Connection_Send#(resp_T) respconn <- mkConnection_Send(sendname);

  method Action makeResp(resp_T data);
    respconn.send(data);
  endmethod
  
  method req_T getReq();
    return reqconn.receive();
  endmethod
  
  method Action deq();
    reqconn.deq();
  endmethod

endmodule


// ========================================================================
//
// mkConnection_Chain --
//     A Connection in a Chain
//
// ========================================================================

module [Connected_Module] mkConnection_Chain#(Integer chain_num)
    //interface:
		(Connection_Chain#(msg_T))
    provisos
	    (Bits#(msg_T, msg_SZ),
	   Transmittable#(msg_T));

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.

  RWire#(msg_T)  data_w  <- mkRWire();
  PulseWire      en_w    <- mkPulseWire();
  FIFO#(msg_T)   q       <- mkCON_FIFO();

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
	       
	       method Action success = q.deq();

	     endinterface);

  let chn = (interface CON_Chain;
               
	       interface incoming = inc;
	       interface outgoing = outg;
	     endinterface);

  //Figure out my type for typechecking
  msg_T msg = ?;
  String mytype = printType(typeOf(msg));

  //Add the chain to the ModuleCollect collection
  let info = CChain_Info {cnum: chain_num, ctype: mytype, conn: chn};
  addToCollection(tagged LChain info);


  method Action send_to_next(msg_T data);
    q.enq(data);
  endmethod

  method ActionValue#(msg_T) receive_from_prev() if (data_w.wget() matches tagged Valid .val);

    en_w.send();
    return val;

  endmethod

endmodule

//Helper functions


typeclass Transmittable#(type any_T);

  function CON_Data marshall(any_T data);
  
  function any_T unmarshall(CON_Data data);
  
endtypeclass

instance Transmittable#(any_T)
      provisos
              (Bits#(any_T, any_SZ),
               Bits#(CON_Data, con_SZ),
	       Add#(any_SZ, k_TMP, con_SZ));

  function CON_Data marshall(any_T data);
    return zeroExtend(pack(data));
  endfunction
  
  function any_T unmarshall(CON_Data data);
    return unpack(truncate(data));
  endfunction
  
endinstance
