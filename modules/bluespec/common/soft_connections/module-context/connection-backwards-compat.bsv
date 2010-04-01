
// Backwards compatability

// This file contains MOST of the code needed for backwards compatability.
// Some code, particularly relating to connection chains, lives in other 
// files because other things need to invoke it.

// This file mostly deals with backwards compatability functions and 
// interfaces seen by the user.


// Legacy typdefs
typedef ConnectedModule Connected_Module;
typedef Connected_Module CONNECTED_MODULE;

// Legacy naming conventions

module [ConnectedModule] mkConnection_Send#(String name) (Connection_Send#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- mkPhysicalConnectionSend(name, Invalid, False, False);
   return m;

endmodule

module [ConnectedModule] mkConnection_Receive#(String name) (Connection_Receive#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SIZE),
         Transmittable#(t_MSG));

   let m <- mkPhysicalConnectionRecv(name, Invalid, False, False);
   return m;

endmodule


typedef CONNECTION_SEND#(t) Connection_Send#(type t);
typedef CONNECTION_RECV#(t) Connection_Receive#(type t);

interface Connection_Server#(type req_T, type resp_T);

  method Bool   reqNotEmpty(); 
  method req_T  getReq();
  method Action deq();
  method Action makeResp(resp_T data);
  method Bool   respNotFull();
  
endinterface


module [ConnectedModule] mkConnection_Server#(String server_name)
    //interface:
                (Connection_Server#(t_REQ, t_RSP))
    provisos
            (Bits#(t_REQ, t_REQ_SIZE),
	     Bits#(t_RSP, t_RSP_SIZE),
	     Transmittable#(t_REQ),
	     Transmittable#(t_RSP));

  CONNECTION_SERVER#(t_REQ, t_RSP) s <- mkConnectionServer(server_name);

  method Bool   reqNotEmpty() = s.reqNotEmpty();
  method t_REQ  getReq() = s.getReq();
  method Action deq() = s.deq();
  method Action makeResp(t_RSP data) = s.makeRsp(data);
  method Bool   respNotFull() = s.rspNotFull();

endmodule

interface Connection_Client#(type req_T, type resp_T);

  method Action makeReq(req_T data);
  method Bool   reqNotFull();
  method Bool   respNotEmpty(); 
  method resp_T getResp();
  method Action deq();
  
endinterface

module [ConnectedModule] mkConnection_Client#(String client_name)
    //interface:
                (Connection_Client#(t_REQ, t_RSP))
    provisos
            (Bits#(t_REQ, t_REQ_SIZE),
	     Bits#(t_RSP, t_RSP_SIZE),
	     Transmittable#(t_REQ),
	     Transmittable#(t_RSP));

  CONNECTION_CLIENT#(t_REQ, t_RSP) c <- mkConnectionClient(client_name);

  method Action makeReq(t_REQ data) = c.makeReq(data);
  method Bool   respNotEmpty() = c.rspNotEmpty();
  method t_RSP  getResp() = c.getRsp();
  method Action deq() = c.deq();
  method Bool   reqNotFull() = c.reqNotFull();

endmodule


interface Connection_Chain#(type msg_T);

  method ActionValue#(msg_T) receive_from_prev();
  method Action              send_to_next(msg_T data);
  
endinterface


module [Connected_Module] mkConnection_Chain#(Integer chain_num)
    //interface:
		(Connection_Chain#(msg_T))
    provisos
	    (Bits#(msg_T, msg_SZ),
	   Transmittable#(msg_T));

  RWire#(msg_T)  dataW  <- mkRWire();
  PulseWire      enW    <- mkPulseWire();
  FIFOF#(msg_T)  q       <- mkFIFOF();

  let inc = (interface PHYSICAL_CONNECTION_IN;

               // Part 1 of the anti-method to get(). Unmarshall the data and send it on the wire.
               method Action try(x);
                 dataW.wset(unmarshall(x));
               endmethod

               // Part 2 of the anti-method to get(). If someone actually was listening, then the get() succeeded.
               method Bool success();
                 return enW;
               endmethod

	     endinterface);

  let outg = (interface PHYSICAL_CONNECTION_OUT;

               // Trying to transmit something means marshalling up the first thing in the queue.
	       method PHYSICAL_CONNECTION_DATA first() = marshall(q.first());

               // Sometimes its useful to have an explicit notEmpty
               method Bool notEmpty() = q.notEmpty();

               // If we were successful we can dequeue.
	       method Action deq() = q.deq();

	     endinterface);

  //Figure out my type for typechecking
  msg_T msg = ?;
  String my_type = printType(typeOf(msg));

  // Collect up our info.
  let info = 
      LOGICAL_CHAIN_INFO 
      {
          logicalIdx: chain_num, 
          logicalType: my_type, 
          incoming: inc,
          outgoing: outg
      };

  // Register the chain
  registerChain(info);


  method Action send_to_next(msg_T data);
    q.enq(data);
  endmethod

  method ActionValue#(msg_T) receive_from_prev() if (dataW.wget() matches tagged Valid .val);

    enW.send();
    return val;

  endmethod

endmodule

module [ConnectedModule] mkConnectionServerOptional#(String server_name)
    //interface:
                (Connection_Server#(t_REQ, t_RSP))
    provisos
            (Bits#(t_REQ, t_REQ_SIZE),
	     Bits#(t_RSP, t_RSP_SIZE),
	     Transmittable#(t_REQ),
	     Transmittable#(t_RSP));

  CONNECTION_SERVER#(t_REQ, t_RSP) s <- mkPhysicalConnectionServer(server_name, Invalid, False, True);

  method Bool   reqNotEmpty() = s.reqNotEmpty();
  method t_REQ  getReq() = s.getReq();
  method Action deq() = s.deq();
  method Action makeResp(t_RSP data) = s.makeRsp(data);
  method Bool   respNotFull() = s.rspNotFull();

endmodule

module [ConnectedModule] mkConnectionClientOptional#(String client_name)
    //interface:
                (Connection_Client#(t_REQ, t_RSP))
    provisos
            (Bits#(t_REQ, t_REQ_SIZE),
	     Bits#(t_RSP, t_RSP_SIZE),
	     Transmittable#(t_REQ),
	     Transmittable#(t_RSP));

  CONNECTION_CLIENT#(t_REQ, t_RSP) c <- mkPhysicalConnectionClient(client_name, Invalid, False, True);

  method Action makeReq(t_REQ data) = c.makeReq(data);
  method Bool   respNotEmpty() = c.rspNotEmpty();
  method t_RSP  getResp() = c.getRsp();
  method Action deq() = c.deq();
  method Bool   reqNotFull() = c.reqNotFull();

endmodule
