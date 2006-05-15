
import GetPut::*;
import ClientServer::*;

import FIFO::*;

import HASim::*;

interface Link_Send#(type msg_T);
  
  //For the user
  method Action send(msg_T data);

  //The outgoing connection
  //Hooked up by the system
  interface Get#(msg_T) outgoing;
  
endinterface


interface Link_Receive#(type msg_T);
  
  //For the user
  method ActionValue#(msg_T) receive();

  //The incoming connection
  //Hooked up by the system
  interface Put#(msg_T) incoming;
  
endinterface

interface Link_Client#(type req_T, type resp_T);

  //For the user
  method Action               makeReq(req_T data);
  method ActionValue#(resp_T) getResp;

  //The outgoing req and incoming resp
  //Hooked up by the system
  interface Client#(req_T, resp_T) client;
  
endinterface

interface Link_Server#(type req_T, type resp_T);

  //For the user
  method ActionValue#(req_T) getReq();
  method Action              makeResp(resp_T data);

  //The outgoing req and incoming resp
  //Hooked up by the system
  interface Server#(req_T, resp_T) server;
  
endinterface

//Module implementations

module [Module] mkLink_Send#(String portname)
    //interface:
                (Link_Send#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ));

  FIFO#(msg_T) q <- mkFIFO();


  interface Get outgoing;
  
    method ActionValue#(msg_T) get();
      q.deq();
      return q.first();
    endmethod
    
  endinterface

  method Action send(msg_T data);
    q.enq(data);
  endmethod

endmodule

module [Module] mkLink_Receive#(String portname)
    //interface:
                (Link_Receive#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ));

  Wire#(msg_T) w <- mkWire();


  interface Put incoming;
  
    method Action put(msg_T data);
      w <= data;
    endmethod
    
  endinterface

  method ActionValue#(msg_T) receive();
    noAction;
    return w;
  endmethod

endmodule

module [Module] mkLink_Client#(String portname)
    //interface:
                (Link_Client#(req_T, resp_T))
    provisos
            (Bits#(req_T, req_SZ),
	     Bits#(resp_T, resp_SZ));

  FIFO#(req_T)  reqQ  <- mkFIFO();
  Wire#(resp_T) respW <- mkWire();

  method Action makeReq(req_T data);
    reqQ.enq(data);
  endmethod
  
  method ActionValue#(resp_T) getResp;
    noAction;
    return respW;
  endmethod

  interface Client client;

    interface Get request;

      method ActionValue#(req_T) get();

	reqQ.deq();
	return reqQ.first();

      endmethod

    endinterface

    interface Put response;

      method Action put(resp_T data);

	respW <= data;

      endmethod

    endinterface

  endinterface
  
endmodule

module [Module] mkLink_Server#(String portname)
    //interface:
                (Link_Server#(req_T, resp_T))
    provisos
            (Bits#(req_T, req_SZ),
	     Bits#(resp_T, resp_SZ));

  Wire#(req_T)  reqW  <- mkWire();
  FIFO#(resp_T) respQ <- mkFIFO();

  method ActionValue#(req_T) getReq();
    
    noAction;
    return reqW;
    
  endmethod
  
  
  method Action makeResp(resp_T data);
  
    respQ.enq(data);
  
  endmethod

  interface Server server;

    interface Put request;

      method Action put(req_T data);

	reqW <= data;

      endmethod

    endinterface

    interface Get response;

      method ActionValue#(resp_T) get();

	respQ.deq();
	return respQ.first();

      endmethod

    endinterface

  endinterface
  
endmodule
