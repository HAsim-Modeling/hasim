
import GetPut::*;
import ClientServer::*;
import RegFile::*;

import FIFO::*;

import HASim::*;

interface Port_Send#(type msg_T);
  
  //For the user
  method Action send(msg_T data);

  //The outgoing connection
  //Hooked up by the system
  interface Get#(msg_T) outgoing;
  
endinterface


interface Port_Receive#(type msg_T);
  
  //For the user
  method ActionValue#(msg_T) receive();

  //The incoming connection
  //Hooked up by the system
  interface Put#(msg_T) incoming;
  
endinterface

interface Port_Client#(type req_T, type resp_T);

  //For the user
  method Action               makeReq(req_T data);
  method ActionValue#(resp_T) getResp;

  //The outgoing req and incoming resp
  //Hooked up by the system
  interface Client#(req_T, resp_T) client;
  
endinterface

interface Port_Server#(type req_T, type resp_T);

  //For the user
  method ActionValue#(req_T) getReq();
  method Action              makeResp(resp_T data);

  //The outgoing req and incoming resp
  //Hooked up by the system
  interface Server#(req_T, resp_T) server;
  
endinterface
/*
//Functional Partition with support for Ports

//interface FunctionalPartition_Ports

interface FunctionalPartition_Ports#(type tick_T,    type token_T,
                                     type addr_T,    type value_T,
			             type inst_T,
			             type tok_req_T, type tok_resp_T,
                                     type fet_req_T, type fet_resp_T,
			             type dec_req_T, type dec_resp_T,
			             type exe_req_T, type exe_resp_T,
			             type mem_req_T, type mem_resp_T,
			             type lcm_req_T, type lcm_resp_T,
			             type gcm_req_T, type gcm_resp_T);

  //Interface For Timing Partition Ports
  
  interface FPServer#(token_T, tick_T, tok_req_T, tok_resp_T) tokgen;
  interface FPServer#(token_T, tick_T, fet_req_T, fet_resp_T) fetch;
  interface FPServer#(token_T, tick_T, dec_req_T, dec_resp_T) decode;
  interface FPServer#(token_T, tick_T, exe_req_T, exe_resp_T) execute;
  interface FPServer#(token_T, tick_T, mem_req_T, mem_resp_T) memory;
  interface FPServer#(token_T, tick_T, lcm_req_T, lcm_resp_T) local_commit;
  interface FPServer#(token_T, tick_T, gcm_req_T, gcm_resp_T) global_commit;
  
  interface Put#(token_T) killToken;
  
  
  //Interface For Memory Ports
  
  interface Client#(MemReq#(addr_T, token_T, value_T), MemResp#(value_T)) to_dmem;
  interface Client#(addr_T, inst_T) to_imem;
  
  interface Get#(token_T) commit;
  interface Get#(Tuple2#(token_T, token_T)) killRange; 
  
endinterface

//Timing Partition with support for Ports

typedef Client#(Tuple3#(token_T, req_T, tick_T), Tuple2#(token_T, resp_T)) 
        TPClient#(type token_T, type tick_T, type req_T, type resp_T);

interface TimingPartition_Ports#(type tick_T,    type token_T,
				 type tok_req_T, type tok_resp_T,
				 type fet_req_T, type fet_resp_T,
				 type dec_req_T, type dec_resp_T,
				 type exe_req_T, type exe_resp_T,
				 type mem_req_T, type mem_resp_T,
				 type lcm_req_T, type lcm_resp_T,
				 type gcm_req_T, type gcm_resp_T);

  //Interface for CPU Ports
  interface Put#(void) start;
  interface Get#(Bool) done;
  
  //Interface for Functional Partition Ports
  
  interface TPClient#(token_T, tick_T, tok_req_T, tok_resp_T) tokgen;
  interface TPClient#(token_T, tick_T, fet_req_T, fet_resp_T) fetch;
  interface TPClient#(token_T, tick_T, dec_req_T, dec_resp_T) decode;
  interface TPClient#(token_T, tick_T, exe_req_T, exe_resp_T) execute;
  interface TPClient#(token_T, tick_T, mem_req_T, mem_resp_T) memory;
  interface TPClient#(token_T, tick_T, lcm_req_T, lcm_resp_T) local_commit;
  interface TPClient#(token_T, tick_T, gcm_req_T, gcm_resp_T) global_commit;
  
  interface Get#(token_T) killToken;
  
endinterface

// Memory system with support for Ports

interface Memory_Ports#(type addr_T,  type inst_T, type value_T, type token_T);

  interface Server#(addr_T, inst_T) imem;
  interface Server#(MemReq#(addr_T, token_T, value_T), MemResp#(value_T)) dmem;
  
  interface Put#(token_T) commit;
  interface Put#(Tuple2#(token_T, token_T)) killRange; 
  
  //Magic link for the test harness to load the program
  interface RegFile#(addr_T, inst_T) magic_imem;
  interface RegFile#(addr_T, value_T) magic_dmem;

endinterface

//CPU with support for ports

interface CPU_Ports#(type token_T, type addr_T, type value_T, type inst_T);

  interface Put#(void) start;
  interface Get#(Bool) done;
  
  //Interface For Memory Ports
  
  interface Client#(MemReq#(addr_T, token_T, value_T), MemResp#(value_T)) to_dmem;
  interface Client#(addr_T, inst_T) to_imem;
  
  interface Get#(token_T) commit;
  interface Get#(Tuple2#(token_T, token_T)) killRange; 

endinterface
*/
//Module implementations

module [Module] mkPort_Send#(String portname)
    //interface:
                (Port_Send#(msg_T))
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

module [Module] mkPort_Receive#(String portname)
    //interface:
                (Port_Receive#(msg_T))
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

module [Module] mkPort_Client#(String portname)
    //interface:
                (Port_Client#(req_T, resp_T))
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

module [Module] mkPort_Server#(String portname)
    //interface:
                (Port_Server#(req_T, resp_T))
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
