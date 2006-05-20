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

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;

// Global top-level is a Model.

typedef Empty Model;

/************* Controller Interface *************/

// A Controller is the part of the Model which controls the
// simulation. All communication with the "outside world" passes
// through it. It switches between normal simulation and special
// modes like Debug/Events/Stats.

// Eventually this could be mostly implemented as software on the
// FPGA PowerPC.

// In the future I expect this to become more interesting with
// Bus interfaces.

typedef Empty Controller;

/************* TModule Interface *************/

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

/************* Timing Partition Heirarchy *************/

// Top-level TModule is a System.
// Currently it includes magic links to the IMem/DMem 
// so that the controller can load the program

interface System#(type tick_T, 
                  type command_T, 
		  type result_T, 
		  type addr_T,
		  type inst_T,
		  type value_T);

  //TModule interface
  interface TModule#(tick_T, command_T, result_T) tmod;

  //Magic Memory interfaces
  interface RegFile#(addr_T, inst_T)  imem;
  interface RegFile#(addr_T, value_T) dmem;

endinterface
        

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



/************************** Links **************************/
/*                                                         */
/* Links are the plumbing of HASim. They represent basic   */
/* point-to-point communication. The advantage over        */
/* traditional Bluespec Connectables is that they are      */
/* easier to use, and can easily be extended to include    */
/* model latency (ASim Ports).                             */
/*                                                         */
/* These might eventually be donated to the Bluespec       */
/* library.                                                */
/*                                                         */
/***********************************************************/


//The basic sending link.

interface Link_Send#(type msg_T);
  
  //For the user
  method Action send(msg_T data);

  //The outgoing connection
  //Hooked up by the system
  interface Get#(msg_T) outgoing;
  
endinterface


//The basic receiving link.

interface Link_Receive#(type msg_T);
  
  //For the user
  method ActionValue#(msg_T) receive();

  //The incoming connection
  //Hooked up by the system
  interface Put#(msg_T) incoming;
  
endinterface


// A client sends requests and receives responses
// (which may not come instantly)

interface Link_Client#(type req_T, type resp_T);

  //For the user
  method Action               makeReq(req_T data);
  method ActionValue#(resp_T) getResp;

  //The outgoing req and incoming resp
  //Hooked up by the system
  interface Client#(req_T, resp_T) client;
  
endinterface


// A server receives requests and gives back responses
// It can take any amount of time, and there is no assumption
// that the responses are FIFO.

interface Link_Server#(type req_T, type resp_T);

  //For the user
  method ActionValue#(req_T) getReq();
  method Action              makeResp(resp_T data);

  //The outgoing req and incoming resp
  //Hooked up by the system
  interface Server#(req_T, resp_T) server;
  
endinterface

//Link Implementations

// These might live in a different file.

// Currently all requests are buffered until we are 100% sure
// about correctness.

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

