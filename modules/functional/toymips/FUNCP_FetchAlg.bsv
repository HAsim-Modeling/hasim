import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import FUNCP_Base::*;
import Debug::*;

import ISA::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"


// ToyMIPS is an extremely simple ISA designed to work as a proof of concept.
    

//-------------------------------------------------------------------------//
// Fetch Algorithm                                                         //
//-------------------------------------------------------------------------//

`define MODULE_NAME "mkFUNCP_FetchAlg"
module [HASim_Module] mkFUNCP_FetchAlg ();

  FIFO#(Tuple2#(Token, Addr)) waitingQ <- mkFIFO();

  //Ports
  
  Connection_Server#(Tuple3#(Token, void, Addr), 
                     Tuple3#(Token, Inst, Tuple2#(Addr, Inst)))
  //... 
  link_fet <- mkConnection_Server("link_fet");
	 
  Connection_Client#(Addr, Inst) 
  //...
  link_to_imem <- mkConnection_Client("mem_imem");
  
  //handleReq
  
  //Just pass the request on to the IMem

  rule handleFetch (True);
  
    debug_rule("handleFetch");
    
    Tuple3#(Token, void, Addr) tup <- link_fet.getReq();
    match {.t, .*, .a} = tup;
    
    link_to_imem.makeReq(a);
    waitingQ.enq(tuple2(t, a));
    
  endrule

  //getMemResp
  
  //Just pass the response back from the IMem

  rule getMemResp (True);
  
    debug_rule("getMemResp");
    
    Inst resp <- link_to_imem.getResp();
    
    match {.tok, .addr} = waitingQ.first();
    waitingQ.deq();
    
    link_fet.makeResp(tuple3(tok, resp, tuple2(addr, resp)));
  endrule

endmodule
`undef MODULE_NAME
