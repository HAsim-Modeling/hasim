import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import FUNCP_Base::*;
import FUNCP_MemState::*;
import Debug::*;

import ISA::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"


//-------------------------------------------------------------------------//
// Global Commit Unit                                                      //
//-------------------------------------------------------------------------//

//mkGlobalCommit :: Memory -> FP_Unit

`define MODULE_NAME "mkGlobalCommit"
module [HASim_Module] mkFUNCP_GlobalCommitAlg ();

  Connection_Send#(Token) link_mem_commit <- mkConnection_Send("mem_commit");
  
  Connection_Server#(Tuple3#(Token, ExecedInst, void),
                     Tuple3#(Token, void, void)) 
  //...
  link_gco <- mkConnection_Server("link_gco");
  
  rule handleGCO (True);
  
    match {.tok, .*, .*} <- link_gco.getReq();
    
    link_mem_commit.send(tok);
    
    link_gco.makeResp(tuple3(tok, ?, ?));
  
  endrule
  
  
endmodule
`undef MODULE_NAME
