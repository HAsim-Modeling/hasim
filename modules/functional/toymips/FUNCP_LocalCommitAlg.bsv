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
// Local Commit Unit                                                       //
//-------------------------------------------------------------------------//

//mkLocalCommit :: BypassUnit -> FP_Unit

`define MODULE_NAME "mkLocalCommit"
module [HASim_Module] mkFUNCP_LocalCommitAlg ();
  
   
  Connection_Server#(Tuple3#(Token, ExecedInst, void),
                     Tuple3#(Token, void, ExecedInst)) 
  //...
  link_lco <- mkConnection_Server("link_lco");
  
  Connection_Send#(Tuple2#(Token, PRName)) 
  //...
        link_freePReg <- mkConnection_Send("lco_to_bypass_free");

  Connection_Send#(Token) 
  //...
        link_rewindToToken <- mkConnection_Send("lco_to_bypass_rewind");

  rule handleLCO (True);
  
    match {.t, .ei, .*} <- link_lco.getReq();
    
    PRName p = case (ei) matches
                 tagged ENop    .x: return(x.opdest);
		 tagged EWB     .x: return(x.opdest);
		 tagged ELoad   .x: return(x.opdest);
		 tagged EStore  .x: return(x.opdest);
                 tagged ETerminate: return(?);
	       endcase;

    link_freePReg.send(tuple2(t, p));

    link_lco.makeResp(tuple3(t, ?, ?));

  endrule
  
endmodule
`undef MODULE_NAME  

