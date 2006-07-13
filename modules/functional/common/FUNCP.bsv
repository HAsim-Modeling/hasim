import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import Debug::*;
import FUNCP_Base::*;
import FUNCP_Pipeline::*;
import FUNCP_MemState::*;
import FUNCP_RegState::*;
import FUNCP_Loader::*;
import FUNCP_Checker::*;

import ISA::*;

`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"


module [HASim_Module] mkFUNCP (TModule#(Command, Response));
  
  Loader  loader  <- mkFUNCP_Loader();
  Checker checker <- mkFUNCP_Checker();
  
  Reg#(Bool)      loading  <- mkReg(False);
  Reg#(Bool)      checking <- mkReg(False);
  FIFO#(Response) respQ    <- mkFIFO();
  
    rule loadResp (loading && loader.done());
      respQ.enq(RESP_DoneLoading);
      loading <= False;
    endrule
    
    rule failResp (checking);
      match {.a, .exp, .v} <- checker.getFailure();
      respQ.enq(RESP_Failure {addr: a, exp_v: exp, found_v: v});
    endrule
    
    rule checkResp (checking && checker.done());
      if (checker.passed())
        respQ.enq(RESP_CheckPassed);
      else
        respQ.enq(RESP_CheckFailed);
      checking <= False;
    endrule
    
    method Action exec(Command com);
    
      case (com) matches
        tagged COM_LoadState:
	begin
	  loading <= True;
	  loader.loadProgram();
	end
	tagged COM_CheckResult:
	begin
	  checker.checkResult();
	  checking <= True;
	end
	default:
          $display("Error! FP unknown command!");
      endcase
    endmethod
    
    method ActionValue#(Response) response();
    
      respQ.deq();
      return respQ.first();
    
    endmethod
  
endmodule
