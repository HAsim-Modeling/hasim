
//BSV library imports
import PrimArray::*;
import RegFile::*;
import Connectable::*;

//HASim library imports
import HASim::*;

//HASim model-specific imports
import ISA::*;

//************* OneTest Controller **************

//This controller simply loads a single test case, 
//runs it, and reports the result.

//Useful for RTL simulation verification rather than
//extensive benchmarking or testing

//mkController_Software_OneTest :: TestHarness -> TestCase -> Module ()

typedef enum
{
  CON_Init,
  CON_Loading,
  CON_Running,
  CON_Checking,
  CON_Finished
}
  ConState
           deriving (Eq, Bits);

module [HASim_Module] mkController#(TModule#(Command, Response) th) ();


  //*********** State ***********
  
  Reg#(Tick) curTick <- mkReg(minBound);

  Reg#(ConState) state <- mkReg(CON_Init);
  
  //*********** Rules ***********
  
  //load_imem
  
  rule load_prog(state == CON_Init);
  
    th.exec(COM_LoadState);
    state <= CON_Loading;
  
  endrule
  
  //run_prog
  
  rule run_prog (state == CON_Loading);
  
    let resp <- th.response();
    case (resp) matches
      tagged RESP_DoneLoading:
	begin
          th.exec(COM_RunProgram);
          state <= CON_Running;
	end
      default:
        begin
          $display("Unexpected FP response.");
	  $finish(1);
	end
    endcase 
    //$display("Staring Program...");
  endrule
  
  rule run_ends (state == CON_Running);
  
    let resp <- th.response();
  
    case (resp) matches
      tagged RESP_DoneRunning:
	begin
          th.exec(COM_CheckResult);
          state <= CON_Checking;
	end
      default:
        begin
          $display("Unexpected TModule response.");
	  $finish(1);
	end
    endcase
  endrule
  
  rule getFails (state == CON_Checking);
    
    let resp <- th.response();
  
    case (resp) matches
      tagged RESP_Failure .f:
	begin
	  $display("Controller: ERROR: Memory location 0x%0h does not match expected result", f.addr);
	  $display("            Expected Value: 0x%0h", f.exp_v);
	  $display("            Actual Value: 0x%0h", f.found_v);
	end
      tagged RESP_CheckPassed:
        begin
	  state <= CON_Finished;

	  $display("Controller: Test program finished succesfully.");
	  $finish(0);
	end
      tagged RESP_CheckFailed:
        begin
	  state <= CON_Finished;

	  $display("Controller: Test program finished. One or more failures occurred.");
	  $finish(1);
	end
      default:
        begin
          $display("Unexpected TModule response.");
	  $finish(1);
	end
    endcase

    
  endrule
 
endmodule

