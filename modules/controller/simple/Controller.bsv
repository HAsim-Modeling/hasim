//HASim imports
import HASim::*;
import System::*;
import TestCase::*;
import Loader::*;
import Checker::*;

//BSV library imports
import PrimArray::*;
import RegFile::*;
import Connectable::*;

//************* OneTest Controller **************

//This controller simply loads a single test case, 
//runs it, and reports the result.

//Useful for RTL simulation verification rather than
//extensive benchmarking or testing

//mkController_Software_OneTest :: TestHarness -> TestCase -> Module ()

typedef enum
{
  Init,
  Loading,
  Running,
  Checking,
  Finished
}
  ConState
           deriving (Eq, Bits);

module [Module] mkController_Software_OneTest#(System#(tick_T, command_T, result_T, addr_T, inst_T, value_T) th, 
                                               TestCase#(inst_T, value_T) tc) 
    //interface:
                ()
    provisos
            (Bits#(addr_T,  addr_SZ),
	     Bits#(inst_T,  inst_SZ),
	     Bits#(value_T, value_SZ),
	     Bits#(tick_T, tick_SZ),
	     Bounded#(tick_T),
	     Arith#(tick_T),
	     //PrimIndex#(addr_T, addr_DY),
	     PrimIndex#(addr_T),
	     Bounded#(addr_T),
	     Literal#(addr_T),
	     Eq#(addr_T),
	     Arith#(addr_T),
	     Ord#(addr_T),
	     Eq#(value_T));


  //*********** State ***********

  Loader loader <- mkLoader(th.imem, th.dmem, tc);
  Checker#(addr_T, value_T) checker <- mkChecker(th.dmem, tc);
  
  Reg#(tick_T) curTick <- mkReg(minBound);

  Reg#(ConState) state <- mkReg(Loading);
  
  //*********** Rules ***********
  
  //load_imem
  
  rule load_prog(state == Init);
  
    loader.loadProgram();
    state <= Loading;
  
  endrule
  
  //run_prog
  
  rule run_prog (state == Loading && loader.done());
  
    th.tmod.exec(?);
    state <= Running;
    
    //$display("Staring Program...");
  endrule
  
  rule tick (th.tmod.done && state == Running);
    th.tmod.tick(curTick);
    curTick <= curTick + 1;
  endrule
  
  rule run_ends (state == Running);
    let x = th.tmod.exec_response();
    checker.checkResult();
    state <= Checking;
  endrule
  
  rule getFails (state == Checking);
    
    match {.a, .exp_v, .found_v} <- checker.getFailure();

    $display("Controller: ERROR: Memory location 0x%0h does not match expected result", a);
    $display("            Expected Value: 0x%0h", exp_v);
    $display("            Actual Value: 0x%0h", found_v);
    
  endrule
 
  rule runPassed (state == Checking && checker.done() && checker.passed());

    state <= Finished;

    $display("Controller: Test program finished succesfully.");
    $finish(0);
    
  endrule
 
  rule runFailed (state == Checking && checker.done() && !checker.passed());

    state <= Finished;

    $display("Controller: Test programs finished. One or more failures occurred.");
    $finish(1);
    
  endrule
 
endmodule

