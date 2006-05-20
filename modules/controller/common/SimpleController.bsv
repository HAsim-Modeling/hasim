import HASim::*;
import System::*;
import TestCase::*;

import PrimArray::*;
import RegFile::*;
import Connectable::*;

/************** Simple Controller **************/

//This controller simply loads a single test case, 
//runs it, and reports the result.

//Useful for RTL simulation verification rather than
//extensive benchmarking or testing

//mkController_SimpleTest :: TestHarness -> TestCase -> ()

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
	     PrimIndex#(addr_T, addr_DY),
	     Bounded#(addr_T),
	     Literal#(addr_T),
	     Eq#(addr_T),
	     Arith#(addr_T),
	     Ord#(addr_T),
	     Eq#(value_T));


  /*********** State ***********/

  Reg#(addr_T) icur <- mkReg(minBound);
  Reg#(addr_T) dcur <- mkReg(minBound);
  Reg#(addr_T) ecur <- mkReg(minBound);
  
  Reg#(tick_T) curTick <- mkReg(minBound);

  Reg#(Bool) running  <- mkReg(False);
  Reg#(Bool) finished <- mkReg(False);
  Reg#(Bool) passed   <- mkReg(True);
    
  Bool i_loading  = icur < fromInteger(primArrayLength(tc.imem_init));
  Bool d_loading  = dcur < fromInteger(primArrayLength(tc.dmem_init));
  Bool d_checking = ecur < fromInteger(primArrayLength(tc.dmem_exp));
  
  Bool loading = i_loading || d_loading;
  
  /*********** Rules ***********/
  
  //load_imem
  
  rule load_imem (i_loading);
  
    th.imem.upd(icur, tc.imem_init[icur]);
    
    icur <= icur + 1;
    //$display("Loading IMem");
  
  endrule
  
  
  //load_dmem
  
  rule load_dmem (d_loading);
  
    th.dmem.upd(dcur, tc.dmem_init[dcur]);
    
    dcur <= dcur + 1;
    //$display("Loading DMem");
  
  endrule
  
  
  //run_prog
  
  rule run_prog (!loading && !running);
    th.tmod.exec(?);
    running <= True;
    //$display("Staring Program...");
  endrule
  
  rule tick (th.tmod.done && running);
    th.tmod.tick(curTick);
    curTick <= curTick + 1;
  endrule
  
  
  rule run_ends (running && !finished);
    let x = th.tmod.exec_response();
    finished <= True;
  endrule
  
  //check_dmem
  
  rule check_dmem (finished && d_checking);
  
    value_T v = th.dmem.sub(ecur);
    value_T exp_v = tc.dmem_exp[ecur];
     
    if (v != exp_v)
    begin
      $display("ERROR: Memory location 0x%0h does not match expected result", ecur);
      $display("       Expected Value: 0x%0h", exp_v);
      $display("       Actual Value: 0x%0h", v);
      passed <= False;
    end
    
    ecur <= ecur + 1;
    
  endrule


  //done

  rule final_check (finished && !d_checking);
    
    if (passed)
    begin
      $display("Test program finished succesfully");
      $finish(0);
    end
    else
    begin
      $display("Test program finished. Failures occurred.");
      $finish(1);
    end
    
  endrule
 
endmodule


/************** Design Equivalence Test Controller **************/

//This simple controller simply runs the same test case on two different
//Designs and reports if the tests pass, and if the results are equal.

//Useful if you make a "Golden Model" Timing Partition and wish to
//verify against it via software simulation.

module [Module] mkController_Software_TestEQ#(System#(tick_T, command_T, result_T, addr_T, inst_T, value_T) th1, 
                                              System#(tick_T, command_T, result_T, addr_T, inst_T, value_T) th2, 
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
	     Bounded#(addr_T),
	     PrimIndex#(addr_T, addr_DY),
	     Literal#(addr_T),
	     Eq#(addr_T),
	     Arith#(addr_T),
	     Ord#(addr_T),
	     Eq#(value_T));


  /*********** State ***********/
  
  Reg#(tick_T) curTick1 <- mkReg(minBound);
  Reg#(tick_T) curTick2 <- mkReg(maxBound);
  
  Reg#(Bool) passed1 <- mkReg(True);
  Reg#(Bool) passed2 <- mkReg(True);

  Reg#(Bool) running1 <- mkReg(False);
  Reg#(Bool) running2 <- mkReg(False);
  Reg#(Bool) ran     <- mkReg(False);

  Bool running = running1 || running2;

  Reg#(addr_T) icur <- mkReg(minBound);
  Reg#(addr_T) dcur <- mkReg(minBound);
  Reg#(addr_T) ecur <- mkReg(minBound);
  Reg#(addr_T) eqcur <- mkReg(minBound);
  
  Bool i_loading = icur < fromInteger(primArrayLength(tc.imem_init));
  Bool d_loading = dcur < fromInteger(primArrayLength(tc.dmem_init));
  Bool d_checking = ecur < fromInteger(primArrayLength(tc.dmem_exp));
  Bool eq_checking = ecur < maxBound;
  
  Bool loading = i_loading || d_loading;
  Bool done = !running && ran;
  
  
  /*********** Rules ***********/
  
  //load_imem
  
  rule load_imem (i_loading);
  
    th1.imem.upd(icur, tc.imem_init[icur]);
    th2.imem.upd(icur, tc.imem_init[icur]);

    icur <= icur + 1;
    $display("Controller: Loading IMems.");
  
  endrule
  
  
  //load_dmem
  
  rule load_dmem (d_loading);
  
    th1.dmem.upd(dcur, tc.dmem_init[dcur]);
    th2.dmem.upd(dcur, tc.dmem_init[dcur]);

    dcur <= dcur + 1;
    $display("Controller: Loading DMems.");
  
  endrule
  
  
  //run_prog
  
  rule run_prog (!loading && !running && !ran);
    th1.tmod.exec(?);
    th2.tmod.exec(?);
    running1 <= True;
    running2 <= True;
    ran <= True;

    $display("Controller: Staring Programs...");
  endrule
  
  rule tick1 (th1.tmod.done && running1);
    th1.tmod.tick(curTick1);
    curTick1 <= curTick1 + 1;
  endrule
  
  rule tick2(th2.tmod.done && running2);
    th2.tmod.tick(curTick2);
    curTick2 <= curTick2 + 1;
  endrule
  
  rule end_run1 (running1);
    let foo = th1.tmod.exec_response();
    running1 <= False;
  endrule
  
  rule end_run2 (running2);
    let foo = th2.tmod.exec_response();
    running2 <= False;
  endrule
  
  //check_dmem
  
  rule check_dmem (done && d_checking);
  
    value_T v1 = th1.dmem.sub(ecur);
    value_T v2 = th2.dmem.sub(ecur);
    value_T exp_v = tc.dmem_exp[ecur];
    
    if (v1 != exp_v)
    begin
      $display("Controller: ERROR: In Test Harness 1 memory location 0x%0h does not match expected result", ecur);
      $display("            Expected Value: 0x%0h", exp_v);
      $display("            Actual Value: 0x%0h", v1);
      passed1 <= False;
    end
    
    if (v2 != exp_v)
    begin
      $display("Controller: ERROR: In Test Harness 2 memory location 0x%0h does not match expected result", ecur);
      $display("            Expected Value: 0x%0h", exp_v);
      $display("            Actual Value: 0x%0h", v2);
      passed2 <= False;
    end
    ecur <= ecur + 1;
    
  endrule


  //done

  rule final_check (done && !d_checking);
    
    if (passed1 && passed2)
    begin
      $display("Controller: Test programs finished succesfully and produced equivalent memory.");
      $finish(0);
    end
    else
    begin
      $display("Controller: Test programs finished. Failures occurred.");
      $finish(1);
    end
    
  endrule
 
endmodule

