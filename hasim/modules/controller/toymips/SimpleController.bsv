import Datatypes::*;

import List::*;
import RegFile::*;

/************** Simple Controller **************/

//This controller simply loads a single test case, 
//runs it, and reports the result.

//Useful for RTL simulation verification rather than
//extensive benchmarking or testing


module mkController_SimpleTest#(TestHarness th, TestCase tc) ();


  Reg#(Addr) icur <- mkReg(0);
  Reg#(Addr) dcur <- mkReg(0);
  Reg#(Addr) ecur <- mkReg(0);

  Reg#(Bool) running <- mkReg(False);
  Reg#(Bool) passed <- mkReg(True);
    
  Bool i_loading = icur < fromInteger(length(tc.imem_init));
  Bool d_loading = dcur < fromInteger(length(tc.dmem_init));
  Bool d_checking = ecur < fromInteger(length(tc.dmem_exp));
  
  Bool loading = i_loading || d_loading;
  
  rule load_imem (i_loading);
  
    th.imem.upd(icur, select(tc.imem_init, icur));
    //imem.upd(icur, imem_init[icur]);
    icur <= icur + 1;
    //$display("Loading IMem");
  
  endrule
  
  rule load_dmem (d_loading);
  
    th.dmem.upd(dcur, select(tc.dmem_init, dcur));
    //dmem.upd(dcur, dmem_init[dcur]);
    dcur <= dcur + 1;
    //$display("Loading DMem");
  
  endrule
  
  rule run_prog (!loading && !running);
    th.cpu.start();
    running <= True;
    //$display("Staring Program...");
  endrule
  
  rule check_dmem (running && th.cpu.done() && d_checking);
  
    Value v = th.dmem.sub(ecur);
    Value exp_v = select(tc.dmem_exp, ecur);
    //Value exp_v = dmem_exp[ecur];
     
    if (v != exp_v)
    begin
      $display("ERROR: Memory location 0x%0h does not match expected result", ecur);
      $display("       Expected Value: 0x%0h", exp_v);
      $display("       Actual Value: 0x%0h", v);
      passed <= False;
    end
    
    ecur <= ecur + 1;
    
  endrule

  rule done (th.cpu.done() && !d_checking);
    
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

module mkController_TestEQ#(TestHarness th1, TestHarness th2, TestCase tc) ();


  Reg#(Bool) passed1 <- mkReg(True);
  Reg#(Bool) passed2 <- mkReg(True);

  Reg#(Bool) running <- mkReg(False);


  Reg#(Addr) icur <- mkReg(0);
  Reg#(Addr) dcur <- mkReg(0);
  Reg#(Addr) ecur <- mkReg(0);
  Reg#(Addr) eqcur <- mkReg(0);
  
  Bool i_loading = icur < fromInteger(length(tc.imem_init));
  Bool d_loading = dcur < fromInteger(length(tc.dmem_init));
  Bool d_checking = ecur < fromInteger(length(tc.dmem_exp));
  Bool eq_checking = ecur < 255;
  
  Bool loading = i_loading || d_loading;
  
  rule load_imem (i_loading);
  
    th1.imem.upd(icur, select(tc.imem_init, icur));
    th2.imem.upd(icur, select(tc.imem_init, icur));

    icur <= icur + 1;
    $display("Controller: Loading IMems.");
  
  endrule
  
  rule load_dmem (d_loading);
  
    th1.dmem.upd(dcur, select(tc.dmem_init, dcur));
    th2.dmem.upd(dcur, select(tc.dmem_init, dcur));

    dcur <= dcur + 1;
    $display("Controller: Loading DMems.");
  
  endrule
  
  rule run_prog (!loading && !running);
    th1.cpu.start();
    th2.cpu.start();

    running <= True;

    $display("Controller: Staring Programs...");
  endrule
  
  rule check_dmem (running && th1.cpu.done() && th2.cpu.done() && d_checking);
  
    Value v1 = th1.dmem.sub(ecur);
    Value v2 = th2.dmem.sub(ecur);
    Value exp_v = select(tc.dmem_exp, ecur);
    
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

  rule done (th1.cpu.done() && th2.cpu.done() && !d_checking);
    
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

