import HASim::*;
import TestHarness::*;
import TestCase::*;
import Ports::*;

import PrimArray::*;
import RegFile::*;
import Connectable::*;

/************** Simple Controller **************/

//This controller simply loads a single test case, 
//runs it, and reports the result.

//Useful for RTL simulation verification rather than
//extensive benchmarking or testing

//mkController_SimpleTest :: TestHarness -> TestCase -> ()

module [Module] mkController_SimpleTest#(TestHarness#(addr_T, inst_T, value_T) th, 
                                         TestCase#(inst_T, value_T) tc) 
    //interface:
                ()
    provisos
            (Bits#(addr_T,  addr_SZ),
	     Bits#(inst_T,  inst_SZ),
	     Bits#(value_T, value_SZ),
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

  Reg#(Bool) running  <- mkReg(False);
  Reg#(Bool) started  <- mkReg(False);
  Reg#(Bool) finished <- mkReg(False);
  Reg#(Bool) passed   <- mkReg(True);
    
  Bool i_loading  = icur < fromInteger(primArrayLength(tc.imem_init));
  Bool d_loading  = dcur < fromInteger(primArrayLength(tc.dmem_init));
  Bool d_checking = ecur < fromInteger(primArrayLength(tc.dmem_exp));
  
  Bool loading = i_loading || d_loading;
  
  /*********** Ports ***********/
  
  Port_Send#(void)    port_start <- mkPort_Send("port_start");
  Port_Receive#(Bool) port_done  <- mkPort_Receive("port_done");
  
  mkConnection(port_start.outgoing, th.start);
  mkConnection(th.done, port_done.incoming);
  
  /*********** Rules ***********/
  
  //load_imem
  
  rule load_imem (i_loading);
  
    th.imem.upd(icur, primArrayDynamicSelect(tc.imem_init, icur));
    
    icur <= icur + 1;
    //$display("Loading IMem");
  
  endrule
  
  
  //load_dmem
  
  rule load_dmem (d_loading);
  
    th.dmem.upd(dcur, primArrayDynamicSelect(tc.dmem_init, dcur));
    
    dcur <= dcur + 1;
    //$display("Loading DMem");
  
  endrule
  
  
  //run_prog
  
  rule run_prog (!loading && !running && !started);
    port_start.send(?);
    running <= True;
    //$display("Staring Program...");
  endrule
  
  rule run_starts (running && !started);
    Bool d <- port_done.receive();
    started <= !d;
  endrule
  
  rule run_ends (running && started && !finished);
    Bool d <- port_done.receive();
    finished <= d;
  endrule
  
  //check_dmem
  
  rule check_dmem (finished && d_checking);
  
    value_T v = th.dmem.sub(ecur);
    value_T exp_v = primArrayDynamicSelect(tc.dmem_exp, ecur);
     
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

module [Module] mkController_TestEQ#(TestHarness#(addr_T, inst_T, value_T) th1, 
                                     TestHarness#(addr_T, inst_T, value_T) th2, 
                                     TestCase#(inst_T, value_T) tc) 
    //interface:
                ()
    provisos
            (Bits#(addr_T,  addr_SZ),
	     Bits#(inst_T,  inst_SZ),
	     Bits#(value_T, value_SZ),
	     Bounded#(addr_T),
	     Literal#(addr_T),
	     Eq#(addr_T),
	     Arith#(addr_T),
	     Ord#(addr_T),
	     Eq#(value_T));


  /*********** State ***********/
  
  Reg#(Bool) passed1 <- mkReg(True);
  Reg#(Bool) passed2 <- mkReg(True);

  Reg#(Bool) running <- mkReg(False);
  Reg#(Bool) ran     <- mkReg(False);


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
  
  /*********** Ports ***********/
  
  Port_Send#(void)    port1_start <- mkPort_Send("port_start");
  Port_Send#(void)    port2_start <- mkPort_Send("port_start");
  Port_Receive#(Bool) port1_done  <- mkPort_Receive("port_done");
  Port_Receive#(Bool) port2_done  <- mkPort_Receive("port_done");
  
  mkConnection(port1_start.outgoing, th1.start);
  mkConnection(port2_start.outgoing, th2.start);
  mkConnection(th1.done, port1_done.incoming);
  mkConnection(th2.done, port2_done.incoming);
  
  /*********** Rules ***********/
  
  //load_imem
  
  rule load_imem (i_loading);
  
    th1.imem.upd(icur, primArrayDynamicSelect(tc.imem_init, icur));
    th2.imem.upd(icur, primArrayDynamicSelect(tc.imem_init, icur));

    icur <= icur + 1;
    $display("Controller: Loading IMems.");
  
  endrule
  
  
  //load_dmem
  
  rule load_dmem (d_loading);
  
    th1.dmem.upd(dcur, primArrayDynamicSelect(tc.dmem_init, dcur));
    th2.dmem.upd(dcur, primArrayDynamicSelect(tc.dmem_init, dcur));

    dcur <= dcur + 1;
    $display("Controller: Loading DMems.");
  
  endrule
  
  
  //run_prog
  
  rule run_prog (!loading && !running && !ran);
    port1_start.send(?);
    port2_start.send(?);

    running <= True;
    ran <= True;

    $display("Controller: Staring Programs...");
  endrule
  
  rule end_run (running);
    Bool d1 <- port1_done.receive();
    Bool d2 <- port2_done.receive();
    running <= !d1 && !d2;
  endrule
  
  //check_dmem
  
  rule check_dmem (done && d_checking);
  
    value_T v1 = th1.dmem.sub(ecur);
    value_T v2 = th2.dmem.sub(ecur);
    value_T exp_v = primArrayDynamicSelect(tc.dmem_exp, ecur);
    
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

