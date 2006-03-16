import Datatypes::*;
import Mem::*;
import BypassUnit::*;
import FunctionalPartition::*;
import TimingPartition::*;
import TimingPartition_Pipeline::*;

import PrimArray::*;
import List::*;
import RegFile::*;

typedef PrimArray#(Inst) Program; //XXX prettify this

typedef PrimArray#(Value) MemVals;

interface TestHarness;

  interface RegFile#(Addr, Inst) imem;
  interface RegFile#(Addr, Value) dmem;
  interface CPU cpu;
  
endinterface

module [Module] mkTestHarness#(function Module#(CPU) mkCPU(Memory#(Addr, Inst, Value, Token) m)) (TestHarness);

    
    let mem <- mkMem();
    
    CPU dut <- mkCPU(mem); //YYY mkCPU_Pipe
    
    interface imem = mem.magic_imem;
    interface dmem = mem.magic_dmem;
    interface cpu = dut;

endmodule


typedef struct
{
  Program imem_init;
  MemVals dmem_init;
  MemVals dmem_exp;
}
  TestCase;
  
function TestCase testAddition (Integer x, Integer y);

  
  Inst prog[5] = 
    {
      ILoad  {dest: r1, idx: r0, offset: 0},
      ILoad  {dest: r2, idx: r0, offset: 1},
      IAdd   {dest: r4, src1: r1, src2: r2},
      IStore {src: r4, idx: r0, offset: 2}, 
      ITerminate
    };

  Value dmem_i[2] = {fromInteger(x), fromInteger(y)};
  Value dmem_e[3] = {fromInteger(x), fromInteger(y), fromInteger(x + y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i, 
	   dmem_exp: dmem_e
	 };
	 
endfunction

function TestCase testSubtraction (Integer x, Integer y);

  
  Inst prog[5] = 
    {
      ILoad  {dest: r1, idx: r0, offset: 0},
      ILoad  {dest: r2, idx: r0, offset: 1},
      ISub   {dest: r4, src1: r1, src2: r2},
      IStore {src: r4, idx: r0, offset: 2}, 
      ITerminate
    };

  Value dmem_i[2] = {fromInteger(x), fromInteger(y)};
  Value dmem_e[3] = {fromInteger(x), fromInteger(y), fromInteger(x - y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i, 
	   dmem_exp: dmem_e
	 };
	 
endfunction

module [Module] testAddition_1();
  
  TestHarness th <- mkTestHarness(mkCPU_Test);

  Empty test <- mkTestbench(th, testAddition(11, 5));
endmodule
  
module [Module] testSubtraction_1();
  
  TestHarness th <- mkTestHarness(mkCPU_Test);

  Empty test <- mkTestbench(th, testSubtraction(11, 5));
endmodule
  
function TestCase testBranch (Integer x, Integer y);

  Inst prog[22] = 
    { 
      ILoadImm {dest: r1, imm: fromInteger(x)}, //      Set x
      ILoadImm {dest: r2, imm: fromInteger(y)}, //      Set y	    
      ILoadImm {dest: r9, imm: 1},              //      Set 1	    
      ILoadImm {dest: r3, imm: 0},              //      Set res = 0 
      IStore   {src: r1, idx: r0, offset: 1},   //      Store 1 r1
      IStore   {src: r2, idx: r0, offset: 2},   //      Store 2 r2
      IStore   {src: r9, idx: r0, offset: 9},   //      Store 9 r9
      IStore   {src: r3, idx: r0, offset: 3},   //      Store 3 r3
      ILoadImm {dest: r1, imm: 7},              //      reset r1
      ILoadImm {dest: r2, imm: 7},              //      reset r2    
      ILoadImm {dest: r9, imm: 7},              //      reset r9	    
      ILoadImm {dest: r3, imm: 7},              //      reset r3        
      ILoad    {dest: r1, idx: r0, offset: 1},   //      Load r1 1
      ILoad    {dest: r2, idx: r0, offset: 2},   //      Load r2 2
      ILoad    {dest: r9, idx: r0, offset: 9},   //      Load r9 9
      ILoad    {dest: r3, idx: r0, offset: 3},   //      Load r3 3
      IAdd     {dest: r3, src1: r3, src2: r1},  //Loop: res := res + x
      ISub     {dest: r2, src1: r2, src2: r9},  //      y := y - 1
      IBz      {cond: r2, addr: 20},             //	if y = 0 GOTO End
      IBz      {cond: r0, addr: 4},             //	GOTO Loop
      IStore   {src: r3, idx: r0, offset: 0},   //      Store res
      ITerminate                                //End:  finish(res)
    };

  Value dmem_i[1] = {0};
  Value dmem_e[1] = {fromInteger(x * y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i, 
	   dmem_exp: dmem_e
	 };
	 
endfunction

module [Module] testBranch_1 ();
  
  TestHarness th <- mkTestHarness(mkCPU_Test);

  Empty test <- mkTestbench(th, testBranch(17, 12));
endmodule
  
function TestCase testStalls (Integer x);

  Inst prog[13] = 
    { 
      ILoadImm {dest: r0, imm: 0},                //       Set 0
      ILoadImm {dest: r1, imm: 1},                //       Set 1
      ILoadImm {dest: r9, imm: fromInteger(x-1)}, //       Set x-1
      ILoadImm {dest: r8, imm: 0},                //       Set addr
      ILoad    {dest: r2, idx: r8, offset: 0},    // loop: Load a
      ILoad    {dest: r3, idx: r8, offset: 1},    //       Load b
      IAdd     {dest: r2, src1: r2, src2: r3},    //       Set a = a + b
      IStore   {src: r2, idx: r8, offset: 0},     //       Store a
      ISub     {dest: r9, src1: r9, src2: r1},    //       x = x - 1
      IAdd     {dest: r8, src1: r8, src2: r1},    //       addr = addr + 1
      IBz      {cond: r9, addr: 12},              //       if x == 0 GOTO end
      IBz      {cond: r0, addr: 4},               //       GOTO loop
      ITerminate                                  //  end: Halt.
    };

  PrimArray#(Value) dmem_i = primArrayNewU(x);
  PrimArray#(Value) dmem_e = primArrayNewU(x);
  
  for(Integer k = 0; k < x; k = k + 1)
    dmem_i[k] = fromInteger(k);
  
  for(Integer k = 0; k < (x-1); k = k + 1)
    dmem_e[k] = fromInteger(k + k + 1);
    
  dmem_e[x-1] = fromInteger(x-1);
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i, 
	   dmem_exp: dmem_e
	 };
	 
endfunction

module [Module] testStalls_1 ();
  
  TestHarness th <- mkTestHarness(mkCPU_Test);

  Empty test <- mkTestbench(th, testStalls(17));
endmodule



module mkTestbench_EQ#(TestHarness th1, TestHarness th2, TestCase tc) ();


  Reg#(Bool) passed1 <- mkReg(True);
  Reg#(Bool) passed2 <- mkReg(True);

  Reg#(Bool) running <- mkReg(False);


  Reg#(Addr) icur <- mkReg(0);
  Reg#(Addr) dcur <- mkReg(0);
  Reg#(Addr) ecur <- mkReg(0);
  Reg#(Addr) eqcur <- mkReg(0);
  
  Bool i_loading = icur < fromInteger(primArrayLength(tc.imem_init));
  Bool d_loading = dcur < fromInteger(primArrayLength(tc.dmem_init));
  Bool d_checking = ecur < fromInteger(primArrayLength(tc.dmem_exp));
  Bool eq_checking = ecur < 255;
  
  Bool loading = i_loading || d_loading;
  
  rule load_imem (i_loading);
  
    th1.imem.upd(icur, primArrayDynamicSelect(tc.imem_init, icur));
    th2.imem.upd(icur, primArrayDynamicSelect(tc.imem_init, icur));

    icur <= icur + 1;
    //$display("Loading IMem");
  
  endrule
  
  rule load_dmem (d_loading);
  
    th1.dmem.upd(dcur, primArrayDynamicSelect(tc.dmem_init, dcur));
    th2.dmem.upd(dcur, primArrayDynamicSelect(tc.dmem_init, dcur));

    dcur <= dcur + 1;
    //$display("Loading DMem");
  
  endrule
  
  rule run_prog (!loading && !running);
    th1.cpu.start();
    th2.cpu.start();

    running <= True;

    //$display("Staring Program...");
  endrule
  
  rule check_dmem (running && th1.cpu.done() && th2.cpu.done() && d_checking);
  
    Value v1 = th1.dmem.sub(ecur);
    Value v2 = th2.dmem.sub(ecur);
    Value exp_v = primArrayDynamicSelect(tc.dmem_exp, ecur);
    
    if (v1 != exp_v)
    begin
      $display("ERROR: In Test Harness 1 memory location 0x%0h does not match expected result", ecur);
      $display("       Expected Value: 0x%0h", exp_v);
      $display("       Actual Value: 0x%0h", v1);
      passed1 <= False;
    end
    
    if (v2 != exp_v)
    begin
      $display("ERROR: In Test Harness 2 memory location 0x%0h does not match expected result", ecur);
      $display("       Expected Value: 0x%0h", exp_v);
      $display("       Actual Value: 0x%0h", v2);
      passed2 <= False;
    end
    ecur <= ecur + 1;
    
  endrule

  rule done (th1.cpu.done() && th2.cpu.done() && !d_checking);
    
    if (passed1 && passed2)
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


module mkTestbench#(TestHarness th, TestCase tc) ();


  Reg#(Addr) icur <- mkReg(0);
  Reg#(Addr) dcur <- mkReg(0);
  Reg#(Addr) ecur <- mkReg(0);

  Reg#(Bool) running <- mkReg(False);
  Reg#(Bool) passed <- mkReg(True);
    
  Bool i_loading = icur < fromInteger(primArrayLength(tc.imem_init));
  Bool d_loading = dcur < fromInteger(primArrayLength(tc.dmem_init));
  Bool d_checking = ecur < fromInteger(primArrayLength(tc.dmem_exp));
  
  Bool loading = i_loading || d_loading;
  
  rule load_imem (i_loading);
  
    th.imem.upd(icur, primArrayDynamicSelect(tc.imem_init, icur));
    //imem.upd(icur, imem_init[icur]);
    icur <= icur + 1;
    //$display("Loading IMem");
  
  endrule
  
  rule load_dmem (d_loading);
  
    th.dmem.upd(dcur, primArrayDynamicSelect(tc.dmem_init, dcur));
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
    Value exp_v = primArrayDynamicSelect(tc.dmem_exp, ecur);
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


/*
interface VerilogCPU;

  method IAddress imemOut();
  method DAddress dmemOut();
  method DAddress dmemWriteAddrOut();
  method Value dmemWriteValueOut();

  method Action imemIn(Inst i);
  method Action dmemIn(Value v);

  method Action start();
  method Bool done();

endinterface

(* synthesize *)
module mkVerilogCPU(VerilogCPU);
  VerilogCPU x <- mkVerilogCPU_i();
   
  method imemOut=x.imemOut;
  method dmemOut=x.dmemOut;
   
  method dmemWriteAddrOut = x.dmemWriteAddrOut;
  method dmemWriteValueOut = x.dmemWriteValueOut;
   

  method imemIn = x.imemIn;
  method dmemIn = x.dmemIn;

  method start = x.start;
  method done  = x.done;
   
endmodule

 
import "BVI" proc = 
   module mkVerilogCPU (VerilogCPU);
      default_clock clk(CLK);
      default_reset rst(RST_N);
      method imem_o imemOut() ready (RDY_imem_o);
      method dmem_o dmemOut() ready (RDY_dmem_o);
      method write_a_o dmemWriteAddrOut();
      method write_v_o dmemWriteValueOut() ready (RDY_write_o);
      method imemIn(imem_i) enable(EN_imem_i);
      method dmemIn(dmem_i) enable(EN_dmem_i);
      method start() enable(EN_start);
      method done done ;
   
      schedule (imemOut,dmemOut,dmemWriteAddrOut,dmemWriteValueOut,done)
	CF (imemOut,dmemOut,dmemWriteAddrOut,dmemWriteValueOut,done);
   
      schedule (imemOut,dmemOut,dmemWriteAddrOut,dmemWriteValueOut,
		done) CF (imemIn, dmemIn, start);
      schedule (imemIn) CF (dmemIn, start);
      schedule (dmemIn) CF (start);
   endmodule

module [Module] mkVerilogTH#(function Module#(VerilogCPU) mkVCPU()) (TestHarness);

  IMem imem <- mkIMem();
  DMem dmem <- mkDMem();

  VerilogCPU cpu <- mkVCPU();
  

  rule stitchIMem (!cpu.done());
    IAddress a = cpu.imemOut();
    Inst i = imem.sub(a);
    cpu.imemIn(i);
  endrule

  rule stitchDMem (!cpu.done());
    DAddress a = cpu.dmemOut();
    Value v = dmem.sub(a);
    cpu.dmemIn(v);
  endrule

  rule stitchDMemWrite (!cpu.done());
    DAddress a = cpu.dmemWriteAddrOut();
    Value v = cpu.dmemWriteValueOut();
    dmem.upd(a, v);
  endrule

  method Action loadInst(IAddress a, Inst i) if (cpu.done());
    imem.upd(a, i);
  endmethod

  method Action loadData(IAddress a, Value v) if (cpu.done());
    dmem.upd(a, v);
  endmethod

  
  method Value readData(DAddress a) if (cpu.done());
    return dmem.sub(a);
  endmethod

  method done = cpu.done();

  method Action start() if (cpu.done()) ;
    cpu.start();
  endmethod   

endmodule

*/
