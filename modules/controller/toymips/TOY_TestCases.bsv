///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// TOY_TestCases.bsv                                                         //
//                                                                           //
// Test cases for the ToyMIPS ISA. To be used while verifying timing         //
// partitions.                                                               //
//                                                                           //
//                                                                           //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////


import HASim::*;
import TestCase::*;
import SimpleController::*;
import System::*;

import TOY_Datatypes::*;
import TOY_FunctionalPartition::*;
import TOY_Mem::*;

import TOY_TimingPartition::*;
import TOY_System::*;

import PrimArray::*;
import RegFile::*;
import GetPut::*;
import Connectable::*;

//************ Test Case Datatypes ************//

typedef Program#(TOY_Inst) TOY_Program;




//************ Test Cases ************//

// A simple test that of x + y

function TestCase#(TOY_Inst, TOY_Value) testAddition (Integer x, Integer y);

  
  TOY_Inst prog[5] = 
    {
      ILoad  {dest: r1, idx:  r0, offset: 0  },
      ILoad  {dest: r2, idx:  r0, offset: 1  },
      IAdd   {dest: r4, src1: r1, src2:   r2 },
      IStore {src:  r4, idx:  r0, offset: 2  },
      ITerminate
    };

  TOY_Value dmem_i[2] = {fromInteger(x), fromInteger(y)};
  TOY_Value dmem_e[3] = {fromInteger(x), fromInteger(y), fromInteger(x + y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i, 
	   dmem_exp:  dmem_e
	 };
	 
endfunction

// A simple test of x - y

function TestCase#(TOY_Inst, TOY_Value) testSubtraction (Integer x, Integer y);

  
  TOY_Inst prog[5] = 
    {
      ILoad  {dest: r1, idx:  r0, offset: 0  },
      ILoad  {dest: r2, idx:  r0, offset: 1  },
      ISub   {dest: r4, src1: r1, src2:   r2 },
      IStore {src:  r4, idx:  r0, offset: 2  },
      ITerminate
    };

  TOY_Value dmem_i[2] = {fromInteger(x), fromInteger(y)};
  TOY_Value dmem_e[3] = {fromInteger(x), fromInteger(y), fromInteger(x - y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i, 
	   dmem_exp:  dmem_e
	 };
	 
endfunction

// A more complex, branching test
// Tests x * y without using Mul instruction

function TestCase#(TOY_Inst, TOY_Value) testBranch (Integer x, Integer y);

  TOY_Inst prog[22] = 
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
      ILoad    {dest: r1, idx: r0, offset: 1},  //	Load r1 1
      ILoad    {dest: r2, idx: r0, offset: 2},  //	Load r2 2
      ILoad    {dest: r9, idx: r0, offset: 9},  //	Load r9 9
      ILoad    {dest: r3, idx: r0, offset: 3},  //	Load r3 3
      IAdd     {dest: r3, src1: r3, src2: r1},  //Loop: res := res + x
      ISub     {dest: r2, src1: r2, src2: r9},  //      y := y - 1
      IBz      {cond: r2, addr: 20},            //	if y = 0 GOTO End
      IBz      {cond: r0, addr: 4},             //	GOTO Loop
      IStore   {src: r3, idx: r0, offset: 0},   //      Store res
      ITerminate                                //End:  finish(res)
    };

  TOY_Value dmem_i[1] = {0};
  TOY_Value dmem_e[1] = {fromInteger(x * y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i,
	   dmem_exp:  dmem_e
	 };
	 
endfunction


// A branching test with data dependencies that ensures 
// we are stalling correctly

function TestCase#(TOY_Inst, TOY_Value) testStalls (Integer x);

  TOY_Inst prog[13] = 
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
    
  TOY_Value dmem_i[x];
  TOY_Value dmem_e[x];
  
  for(Integer k = 0; k < x; k = k + 1)
    dmem_i[k] = fromInteger(k);
  
  for(Integer k = 0; k < (x-1); k = k + 1)
    dmem_e[k] = fromInteger(k + k + 1);
    
  dmem_e[x-1] = fromInteger(x-1);

  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i,
	   dmem_exp:  dmem_e
	 };
	 
endfunction

module [Module] testAddition_1();
  
  TOY_System th <- mkSystem_Simple(mkTOY_CPU, mkTOY_Mem)();

  Empty test <- mkController_Software_OneTest(th, testAddition(11, 5));
endmodule

module [Module] testSubtraction_1();
  
  TOY_System th <- mkSystem_Simple(mkTOY_CPU, mkTOY_Mem)();

  Empty test <- mkController_Software_OneTest(th, testSubtraction(11, 5));
endmodule
  

module [Module] testBranch_1 ();
  
  TOY_System th <- mkSystem_Simple(mkTOY_CPU, mkTOY_Mem)();

  Empty test <- mkController_Software_OneTest(th, testBranch(17, 12));
endmodule

module [Module] testStalls_1 ();
  
  TOY_System th <- mkSystem_Simple(mkTOY_CPU, mkTOY_Mem)();

  Empty test <- mkController_Software_OneTest(th, testStalls(17));
endmodule

