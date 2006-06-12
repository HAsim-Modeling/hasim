//HASim library imports
import HASim::*;
import TestCase_Base::*;

//Model-specific imports
import ISA::*;

//This file provides the following HASim-required names:
//     Name:             Type:
//     -----             -----
//     test_case         TestCase


// A branching test with data dependencies that ensures 
// we are stalling correctly

function TestCase#(Inst, Value) testStalls (Integer x);

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
    
  Value dmem_i[x];
  Value dmem_e[x];
  
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

//Eventually this could be set as a parameter
TestCase test_case = testStalls(17);
