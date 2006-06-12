//HASim library imports
import HASim::*;
import TestCase_Base::*;

//Model-specific imports
import Isa::*;

//This file provides the following HASim-required names:
//     Name:             Type:
//     -----             -----
//     test_case         TestCase

// A more complex, branching test
// Tests x * y without using Mul instruction

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

  Value dmem_i[1] = {0};
  Value dmem_e[1] = {fromInteger(x * y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i,
	   dmem_exp:  dmem_e
	 };
	 
endfunction

//Eventually these should be parameters
TestCase test_case = testBranch(17, 12));
