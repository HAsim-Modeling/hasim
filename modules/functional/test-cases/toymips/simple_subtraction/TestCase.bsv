//HASim library imports
import HASim::*;
import TestCase_Base::*;

//Model-specific imports
import ISA::*;

//This file provides the following HASim-required names:
//     Name:             Type:
//     -----             -----
//     test_case         TestCase

// A simple test of x - y

function TestCase#(Inst, Value) testSubtraction (Integer x, Integer y);

  
  Inst prog[5] = 
    {
      ILoad  {dest: r1, idx:  r0, offset: 0  },
      ILoad  {dest: r2, idx:  r0, offset: 1  },
      ISub   {dest: r4, src1: r1, src2:   r2 },
      IStore {src:  r4, idx:  r0, offset: 2  },
      ITerminate
    };

  Value dmem_i[2] = {fromInteger(x), fromInteger(y)};
  Value dmem_e[3] = {fromInteger(x), fromInteger(y), fromInteger(x - y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i, 
	   dmem_exp:  dmem_e
	 };
	 
endfunction

//Eventually these should be parameters
TestCase test_case = testSubtraction(11, 5);
