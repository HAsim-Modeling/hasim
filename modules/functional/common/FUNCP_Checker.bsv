
import FIFO::*;
import RegFile::*;
import PrimArray::*;

import HASim::*;
import ISA::*;
import TestCase_Base::*;
import TestCase::*;

interface Checker;

  method Action checkResult();
  method ActionValue#(Tuple3#(Addr, Value, Value)) getFailure();
  method Bool passed();
  method Bool done();

endinterface


module [HASim_Module] mkFUNCP_Checker#(RegFile#(Addr, Value) dmem)
    //interface:
                (Checker);

  let tc = test_case;

  Reg#(Bool) started <- mkReg(False);
  Reg#(Bool) passedR <- mkReg(True);
  
  Reg#(Addr) ecur <- mkReg(minBound);
  
  FIFO#(Tuple3#(Addr, Value, Value)) failQ <- mkFIFO();
  
  Bool d_checking = ecur < fromInteger(primArrayLength(tc.dmem_exp));
  
  rule check_dmem (started && d_checking);
  
    Value v = dmem.sub(ecur);
    Value exp_v = primArrayDynamicSelect(tc.dmem_exp, ecur);
     
    if (v != exp_v)
    begin
      failQ.enq(tuple3(ecur, exp_v, v));
      passedR <= False;
    end
    
    ecur <= ecur + 1;
    
  endrule

  method Action checkResult();
  
    started <= True;
    
  endmethod
  
  method ActionValue#(Tuple3#(Addr, Value, Value)) getFailure();
  
    failQ.deq();
    return failQ.first();
   
  endmethod
  
  method Bool passed() if (started && !d_checking);
  
    return passedR;
  
  endmethod

  method Bool done() if (started);
  
    return !d_checking;
    
  endmethod

endmodule
