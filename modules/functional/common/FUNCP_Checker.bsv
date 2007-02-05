
import FIFO::*;
import RegFile::*;
import PrimArray::*;

import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_funcp_base::*;
import hasim_isa::*;

import hasim_testcase_base::*;
import hasim_testcase::*;

interface Checker;

  method Action checkResult();
  method ActionValue#(Tuple3#(Addr, Value, Value)) getFailure();
  method Bool passed();
  method Bool done();

endinterface


module [HASim_Module] mkFUNCP_Checker
    //interface:
                (Checker);

  Connection_Client#(Addr, Value) magic_dmem <- mkConnection_Client("magic_dmem_read");
  
  let tc = test_case;

  Reg#(Bool) started <- mkReg(False);
  Reg#(Bool) passedR <- mkReg(True);
  
  Reg#(Addr) ecur <- mkReg(minBound);
  
  FIFO#(Tuple3#(CMD_Addr, CMD_Value, CMD_Value)) failQ <- mkFIFO();
  FIFO#(Addr) locQ <- mkFIFO();
  
  Bool d_checking = ecur < fromInteger(primArrayLength(tc.dmem_exp));
  
  rule check_dmem (started && d_checking);
  
    magic_dmem.makeReq(ecur);
    locQ.enq(ecur);

    ecur <= ecur + 1;
    
  endrule
  
  rule dmem_resp (True);
  
    Value v <- magic_dmem.getResp();
    let cur = locQ.first();
    locQ.deq();
    
    Value exp_v = tc.dmem_exp[cur];
     
    if (v != exp_v)
    begin
      debug(2, $display("Checker: Failure at Location %h: Expected %0h. Found %0h", cur, exp_v, v));
      failQ.enq(tuple3(zeroExtend(cur), zeroExtend(exp_v), zeroExtend(v)));
      passedR <= False;
    end
    
  endrule

  method Action checkResult();
  
    started <= True;
    
  endmethod
  
  method ActionValue#(Tuple3#(CMD_Addr, CMD_Value, CMD_Value)) getFailure();
  
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
