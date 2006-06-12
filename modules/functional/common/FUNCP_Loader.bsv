
import RegFile::*;
import PrimArray::*;

import HASim::*;
import TestCase_Base::*;
import ISA::*;
import TestCase::*;


interface Loader;

  method Action loadProgram();
  method Bool done();

endinterface


module [HASim_Module] mkFUNCP_Loader#(RegFile#(Addr, Inst)  imem,
                                      RegFile#(Addr, Value) dmem)
    //interface:
                (Loader);

  let tc = test_case;

  Reg#(Bool) started <- mkReg(False);
  
  Reg#(Addr) icur <- mkReg(minBound);
  Reg#(Addr) dcur <- mkReg(minBound);
  
  Bool i_loading  = icur < fromInteger(primArrayLength(tc.imem_init));
  Bool d_loading  = dcur < fromInteger(primArrayLength(tc.dmem_init));
  
  Bool loading = i_loading || d_loading;
  
  //load_imem
  
  rule load_imem (started && i_loading);
  
    imem.upd(icur, primArrayDynamicSelect(tc.imem_init, icur));
    
    icur <= icur + 1;
    //$display("Loading IMem");
  
  endrule
  
  
  //load_dmem
  
  rule load_dmem (started && d_loading);
  
    dmem.upd(dcur, primArrayDynamicSelect(tc.dmem_init, dcur));
    
    dcur <= dcur + 1;
    //$display("Loading DMem");
  
  endrule
  
  
  method Action loadProgram();
  
    started <= True;
    
  endmethod
  
  method Bool done();
  
    return !loading;
  
  endmethod

endmodule
