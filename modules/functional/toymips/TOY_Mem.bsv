
import Connectable::*;

import HASim::*;
import Mem::*;

import TOY_Datatypes::*;
import TOY_FunctionalPartition::*;
import TOY_TimingPartition::*;


typedef Memory#(TOY_Token, TOY_Addr, TOY_Inst, TOY_Value) TOY_Mem;

module [HASim_Module] mkTOY_Mem (TOY_Mem);

  let m <- mkMem_Software();
  
  return m;
  
endmodule
