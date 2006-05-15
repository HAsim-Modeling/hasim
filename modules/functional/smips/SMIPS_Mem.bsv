
import Connectable::*;

import HASim::*;
import Ports::*;
import Mem::*;

import TOY_Datatypes::*;
import TOY_FunctionalPartition::*;
import TOY_TimingPartition::*;

(* synthesize *)
module [Module] mkTOY_Mem (Memory#(TOY_Token, TOY_Addr, TOY_Inst, TOY_Value));

  let m <- mkMem_Software();
  
  return m;
  
endmodule
