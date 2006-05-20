
import Connectable::*;

import HASim::*;
import Mem::*;

import TOY_Datatypes::*;
import TOY_FunctionalPartition::*;
import TOY_TimingPartition::*;
import TOY_Mem::*;


typedef System#(TOY_Tick, void, void, TOY_Addr, TOY_Inst, TOY_Value) TOY_System;

typedef CPU#(TOY_Tick, void, void) TOY_CPU;

//mkTOY_CPU :: CPU

module [Module] mkTOY_CPU (TOY_CPU);

  let fp <- mkTOY_FP();
  let tp <- mkTOY_TP_Simple();

  return tp;

endmodule


module [Module] mkTOY_System (TOY_System);

  TOY_CPU cpu <- mkTOY_CPU();
  TOY_Mem mem <- mkTOY_Mem();
  
  interface tmod = cpu;
  interface imem = mem.magic_imem;
  interface dmem = mem.magic_dmem;

endmodule

