
import Connectable::*;

import HASim::*;
import Ports::*;

import TOY_Datatypes::*;
import TOY_FunctionalPartition::*;
import TOY_TimingPartition::*;
import TOY_Mem::*;

//Instantiations of these tests with specific timing partitions

//mkTOY_CPU :: CPU

(* synthesize *)
module [Module] mkTOY_CPU (CPU#(TOY_Token, TOY_Addr, TOY_Inst, TOY_Value));

  let fp <- mkTOY_FP();
  let tp <- mkTOY_TP_Simple();
  
  //Connect up the partitions
  mkConnection(tp.tokgen,        fp.tokgen);
  mkConnection(tp.fetch,         fp.fetch);
  mkConnection(tp.decode,        fp.decode);
  mkConnection(tp.execute,       fp.execute);
  mkConnection(tp.memory,        fp.memory);
  mkConnection(tp.local_commit,  fp.local_commit);
  mkConnection(tp.global_commit, fp.global_commit);
  
  //Interface for Test Harness
  
  interface start = tp.start;
  interface done  = tp.done;

  //Interface For Memory Ports
  
  interface to_dmem = fp.to_dmem;
  interface to_imem = fp.to_imem;
  
  interface commit    = fp.commit;
  interface killRange = fp.killRange; 
endmodule

