import HASim::*;

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import Connectable::*;

interface TestHarness#(type addr_T, type inst_T, type value_T);

  //Magic memory interface
  interface RegFile#(addr_T, inst_T)  imem;
  interface RegFile#(addr_T, value_T) dmem;
  
  //CPU interface for controller
  interface Put#(void) start;
  interface Get#(Bool) done;
endinterface

//Given a CPU module, and a memory module, make a test harness

module [Module] mkTestHarness#(function Module#(CPU#(token_T, addr_T, inst_T, value_T)) mkCPU(),
			       function Module#(Memory#(token_T, addr_T, inst_T, value_T)) mkMem())
    //interface:
                (TestHarness#(addr_T, inst_T, value_T));
    
    Memory#(token_T, addr_T, inst_T, value_T) mem <- mkMem();
    
    CPU#(token_T, addr_T, inst_T, value_T) dut <- mkCPU();
    
    //Connect up the CPU and the Memory
    
    //Connect the CPU to the memory

    mkConnection(dut.to_imem,   mem.imem);
    mkConnection(dut.to_dmem,   mem.dmem);
    mkConnection(dut.commit,    mem.commit);
    mkConnection(dut.killRange, mem.killRange);
  
    //Expose the interfaces to the Controller
    
    interface imem = mem.magic_imem;
    interface dmem = mem.magic_dmem;
    interface start = dut.start;
    interface done = dut.done;

endmodule
