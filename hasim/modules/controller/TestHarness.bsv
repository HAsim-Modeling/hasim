import HASim::*;

interface TestHarness#(addr_T, inst_T, val_T);

  interface RegFile#(addr_T, inst_T) imem;
  interface RegFile#(addr_T, val_T) dmem;
  interface CPU cpu;
  
endinterface

//Given a CPU module, and a memory module, make a simple controller

module [Module] mkTestHarness#(function Module#(CPU) 
                               mkCPU(Memory#(addr_T, inst_T, val_T, token_T) m),
			       function Mem#(addr_T, inst_T, val_T, token_T) mkMem()) 
  (TestHarness#(addr_T, inst_T, val_T));
    
    let mem <- mkMem();
    
    CPU dut <- mkCPU(mem);
    
    interface imem = mem.magic_imem;
    interface dmem = mem.magic_dmem;
    interface cpu = dut;

endmodule
