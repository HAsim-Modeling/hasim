import HASim::*;
import Mem::*;

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import Connectable::*;

//The simplest system is a CPU and a memory

module [HASim_Module] mkSystem_Simple#(function HASim_Module#(CPU#(tick_T, command_T, result_T)) mkCPU(),
			               function HASim_Module#(Memory#(token_T, addr_T, inst_T, value_T)) mkMem())
    //interface:
                (System#(tick_T, command_T, result_T, addr_T, inst_T, value_T));
    
    Memory#(token_T, addr_T, inst_T, value_T) mem <- mkMem();
    
    CPU#(tick_T, command_T, result_T) cpu <- mkCPU();
    
    //Expose the interfaces to the Controller
    
    interface imem = mem.magic_imem;
    interface dmem = mem.magic_dmem;
    interface tmod = cpu;

endmodule
