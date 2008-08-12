`include "hasim_common.bsh"
`include "hasim_funcp.bsh"
`include "hasim_chip.bsh"
`include "hasim_shared_cache.bsh"
`include "hasim_memory.bsh"

module [HASIM_MODULE] mkSystem();
    let funcp <- mkFuncp;
    let chip <- mkChip;
    let memory <- mkMemory;
endmodule
