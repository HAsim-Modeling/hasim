
// funcp_memstate_default

// Just instantiate all submodules (currently just the manager).

// Project foundation includes

`include "hasim_common.bsh"

// Project includes

`include "funcp_memstate_manager.bsh"
`include "funcp_memory.bsh"
`include "asim/provides/funcp_memory_tlb.bsh"


module [HASIM_MODULE] mkFUNCP_MemState
    // interface:
        ();

    let manager <- mkFUNCP_MemStateManager();
    let memory  <- mkFUNCP_Memory();
    let tlbs    <- mkFUNCP_CPU_TLBS();

endmodule
