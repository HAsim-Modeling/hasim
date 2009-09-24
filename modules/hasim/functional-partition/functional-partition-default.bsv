
// functional_partition

// Just instantiate all the submodules (currently just the register and memory state).

// Project foundation includes

`include "hasim_common.bsh"

// Functional Partition includes

`include "funcp_regstate.bsh"
`include "funcp_memstate.bsh"

// ISA includes

`include "hasim_isa_datapath.bsh"

// mkFunctionalPartition

// Instantiate the submodules

module [HASIM_MODULE] mkFuncp
    //interface:
         ();
    
    let regstate <- mkFUNCP_RegState();
    let memstate <- mkFUNCP_MemState();
    let datapath <- mkISA_Datapath();

endmodule
