
// timing partition

// Project Foundation includes

`include "asim/provides/hasim_common.bsh"

// Timing Partition includes

`include "asim/provides/hasim_chip.bsh"
`include "asim/provides/hasim_memory.bsh"


// mkTimingPartition

// The single-chip timing partition just instantiates a chip.

module [HASIM_MODULE] mkTimingPartition
    //interface:
         ();
    
    let chip <- mkChip();
    let memory <- mkMemory();

endmodule
