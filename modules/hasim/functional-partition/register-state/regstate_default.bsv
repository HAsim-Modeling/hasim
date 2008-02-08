
// funcp_regstate_default

// Just instantiate all submodules (currently just the manager).

// Project foundation includes

`include "hasim_common.bsh"

// Project includes

`include "funcp_regstate_manager.bsh"

module [HASIM_MODULE] mkFUNCP_RegState
    // interface:
        ();

    let manager <- mkFUNCP_RegStateManager();

endmodule
