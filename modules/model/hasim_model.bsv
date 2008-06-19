import Clocks::*;

`include "hasim_common.bsh"
`include "soft_connections.bsh"
`include "hasim_controller.bsh"
`include "hasim_system.bsh"
`include "platform_interface.bsh"
`include "physical_platform.bsh"

module [HASim_Module] mkModel (TOP_LEVEL_WIRES);

    // expose current clock and reset
    Clock clock      <- exposeCurrentClock();
    Reset hard_reset <- exposeCurrentReset();

    // 0 is number of stages
    // False = do not start in reset
    MakeResetIfc soft_reset_wrapper <- mkReset(0, False, clock);

    // use mkResetEither as the output
    Reset new_reset <- mkResetEither(hard_reset, soft_reset_wrapper.new_rst);

    // instantiate system, controller and PI with new reset
    let system     <- mkSystem           (reset_by new_reset);
    let controller <- mkController       (reset_by new_reset);
    let pi         <- mkPlatformInterface(reset_by new_reset);
    
    // create a connection to receive reset requests
    Connection_Receive#(Bool) link_reset <- mkConnection_Receive("soft_reset", reset_by new_reset);
    
    // create a rule to assert our generated reset
    rule assert_reset (True);
        
        // accept a reset request
        link_reset.deq();
        
        // blow up the entire model
        soft_reset_wrapper.assertReset();
        
    endrule

    // return top level wires interface
    return pi;

endmodule
