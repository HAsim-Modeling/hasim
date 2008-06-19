import Clocks::*;

`include "bluespec_system.bsh"
`include "fpgaenv.bsh"
`include "low_level_platform_interface.bsh"

module mkModel(TOP_LEVEL_WIRES);

    // expose current clock and reset
    Clock clock      <- exposeCurrentClock();
    Reset hard_reset <- exposeCurrentReset();

    // 0 is number of stages
    // False = do not start in reset
    MakeResetIfc soft_reset_wrapper <- mkReset(0, False, clock);

    // use mkResetEither as the output
    Reset new_reset <- mkResetEither(hard_reset, soft_reset_wrapper.new_rst);

    // instantiate LLPI and system with new reset

    // name must be pi_llpint --- explain!!!
    let pi_llpint <- mkLowLevelPlatformInterface(reset_by new_reset);
    let system    <- mkSystem(pi_llpint, reset_by new_reset);

    // create a rule to assert our generated reset
    rule assert_reset (True);
        
        // accept a reset request
        pi_llpint.physicalDrivers.soft_reset();        
        
        // blow up the entire model
        soft_reset_wrapper.assertReset();
        
    endrule

    // return top level wires interface
    return pi_llpint.topLevelWires;

endmodule
