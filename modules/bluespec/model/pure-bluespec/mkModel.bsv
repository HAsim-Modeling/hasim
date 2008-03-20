
`include "bluespec_system.bsh"
`include "fpgaenv.bsh"
`include "low_level_platform_interface.bsh"

module mkModel(TOP_LEVEL_WIRES);

    // Name must be pi_llpint --- explain!!!

    let pi_llpint   <- mkLowLevelPlatformInterface();
    let system   <- mkSystem(pi_llpint);

    return pi_llpint.topLevelWires;

endmodule

