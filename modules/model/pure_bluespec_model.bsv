import Clocks::*;

`include "bluespec_system.bsh"
`include "fpgaenv.bsh"
`include "low_level_platform_interface.bsh"
`include "clocks_device.bsh"

module mkModel(TOP_LEVEL_WIRES);

    // The Model is instantiated inside a NULL (noClock) clock domain,
    // so first instantiate the LLPI and get a clock and reset from it.

    // name must be pi_llpint --- explain!!!
    let pi_llpint <- mkLowLevelPlatformInterface();

    Clock clk = pi_llpint.physicalDrivers.clocksDriver.clock;
    Reset rst = pi_llpint.physicalDrivers.clocksDriver.reset;
    
    // instantiate system with new clock and reset
    let system <- mkSystem(pi_llpint, clocked_by clk, reset_by rst);
    
    // return top level wires interface
    return pi_llpint.topLevelWires;

endmodule
