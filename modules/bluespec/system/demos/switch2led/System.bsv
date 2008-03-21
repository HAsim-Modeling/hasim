
`include "front_panel.bsh"
`include "physical_platform.bsh"
`include "low_level_platform_interface.bsh"

module mkSystem#(LowLevelPlatformInterface llpi)();

    // instantiate virtual devices
    FrontPanel      fp      <- mkFrontPanel(llpi);

    rule switch_to_led (True);
        let value = fp.readSwitches();
        fp.writeLEDs(FRONTP_MASKED_LEDS {state: truncate(value), mask: '1});
    endrule


endmodule
