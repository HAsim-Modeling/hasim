
`include "front_panel.bsh"
`include "physical_platform.bsh"
`include "virtual_platform.bsh"
`include "virtual_devices.bsh"

module mkApplication#(VIRTUAL_PLATFORM vp)();

    // instantiate virtual devices
    FrontPanel      fp      = vp.virtualDevices.frontPanel;

    rule switch_to_led (True);
        let value = fp.readSwitches();
        fp.writeLEDs(truncate(value), '1);
    endrule


endmodule
