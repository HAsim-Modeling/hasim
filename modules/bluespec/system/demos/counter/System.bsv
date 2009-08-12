import Counter::*;

`include "virtual_platform.bsh"
`include "virtual_devices.bsh"
`include "low_level_platform_interface.bsh"
`include "physical_platform.bsh"
`include "front_panel.bsh"

module mkApplication#(VIRTUAL_PLATFORM vp)();

    // instantiate virtual devices
    FrontPanel      fp      = vp.virtualDevices.frontPanel;

    Counter         counter <- mkCounter();
    Reg#(Bit#(16))  state   <- mkReg(0);


    rule step0(state == 0);
        Bit#(8) extended = zeroExtend(fp.readSwitches());
        counter.load(extended);
        state <= 1;
    endrule

    rule step1(state == 1);
        let value = counter.read();

        fp.writeLEDs(truncate(value), '1);
        state <= 2;
    endrule

    rule done(state == 2);
        state <= 0;
    endrule


endmodule
