import Counter::*;

`include "memory.bsh"
`include "front_panel.bsh"
`include "physical_platform.bsh"
`include "low_level_platform_interface.bsh"

module mkSystem#(LowLevelPlatformInterface llpi)();

    // instantiate virtual devices
    FrontPanel      fp      <- mkFrontPanel(llpi);

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
