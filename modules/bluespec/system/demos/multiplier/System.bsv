
`include "virtual_platform.bsh"
`include "virtual_devices.bsh"
`include "physical_platform.bsh"
`include "front_panel.bsh"

import Multiplier::*;


module mkApplication#(VIRTUAL_PLATFORM vp)();

    // instantiate virtual devices
    FrontPanel      fp = vp.virtualDevices.frontPanel;

    /* state */
    Multiplier      mult        <- mkMultiplier();

    Reg#(Bit#(32))  result      <- mkReg(0);
    Reg#(Bit#(16))  state       <- mkReg(0);
    Reg#(Bit#(32))  in1         <- mkReg(0);
    Reg#(Bit#(32))  in2         <- mkReg(0);
    Reg#(Bit#(1))   go          <- mkReg(0);

    /* rules */
    rule latchSwitches(True);
        Bit#(4) switchVector = fp.readSwitches();
        Bit#(5) buttonVector = fp.readButtons();

        in1 <= zeroExtend(switchVector[1:0]);
        in2 <= zeroExtend(switchVector[3:2]);

        go  <= buttonVector[2];
    endrule: latchSwitches

    rule init(state == 0 && go == 1);
        mult.load(in1, in2);
        mult.start();
        state <= 1;
    endrule: init

    rule waitForResult(state == 1 && mult.isResultReady() == True);
        result <= mult.getResult();
        state <= 2;
    endrule: waitForResult

    rule outputResult(state == 2);
        fp.writeLEDs(truncate(result),'1);

        state <= 3;
    endrule: outputResult

    rule reset(state == 3 && go == 0);
        state <= 0;
    endrule

endmodule
