import mkCounter::*;
import front_panel::*;
import physical_platform::*;

(* synthesize *)
module mkSystem(TOP_LEVEL_WIRES);

    Counter         counter <- mkCounter();
    Reg#(Bit#(16))  state   <- mkReg(0);
    
    PHYSICAL_PLATFORM pp    <- mkPhysicalPlatform();
    FrontPanel        fp    <- mkFrontPanel(pp.physicalDrivers);

    rule step0(state == 0);
        Bit#(8) extended = zeroExtend(fp.readSwitches());
        counter.load(extended);
        state <= 1;
    endrule

    rule step1(state == 1);
        Bit#(4) truncated = truncate(counter.read());
        fp.writeLEDs(truncated);
        state <= 2;
    endrule

    rule done(state == 2);
        state <= 0;
    endrule

    return pp.topLevelWires;

endmodule
