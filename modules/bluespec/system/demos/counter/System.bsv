import mkCounter::*;
import front_panel::*;
import toplevel_wires::*;

(* synthesize *)
module mkSystem();

    Counter         counter <- mkCounter();
    Reg#(Bit#(16))  state   <- mkReg(0);
    
    TopLevelWires   wires   <- mkTopLevelWires();
    FrontPanel      fp      <- mkFrontPanel(wires);

    rule step0(state == 0);
        Bit#(8) truncated = truncate(fp.readSwitches());
        counter.load(truncated);
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

endmodule
