import mkCounter::*;
import front_panel::*;

(* synthesize *)
module mkSystem();

    Counter         counter <- mkCounter();
    Reg#(Bit#(16))  state   <- mkReg(0);

    FrontPanel      fp      <- mkFrontPanel();

    rule step0(state == 0);
        Bit#(8) truncated = truncate(fp.readSwitch());
        counter.load(truncated);
        state <= 1;
    endrule

    rule step1(state == 1);
        Bit#(32) extended = zeroExtend(counter.read());
        fp.writeLED(extended);
        state <= 2;
    endrule

    rule done(state == 2);
        state <= 0;
    endrule

endmodule
