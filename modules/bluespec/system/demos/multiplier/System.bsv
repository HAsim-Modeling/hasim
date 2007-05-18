import Multiplier::*;
import front_panel::*;

(* synthesize *)
module mkSystem();

    /* state */
    Multiplier      mult        <- mkMultiplier();

    Reg#(Bit#(32))  result1     <- mkReg(0);
    Reg#(Bit#(32))  result2     <- mkReg(0);
    Reg#(Bit#(16))  state       <- mkReg(0);
    Reg#(Bit#(32))  switchLatch <- mkReg(0);

    FrontPanel      fp          <- mkFrontPanel();

    /* rules */
    rule latchSwitches(True);
        switchLatch <= fp.readSwitch();
    endrule: latchSwitches

    rule init(state == 0 && (switchLatch & 64) == 64);
        Bit#(32) a = switchLatch & 3;
        Bit#(32) b = (switchLatch & 12) >> 2;
        result1 <= mult.doComb(a, b);
        mult.load(a, b);
        mult.start();
        state <= 1;
    endrule: init

    rule waitForResult(state == 1 && mult.isResultReady() == True);
        result2 <= mult.getResult();
        state <= 2;
    endrule: waitForResult

    rule outputResult(state == 2);
        fp.writeLED(result2);
        state <= 3;
    endrule: outputResult

    rule reset(state == 3 && (switchLatch & 64) == 0);
        state <= 0;
    endrule

endmodule
