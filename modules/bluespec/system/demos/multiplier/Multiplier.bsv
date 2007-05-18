interface Multiplier;
    method Bit#(32) doComb(Bit#(32) a, Bit#(32) b);
    method Action   load(Bit#(32) a, Bit#(32) b);
    method Action   start();
    method Bool     isResultReady();
    method Bit#(32) getResult();
endinterface: Multiplier

module mkMultiplier(Multiplier);

    Reg#(Bit#(32))   mcand    <- mkReg(0);
    Reg#(Bit#(32))   mer      <- mkReg(0);
    Reg#(Bit#(32))   product  <- mkReg(0);

    Reg#(Bool)       processing <- mkReg(False);
    Reg#(Bool)       ready      <- mkReg(False);

    rule shiftAndAdd(processing == True && mer != 0);
        if (mer[0] == 1)
            product <= product + mcand;
        mcand <= mcand << 1;
        mer   <= mer   >> 1;
    endrule: shiftAndAdd

    rule detectEnd(processing == True && mer == 0);
        processing <= False;
        ready      <= True;
    endrule: detectEnd

    method Bit#(32) doComb(Bit#(32) a, Bit#(32) b);
        Bit#(32) c = a * b;
        return c;
    endmethod: doComb

    method Action load(Bit#(32) a, Bit#(32) b);
        mcand <= a;
        mer   <= b;
    endmethod: load

    method Action start();
        processing <= True;
        ready      <= False;
        product    <= 0;
    endmethod: start

    method Bool isResultReady();
        return ready;
    endmethod: isResultReady

    method Bit#(32) getResult();
        return product;
    endmethod: getResult

endmodule: mkMultiplier
