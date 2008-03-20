interface Counter;
    method Bit#(8) read();
    method Action load(Bit#(8) newval);
    method Action increment();
endinterface

module mkCounter(Counter);

    Reg#(Bit#(8)) value <- mkReg(0);

    method Bit#(8) read();
        return value;
    endmethod

    method Action load(Bit#(8) newval);
        value <= newval;
    endmethod

    method Action increment();
        value <= value + 1;
    endmethod

endmodule
