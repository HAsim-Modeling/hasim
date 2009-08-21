import soft_connections::*;

interface Calculation4x4;
   method Action start(Bit#(4) in);
   method Bit#(4) getResult();
endinterface: Calculation4x4

module [CONNECTED_MODULE] mkCalculation4x4(Calculation4x4);
   
    Reg#(Bit#(4))  product      <- mkReg(0);
    Reg#(Bit#(4))  d            <- mkReg(0);
    Reg#(Bit#(2))  r            <- mkReg(0);   

    rule cycle (r != 0);
        product <= (r[0] == 1) ? (product + d) : product;
        d <= d << 1;
        r <= r >> 1;
    endrule

    method Action start(Bit#(4) in) if (r == 0);
        Bit#(2) x = in[3:2];
        Bit#(2) y = in[1:0];
     
        if (x == 0 || y == 0)
            begin
                d <= 0;
                r <= 0;
            end
        else
            begin
            d <= zeroExtend(x);
            r <= y; 
        end
        product <= 0;
    endmethod

    method Bit#(4) getResult() if (r == 0);
        return product;
    endmethod

endmodule
