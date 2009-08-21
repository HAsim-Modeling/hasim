import soft_connections::*;

interface Calculation4x4;
    method Action start(Bit#(4) in);
    method Bit#(4) getResult();
endinterface: Calculation4x4

module [CONNECTED_MODULE] mkCalculation4x4(Calculation4x4);
   
    Reg#(Bit#(8))  product      <- mkReg(0);
    Reg#(Bit#(8))  d            <- mkReg(0);
    Reg#(Bit#(4))  r            <- mkReg(0);   

    rule cycle (r != 0);
        if (r[0] == 1) product <= product + d;
        d <= d << 1;
        r <= r >> 1;
    endrule

    method Action start(Bit#(4) in) if (r == 0);
        Bit#(4) x = in;
     
        if (x == 0)
            begin
                d <= 0;
                r <= 0;
            end
        else
            begin
            d <= zeroExtend(x);
                r <= x; 
        end
        product <= 0;
   endmethod

   method Bit#(4) getResult() if (r == 0);
      return product[3:0];
   endmethod

endmodule


