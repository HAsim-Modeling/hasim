import soft_connections::*;

interface Calculation4x4;
   method Action start(Bit#(4) in);
   method Bit#(4) getResult();
endinterface: Calculation4x4

module [CONNECTED_MODULE] mkCalculation4x4(Calculation4x4);
   
  Reg#(Bit#(2))  d            <- mkReg(0);
  Reg#(Bit#(2))  r            <- mkReg(0);   

  method Action start(Bit#(4) in);
     Bit#(2) x = in[3:2];
     Bit#(2) y = in[1:0];
     d <= x; 
     r <= y; 
   endmethod

   method Bit#(4) getResult();
      return zeroExtend(d) + zeroExtend(r);
   endmethod

endmodule


