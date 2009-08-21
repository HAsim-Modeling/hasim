import soft_connections::*;

interface Calculation4x4;
   method Action start(Bit#(4) in);
   method Bit#(4) getResult();
endinterface: Calculation4x4

module [CONNECTED_MODULE] mkCalculation4x4(Calculation4x4);
   
  Reg#(Bit#(4))  data      <- mkReg(0);

  method Action start(Bit#(4) in);
     data <= in;
   endmethod

   method Bit#(4) getResult();
      return data; 
   endmethod

endmodule


