import hasim_common::*;

module [HASim_Module] mkSystem ();
   
  Connection_Receive#(Bit#(4)) link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(ButtonInfo) link_buttons  <- mkConnection_Receive("fpga_buttons");
  Connection_Send#(Bit#(4))    link_leds     <- mkConnection_Send("fpga_leds");
   
  Reg#(Bit#(4))  product      <- mkReg(0);
  Reg#(Bit#(4))  d            <- mkReg(0);
  Reg#(Bit#(2))  r            <- mkReg(0);

  rule cycle (r != 0);
   if (r[0] == 1) product <= product + d;
   d <= d << 1;
   r <= r >> 1;
  endrule

  rule start (r == 0);
     Bit#(4) inp <- link_switches.receive();
     Bit#(2) x = inp[3:2];
     Bit#(2) y = inp[1:0];
     
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
     
  endrule
   
   rule finishUp (r == 0);
      
      link_leds.send(product);
      
   endrule

endmodule
