import hasim_common::*;
import soft_connections::*;
import platform_interface::*;

module [HASim_Module] mkSystem ();
   
  Connection_Receive#(Bit#(4)) link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(ButtonInfo) link_buttons  <- mkConnection_Receive("fpga_buttons");
  Connection_Send#(Bit#(4))    link_leds     <- mkConnection_Send("fpga_leds");

  Reg#(Bit#(2))  state        <- mkReg(0);
  Reg#(Bit#(4))  product      <- mkReg(0);
  Reg#(Bit#(4))  d            <- mkReg(0);
  Reg#(Bit#(2))  r            <- mkReg(0);

  rule start (state == 0);
     Bit#(4) inp = link_switches.receive();
     ButtonInfo btns = link_buttons.receive();
     link_switches.deq();
     link_buttons.deq();

     Bit#(2) x = inp[3:2];
     Bit#(2) y = inp[1:0];
     
     d <= zeroExtend(x); 
     r <= y; 
     product <= 0;
     
     if (btns.b_center == 1)
     begin
       state <= 1;
       $display("Starting 0x%h X 0x%h", x, y);
     end

  endrule

  rule cycle (state == 1);
   if (r[0] == 1) product <= product + d;
   $display("Running: Partial product = 0x%d", product);

   d <= d << 1;
   r <= r >> 1;

   if (r == 0) state <= 2;
  endrule


  rule finishUp (state == 2);
      
    link_leds.send(product);
    state <= 0;      

    $display("Finished: Product = 0x%h", product);
  endrule

endmodule
