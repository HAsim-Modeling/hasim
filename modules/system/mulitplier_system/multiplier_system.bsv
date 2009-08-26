
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/front_panel_service.bsh"

module [CONNECTED_MODULE] mkSystem ();
   
  Connection_Receive#(FRONTP_SWITCHES) link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(FRONTP_BUTTON_INFO) link_buttons  <- mkConnection_Receive("fpga_buttons");
  Connection_Send#(FRONTP_MASKED_LEDS)    link_leds     <- mkConnection_Send("fpga_leds");

  Reg#(Bit#(2))  state        <- mkReg(0);
  Reg#(Bit#(4))  product      <- mkReg(0);
  Reg#(Bit#(4))  d            <- mkReg(0);
  Reg#(Bit#(2))  r            <- mkReg(0);

  rule start (state == 0);
     Bit#(4) inp = link_switches.receive();
     FRONTP_BUTTON_INFO btns = link_buttons.receive();
     link_switches.deq();
     link_buttons.deq();

     Bit#(2) x = inp[3:2];
     Bit#(2) y = inp[1:0];
     
     d <= zeroExtend(x); 
     r <= y; 
     product <= 0;
     
     if (btns.bCenter == 1)
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
    
    let ledValue = FRONTP_MASKED_LEDS
    {
        state: zeroExtend(product),
        mask: ~0
    };
    link_leds.send(ledValue);
    state <= 0;      

    $display("Finished: Product = 0x%h", product);
  endrule

endmodule
