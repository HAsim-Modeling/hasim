import HASim::*;
import ISA::*;


module [HASim_Module] mkSystem (TModule#(Command, Response));
 
  Connection_Send#(Bit#(4)) link_leds <- mkConnection_Send("fpga_leds");
  Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(ButtonInfo) link_buttons <- mkConnection_Receive("fpga_buttons");

  Reg#(Bit#(4))  product <- mkReg (0);
  Reg#(Bit#(2))  d       <- mkRegU();
  Reg#(Bit#(2))  r       <- mkReg(0);
  Reg#(Bool)     busy    <- mkReg(False);

  rule getInputs (!busy);
    match {.newd, .newr} <- con_switches.receive();
    
    if (newd == 0 || newr == 0)
      con_leds.send(4'b0000);
    else
    begin
      d <= newd; 
      r <= newr; 
      product <= 0;
      busy <= True;
    end
  endrule

  rule cycle (busy && r != 0);
    if (r[0] == 1) product <= product + zeroExtend(d);
    d <= d << 1;
    r <= r >> 1;
  endrule
  
  rule sendResult (busy && r == 0);
  
    con_leds.send(product);
    busy <= False;
  
  endrule

endmodule

