import HASim::*;
import ISA::*;

import Calculation4x4::*;

module [HASim_Module] mkSystem (TModule#(Command, Response));
   
   Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
   Connection_Receive#(ButtonInfo) link_buttons  <- mkConnection_Receive("fpga_buttons");
   Connection_Send#(Bit#(4))       link_leds     <- mkConnection_Send("fpga_leds");

   Calculation4x4 func <- mkCalculation4x4();
   
   rule run(True);
      Bit#(4) inp <- link_switches.receive();      
      func.start(inp);
   endrule
   
   rule finishUp;
      Bit#(4) result = func.getResult();
      link_leds.send(result);
   endrule

endmodule
