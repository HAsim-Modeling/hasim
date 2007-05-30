import hasim_base::*;
import hasim_common::*;

import calculation4x4::*;

//module [HASim_Module] mkSystem (TModule#(Command, Response));
module [HASim_Module] mkSystem ();
   
    Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
    Connection_Receive#(ButtonInfo) link_buttons  <- mkConnection_Receive("fpga_buttons");
    Connection_Send#(Bit#(4))       link_leds     <- mkConnection_Send("fpga_leds");

    Reg#(Bit#(4))   state <- mkReg(0);

    Calculation4x4 func <- mkCalculation4x4();

    rule run(state == 0);
        Bit#(4) inp <- link_switches.receive();
        ButtonInfo btns <- link_buttons.receive();
        if (btns.b_center == 1)
        begin
            func.start(inp);
            state <= 1;
        end
   endrule
   
   rule finishUp(state == 1);
      Bit#(4) result = func.getResult();
      link_leds.send(result);
      state <= 2;
   endrule

endmodule
