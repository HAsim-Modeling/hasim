import hasim_base::*;
import hasim_common::*;

import calculation4x4::*;

//module [HASim_Module] mkSystem (TModule#(Command, Response));
module [HASim_Module] mkSystem ();
   
    Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
    Connection_Receive#(ButtonInfo) link_buttons  <- mkConnection_Receive("fpga_buttons");
    Connection_Send#(Bit#(4))       link_leds     <- mkConnection_Send("fpga_leds");

    Calculation4x4 func <- mkCalculation4x4();

    rule run(True);
        Bit#(4) inp <- link_switches.receive();
        ButtonInfo btns <- link_buttons.receive();
        func.start(inp);
    endrule

    rule disp(True);
        Bit#(4) result = func.getResult();
        link_leds.send(result);
    endrule

endmodule
