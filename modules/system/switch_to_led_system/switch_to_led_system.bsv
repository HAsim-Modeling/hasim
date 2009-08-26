`include "soft_connections.bsh"
`include "front_panel_service.bsh"

import calculation4x4::*;

module [CONNECTED_MODULE] mkSystem ();
   
    Connection_Send#(FRONTP_MASKED_LEDS)    link_leds     <- mkConnection_Send("fpga_leds");
    Connection_Receive#(FRONTP_SWITCHES)    link_switches <- mkConnection_Receive("fpga_switches");
    Connection_Receive#(FRONTP_BUTTON_INFO) link_buttons  <- mkConnection_Receive("fpga_buttons");

    Calculation4x4 func <- mkCalculation4x4();

    rule run(True);
        FRONTP_SWITCHES sw = link_switches.receive();
        Bit#(4) inp = sw[3:0];  // this assumes FRONTP_SWITCHES has at least 4 bits
        FRONTP_BUTTON_INFO btns = link_buttons.receive();
        link_switches.deq();
        link_buttons.deq();
        func.start(inp);
    endrule

    rule disp(True);
        Bit#(4) result = func.getResult();
        FRONTP_LEDS ledstate = zeroExtend(result);
        link_leds.send(FRONTP_MASKED_LEDS{ state: ledstate, mask: 'b1111 });
    endrule

endmodule
