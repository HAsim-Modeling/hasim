import hasim_base::*;
import front_panel::*;

// FPGALib Software implementation as a virtual platform
interface TopLevel;
endinterface

module [HASim_Module] mkFPGALib (TopLevel);

    Connection_Receive#(Bit#(4))    link_leds <- mkConnection_Receive("fpga_leds");
    Connection_Send#(Bit#(4))       link_switches <- mkConnection_Send("fpga_switches");
    Connection_Send#(ButtonInfo)    link_buttons <- mkConnection_Send("fpga_buttons");

    FrontPanel  frontPanel <- mkFrontPanel();
  
    Reg#(Bit#(4)) led_reg <- mkReg(4'b0000);
    Reg#(Bit#(4)) switch_reg <- mkReg(4'b0000);
    Reg#(Bit#(1)) bu_reg <- mkReg(0);
    Reg#(Bit#(1)) bd_reg <- mkReg(0);
    Reg#(Bit#(1)) bl_reg <- mkReg(0);
    Reg#(Bit#(1)) br_reg <- mkReg(0);
    Reg#(Bit#(1)) bc_reg <- mkReg(0);
  
    rule set_leds (True);
        Bit#(4) newval <- link_leds.receive();
        led_reg <= newval;

        // ask front panel to display my current LED state
        frontPanel.writeLEDs(newval);
    endrule
  
    rule send_switches_and_buttons (True);
        // read in switch/button state from front panel
        Bit#(9) sbstate = frontPanel.readSwitches();
        switch_reg  <= sbstate[3:0];
        bu_reg      <= sbstate[4];
        bl_reg      <= sbstate[5];
        bc_reg      <= sbstate[6];
        br_reg      <= sbstate[7];
        bd_reg      <= sbstate[8];

        // send switch and button info over the connection
        ButtonInfo bi = ButtonInfo {
		                    b_up: bu_reg,
                            b_down: bd_reg, 
                            b_left: bl_reg,
                            b_right: br_reg,
		                    b_center: bc_reg
                        };
        link_switches.send(switch_reg);
        link_buttons.send(bi);
    endrule

endmodule