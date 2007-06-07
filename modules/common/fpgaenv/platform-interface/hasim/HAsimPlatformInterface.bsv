import hasim_base::*;
import front_panel::*;
import toplevel_wires::*;

module [HASim_Module] mkPlatformInterface(TopLevelWires);

    // instantiate connections
    Connection_Receive#(Bit#(4))    link_leds       <- mkConnection_Receive("fpga_leds");
    Connection_Send#(Bit#(4))       link_switches   <- mkConnection_Send("fpga_switches");
    Connection_Send#(ButtonInfo)    link_buttons    <- mkConnection_Send("fpga_buttons");

    // instantiate top-level wires
    TopLevelWiresDriver             wires           <- mkTopLevelWiresDriver();

    // instantiate virtual devices
    FrontPanel                      frontPanel      <- mkFrontPanel(wires);

    // rules
    rule set_leds (True);
        Bit#(4) newval <- link_leds.receive();

        // ask front panel to display my current LED state
        frontPanel.writeLEDs(newval);
    endrule
  
    rule send_switches_and_buttons (True);
        // read in switch/button state from front panel
        Bit#(9) sbstate = frontPanel.readSwitches();

        // send switch and button info over the connection
        ButtonInfo bi = ButtonInfo {
    	                    b_up: sbstate[4],
                            b_down: sbstate[8], 
                            b_left: sbstate[5],
                            b_right: sbstate[7],
                            b_center: sbstate[6]
                        };
        link_switches.send(sbstate[3:0]);
        link_buttons.send(bi);
    endrule

    // return interface to top-level wires
    return wires.wires_out;

endmodule
