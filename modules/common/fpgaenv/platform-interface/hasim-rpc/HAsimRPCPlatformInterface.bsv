import hasim_common::*;
import front_panel::*;
import toplevel_wires::*;
import hardware_rpc::*;

module [HASim_Module] mkPlatformInterface(TopLevelWires);

    // instantiate connections
    Connection_Receive#(Bit#(4))    link_leds       <- mkConnection_Receive("fpga_leds");
    Connection_Send#(Bit#(4))       link_switches   <- mkConnection_Send("fpga_switches");
    Connection_Send#(ButtonInfo)    link_buttons    <- mkConnection_Send("fpga_buttons");

    // instantiate top-level wires
    TopLevelWiresDriver             wires           <- mkTopLevelWiresDriver();

    // instantiate RPC client
    RPCClient                       rpcClient       <- mkRPCClient();

    // instantiate virtual devices
    FrontPanel                      frontPanel      <- mkFrontPanel(rpcClient, wires);

    // rules
    rule set_leds (True);
        Bit#(4) newval <- link_leds.receive();

        // ask front panel to display my current LED state
        frontPanel.writeLEDs(newval);
    endrule
  
    rule send_switches (True);
        // read in switch state from front panel
        Bit#(4) sstate = frontPanel.readSwitches();

        // send switch info over the connection
        link_switches.send(sstate);
    endrule

    rule send_buttons (True);
        // read in button state from front panel
        Bit#(5) bstate = frontPanel.readButtons();
        ButtonInfo bi = ButtonInfo {
    	                    b_up: bstate[0],
                            b_down: bstate[4], 
                            b_left: bstate[1],
                            b_right: bstate[3],
                            b_center: bstate[2]
                        };

        // send button info over the connection
        link_buttons.send(bi);
    endrule

    // return interface to top-level wires
    return wires.wires_out;

endmodule
