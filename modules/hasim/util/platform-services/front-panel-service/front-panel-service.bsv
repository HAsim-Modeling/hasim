
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/front_panel.bsh"

`include "asim/provides/soft_connections.bsh"



module [CONNECTED_MODULE] mkFrontPanelService#(VIRTUAL_DEVICES vdevs)
    // interface:
        ();

    
    // Get a link to the real front panel.
    FRONT_PANEL frontPanel = vdevs.frontPanel;

    // Optional connections applications can use to read/write the 
    // front panel.
    Connection_Receive#(FRONTP_MASKED_LEDS) linkLEDs     <- mkConnectionRecvOptional("fpga_leds");
    Connection_Send#(FRONTP_SWITCHES)       linkSwitches <- mkConnectionSendOptional("fpga_switches");
    Connection_Send#(FRONTP_BUTTON_INFO)    linkButtons  <- mkConnectionSendOptional("fpga_buttons");

    // ******* Rules *******
    
    rule setLEDs (True);
        let newval = linkLEDs.receive();
        linkLEDs.deq();

        // ask front panel to display my current LED state
        frontPanel.writeLEDs(newval.state, newval.mask);

    endrule
  
    rule sendSwitches (True);
        // read in switch state from front panel
        FRONTP_SWITCHES sstate = frontPanel.readSwitches();

        // send switch info over the connection
        linkSwitches.send(sstate);
    endrule

    rule sendButtons (True);
        // read in button state from front panel
        FRONTP_BUTTONS bstate = frontPanel.readButtons();
        FRONTP_BUTTON_INFO bi = FRONTP_BUTTON_INFO 
        {
            bUp:     bstate[0],
            bDown:   bstate[4], 
            bLeft:   bstate[1],
            bRight:  bstate[3],
            bCenter: bstate[2]
        };

        // send button info over the connection
        linkButtons.send(bi);
    endrule
    

endmodule
