import hasim_common::*;
import soft_connections::*;
import front_panel::*;
import toplevel_wires::*;
import low_level_platform_interface::*;
import memory::*;
import rrr::*;

typedef struct
{
  Bit#(1) b_up;
  Bit#(1) b_down;
  Bit#(1) b_left;
  Bit#(1) b_right;
  Bit#(1) b_center;
}
  ButtonInfo deriving (Eq, Bits);

module [HASim_Module] mkPlatformInterface(TopLevelWires);

    // instantiate connections
    Connection_Receive#(Bit#(4))    link_leds       <- mkConnection_Receive("fpga_leds");
    Connection_Send#(Bit#(4))       link_switches   <- mkConnection_Send("fpga_switches");
    Connection_Send#(ButtonInfo)    link_buttons    <- mkConnection_Send("fpga_buttons");

    // Currently only one user can read and write memory
    Connection_Server#(MEM_Request, MEM_Value) link_memory       <- mkConnection_Server("vdev_memory");
    Connection_Send#(MEM_Addr)                 link_memory_inval <- mkConnection_Send("vdev_memory_invalidate");

    // direct RRR connection
    Connection_Server#(RRR_Request, RRR_Response) link_rrr <- mkConnection_Server("rrr_client");

    // state of RRR request
    Reg#(Bit#(1))   rrrState    <- mkReg(0);

    // instantiate low-level platform interface
    LowLevelPlatformInterface       llpint          <- mkLowLevelPlatformInterface();

    // instantiate virtual devices
    FrontPanel                      frontPanel      <- mkFrontPanel(llpint);

    Memory                          memory          <- mkMemory(llpint);
    
    // rules
    rule set_leds (True);
        Bit#(4) newval = link_leds.receive();
        link_leds.deq();

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

    rule send_mem_req (True);
      //Read in memory request and pass it on.
      //Eventually we'll have to arbitrate between different users
      let mreq = link_memory.getReq();
      link_memory.deq();
      
      memory.makeMemRequest(mreq);
    
    endrule
    
    rule send_mem_resp (True);
    
      //Read in mem resp and pass it on.
      //Eventually we'll have to figure out which user to send it to.
      let mrsp <- memory.getMemResponse();
      link_memory.makeResp(mrsp);
    
    endrule
    
    rule send_mem_inval (True);
    
      //Read the mem invalidates and pass them on.
      //This will ultimately be a broadcast to all users.
      let inval <- memory.getInvalidateRequest();
      link_memory_inval.send(inval);
    
    endrule

    // RRR link: we can only allow 1 outstanding request per link at
    // any instant
    rule send_rrr_req (rrrState == 0);

        // read in RRR request from connection and translate
        // it to a method call to RRR client
        let req = link_rrr.getReq();
        link_rrr.deq();

        llpint.rrrClient.makeRequest(req);

        if (req.needResponse == True)
            rrrState <= 1;
        else
            rrrState <= 0;

    endrule

    // RRR link: we should only try to obtain a response if we have
    // sent out a request
    rule send_rrr_resp (rrrState == 1);

        // call RRR client method to receive a response, and
        // send the result on the connection
        let resp <- llpint.rrrClient.getResponse();
        link_rrr.makeResp(resp);
        rrrState <= 0;

    endrule

    // return interface to top-level wires
    return llpint.topLevelWires.wires_out;

endmodule
