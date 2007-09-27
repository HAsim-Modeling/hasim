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

    // direct RRR connections
    Connection_Server#(RRR_Request, RRR_Response) link_rrr_diov <- mkConnection_Server("rrr_client_diov");
    Connection_Receive#(RRR_Request) link_rrr_void <- mkConnection_Receive("rrr_client_void");

    // state of RRR request
    Reg#(Bit#(2))   rrrState    <- mkReg(0);

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

    // send non-void RRR request
    rule send_rrr_req_diov (rrrState == 0);

        // read in RRR request from connection and translate
        // it to a method call to RRR client
        let req = link_rrr_diov.getReq();
        link_rrr_diov.deq();

        llpint.rrrClient.makeRequest(req);

        // move to a state where we wait for a response
        rrrState <= 1;

    endrule

    // send void RRR request
    rule send_rrr_req_void (rrrState == 0);

        // read in RRR request from connection and translate
        // it to a method call to RRR client
        let req = link_rrr_void.receive();
        link_rrr_void.deq();

        llpint.rrrClient.makeRequest(req);

        // stay in the same state, we don't need a response
        rrrState <= 0;  

    endrule

    // wait for RRR response
    rule send_rrr_resp (rrrState == 1);
        let resp <- llpint.rrrClient.getResponse();
        link_rrr_diov.makeResp(resp);
        rrrState <= 0;
    endrule

    // return interface to top-level wires
    return llpint.topLevelWires.wires_out;

endmodule
