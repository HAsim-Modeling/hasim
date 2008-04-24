import hasim_common::*;
import soft_connections::*;
import front_panel::*;
import physical_platform::*;
import low_level_platform_interface::*;
import rrr::*;

`include "funcp_memory.bsh" // FIXME: NEED THIS FOR TYPES, REMOVE
`include "scratchpad_memory.bsh"
`include "asim/rrr/server_connections.bsh"
`include "asim/rrr/client_connections.bsh"
`include "streams.bsh"

typedef struct
{
  Bit#(1) b_up;
  Bit#(1) b_down;
  Bit#(1) b_left;
  Bit#(1) b_right;
  Bit#(1) b_center;
}
  ButtonInfo deriving (Eq, Bits);

module [HASim_Module] mkPlatformInterface (TOP_LEVEL_WIRES);

    // instantiate connections
    Connection_Receive#(FRONTP_MASKED_LEDS) link_leds <- mkConnection_Receive("fpga_leds");
    Connection_Send#(FRONTP_SWITCHES) link_switches   <- mkConnection_Send("fpga_switches");
    Connection_Send#(ButtonInfo)      link_buttons    <- mkConnection_Send("fpga_buttons");

    // Currently only one user can read and write memory
    Connection_Server#(MEM_REQUEST, MEM_VALUE) link_memory       <- mkConnection_Server("vdev_memory");
    Connection_Send#(MEM_ADDRESS)              link_memory_inval <- mkConnection_Send("vdev_memory_invalidate");

    // other virtual devices
    Connection_Receive#(STREAMS_REQUEST) link_streams <- mkConnection_Receive("vdev_streams");

    // direct RRR links (TEMPORARY, these will be automatically generated in future)
    Connection_Receive#(RRR_Request) link_rrr_events  <- mkConnection_Receive("rrr_client_events");
    Connection_Receive#(RRR_Request) link_rrr_stats   <- mkConnection_Receive("rrr_client_stats");
    Connection_Receive#(RRR_Request) link_rrr_assertions <- mkConnection_Receive("rrr_client_assertions");
    Connection_Receive#(RRR_Request) link_rrr_sync <- mkConnection_Receive("rrr_client_sync");
    Connection_Receive#(RRR_Request) link_rrr_emulate <- mkConnection_Receive("rrr_client_emulate");

    // instantiate low-level platform interface
    LowLevelPlatformInterface       llpint          <- mkLowLevelPlatformInterface();

    // instantiate virtual devices
    FrontPanel   frontPanel <- mkFrontPanel(llpint);
    MEMORY_VIRTUAL_DEVICE  memory     <- mkMemoryVirtualDevice(llpint);
    Streams      streams    <- mkStreams(llpint);

    // connection terminus
    let t <- mkConnectionTerminus();

    // auto-generated submodules for RRR connections
    let rrr_server_links <- mkServerConnections(llpint.rrrServer);
    let rrr_client_links <- mkClientConnections(llpint.rrrClient);
    
    // rules
    rule set_leds (True);
        let newval = link_leds.receive();
        link_leds.deq();

        // ask front panel to display my current LED state
        frontPanel.writeLEDs(newval.state, newval.mask);
    endrule
  
    rule send_switches (True);
        // read in switch state from front panel
        FRONTP_SWITCHES sstate = frontPanel.readSwitches();

        // send switch info over the connection
        link_switches.send(sstate);
    endrule

    rule send_buttons (True);
        // read in button state from front panel
        FRONTP_BUTTONS bstate = frontPanel.readButtons();
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

    rule send_streams_req (True);

        // read in streams request and send it to device
        let sreq = link_streams.receive();
        link_streams.deq();
        streams.makeRequest(sreq.streamID,
                            sreq.stringID,
                            sreq.payload0,
                            sreq.payload1);

    endrule

    // direct RRR links (TEMPORARY, these will be automatically generated in future)
    // NOTE: When this is automatically generated we will need some kind of
    // dynamic fairness. For now we can use our high-level knowledge to give
    // a static urgency. Otherwise Events will starve everyone else.

    (* descending_urgency= "translate_rrr_assertions_req, translate_rrr_stats_req, translate_rrr_sync_req, translate_rrr_emulate_req, translate_rrr_events_req" *)

    rule translate_rrr_events_req (True);

        let req = link_rrr_events.receive();
        link_rrr_events.deq();
        llpint.oldrrrClient.makeRequest(req);

    endrule

    rule translate_rrr_stats_req (True);
    
        let req = link_rrr_stats.receive();
        link_rrr_stats.deq();
        llpint.oldrrrClient.makeRequest(req);
    
    endrule
    
    rule translate_rrr_assertions_req (True);
    
        let req = link_rrr_assertions.receive();
        link_rrr_assertions.deq();
        llpint.oldrrrClient.makeRequest(req);
    
    endrule

    rule translate_rrr_sync_req (True);
    
        let req = link_rrr_sync.receive();
        link_rrr_sync.deq();
        llpint.oldrrrClient.makeRequest(req);
    
    endrule

    rule translate_rrr_emulate_req (True);
    
        let req = link_rrr_emulate.receive();
        link_rrr_emulate.deq();
        llpint.oldrrrClient.makeRequest(req);
    
    endrule

    // return interface to top-level wires
    return llpint.topLevelWires;

endmodule
