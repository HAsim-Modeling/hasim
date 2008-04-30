import hasim_common::*;
import soft_connections::*;
import funcp_memory::*;
import rrr::*;

`include "streams.bsh"

module [HASim_Module] mkConnectionTerminus();

    if (`TERM_FPGA_SWITCHES == 1)
        Connection_Receive#(FRONTP_SWITCHES) link_switches <- mkConnection_Receive("fpga_switches");

    if (`TERM_FPGA_BUTTONS == 1)
        Connection_Receive#(ButtonInfo) link_buttons <- mkConnection_Receive("fpga_buttons");

    if (`TERM_FPGA_LEDS == 1)
        Connection_Send#(FRONTP_MASKED_LEDS) link_leds <- mkConnection_Send("fpga_leds");

    if (`TERM_VDEV_MEMORY == 1)
        Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE) link_memory <- mkConnection_Client("vdev_memory");

    if (`TERM_VDEV_MEMORY_INVALIDATE == 1)
        Connection_Receive#(SCRATCHPAD_MEM_ADDRESS) link_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");

    if (`TERM_VDEV_STREAMS == 1)
        Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    // direct RRR links (TEMPORARY, these will be automatically generated in future)
    if (`TERM_RRR_CLIENT_EVENTS == 1)
        Connection_Send#(RRR_Request) link_rrr_events <- mkConnection_Send("rrr_client_events");
        
    if (`TERM_RRR_CLIENT_STATS == 1) 
        Connection_Send#(RRR_Request) link_rrr_stats <- mkConnection_Send("rrr_client_stats");
        
    if (`TERM_RRR_CLIENT_ASSERTIONS == 1)
        Connection_Send#(RRR_Request) link_rrr_assertions <- mkConnection_Send("rrr_client_assertions");
    
    if (`TERM_RRR_CLIENT_SYNC == 1)
        Connection_Send#(RRR_Request) link_rrr_sync <- mkConnection_Send("rrr_client_sync");
                                                                                            
    if (`TERM_RRR_CLIENT_EMULATE == 1)
        Connection_Send#(RRR_Request) link_rrr_emulate <- mkConnection_Send("rrr_client_emulate");
endmodule
