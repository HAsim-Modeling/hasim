import hasim_common::*;
import soft_connections::*;
import memory::*;
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
        Connection_Client#(MEM_Request, MEM_Value) link_memory <- mkConnection_Client("vdev_memory");

    if (`TERM_VDEV_MEMORY_INVALIDATE == 1)
        Connection_Receive#(MEM_Addr) link_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");

    if (`TERM_VDEV_STREAMS == 1)
        Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    // direct RRR links (TEMPORARY, these will be automatically generated in future)
    if (`TERM_RRR_CLIENT_STARTER == 1)
        Connection_Send#(RRR_Request) link_rrr <- mkConnection_Send("rrr_client_starter");

endmodule
