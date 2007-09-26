import hasim_common::*;
import soft_connections::*;
import memory::*;
import platform_interface::*;
import rrr::*;

module [HASim_Module] mkConnectionTerminus();

    if (`TERM_FPGA_SWITCHES == 1)
        Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");

    if (`TERM_FPGA_BUTTONS == 1)
        Connection_Receive#(ButtonInfo) link_buttons  <- mkConnection_Receive("fpga_buttons");

    if (`TERM_FPGA_LEDS == 1)
        Connection_Send#(Bit#(4))       link_leds     <- mkConnection_Send("fpga_leds");

    if (`TERM_VDEV_MEMORY == 1)
        Connection_Client#(MEM_Request, MEM_Value) link_memory <- mkConnection_Client("vdev_memory");

    if (`TERM_VDEV_MEMORY_INVALIDATE == 1)
        Connection_Receive#(MEM_Addr) link_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");

    if (`TERM_RRR_CLIENT == 1)
        Connection_Client#(RRR_Request, RRR_Response) link_rrr <- mkConnection_Client("rrr_client");

endmodule
