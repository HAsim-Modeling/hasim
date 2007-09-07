import hasim_common::*;
import soft_connections::*;
import platform_interface::*;
import memory::*;

module [HASim_Module] mkSystem ();
   
    Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
    Connection_Receive#(ButtonInfo) link_buttons  <- mkConnection_Receive("fpga_buttons");
    Connection_Send#(Bit#(4))       link_leds     <- mkConnection_Send("fpga_leds");

    Connection_Client#(MEM_Request, MEM_Value) link_memory <- mkConnection_Client("vdev_memory");
    Connection_Receive#(MEM_Addr) link_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");

    Reg#(MEM_Addr) addr <- mkReg(0);
    Reg#(Bit#(2)) state <- mkReg(0);

    rule send(state == 0);
        link_memory.makeReq(tagged MEM_Load addr);
        state <= 1;
    endrule

    rule recv(state == 1);
        MEM_Value v = link_memory.getResp();
        link_memory.deq();
        if (v == 0)
            $finish(0);
        else
        begin
            $display("%c", v);
            state <= 0;
            addr <= addr + 1;
        end
    endrule

    rule sinkSwitches(True);
        link_switches.deq();
    endrule

    rule sinkButtons(True);
        link_buttons.deq();
    endrule

endmodule
