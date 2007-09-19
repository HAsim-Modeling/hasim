import hasim_common::*;
import soft_connections::*;
import platform_interface::*;
import memory::*;

module [HASim_Module] mkSystem ();
   
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
        $display("%8x: %8x", addr, v);
        if (addr <= 32000)
            addr <= addr + 1;
        else
            $finish(0);
        state <= 0;
    endrule

endmodule
