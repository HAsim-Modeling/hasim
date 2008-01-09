import hasim_common::*;
import soft_connections::*;
import platform_interface::*;
import memory::*;

typedef enum
{
    STATE_ready,
    STATE_awaitingResponse
}
STATE
    deriving (Bits, Eq);

module [HASim_Module] mkSystem ();

    Connection_Client#(MEM_Request, MEM_Value) link_memory <- mkConnection_Client("vdev_memory");
    Connection_Receive#(MEM_Addr) link_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");

    Reg#(MEM_Addr) addr <- mkReg('h1000);
    Reg#(STATE)    state <- mkReg(STATE_ready);

    rule send(state == STATE_ready);

        link_memory.makeReq(tagged MEM_Load addr);
        state <= STATE_awaitingResponse;

    endrule

    rule recv(state == STATE_awaitingResponse);

        MEM_Value v = link_memory.getResp();
        link_memory.deq();

        if (v != 0)
        begin
            $display("%8x: %8x", addr, v);
            $fflush(stdout);
        end

        if (addr <= 'h2000)
            addr <= addr + 1;
        else
        begin
            $display("memtest: done");
            $finish(0);
        end

        state <= STATE_ready;

    endrule

    rule accept_invalidates(True);

        MEM_Addr addr = link_memory_inval.receive();
        link_memory_inval.deq();
        $display("INVALIDATE %8x", addr);
        $fflush(stdout);

    endrule

endmodule
