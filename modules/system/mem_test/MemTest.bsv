import hasim_common::*;
import soft_connections::*;
import platform_interface::*;

`include "scratchpad_memory.bsh"
`include "streams.bsh"

`define LAST_ADDR 'h2000

typedef enum
{
    STATE_ready,
    STATE_awaitingResponse,
    STATE_finished
}
STATE
    deriving (Bits, Eq);

module [HASim_Module] mkSystem ();

    Connection_Client#(MEM_Request, MEM_Value) link_memory <- mkConnection_Client("vdev_memory");
    Connection_Receive#(MEM_Addr)              link_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");
    Connection_Send#(STREAMS_REQUEST)          link_streams <- mkConnection_Send("vdev_streams");

    Reg#(Bit#(32)) cooldown <- mkReg(1000);
    Reg#(MEM_Addr) addr <- mkReg('h1000);
    Reg#(STATE)    state <- mkReg(STATE_ready);

    rule send(state == STATE_ready && addr != `LAST_ADDR);

        link_memory.makeReq(tagged MEM_Load addr);
        state <= STATE_awaitingResponse;

    endrule

    rule recv(state == STATE_awaitingResponse);

        MEM_Value v = link_memory.getResp();
        link_memory.deq();

        if (v != 0)
        begin
            link_streams.send(STREAMS_REQUEST { streamID: STREAMID_MEMTEST,
                                                stringID: STREAMS_MEMTEST_DATA,
                                                payload0: addr,
                                                payload1: v });
        end

        addr  <= addr + 4;
        state <= STATE_ready;

    endrule

    rule terminate (state != STATE_finished && addr == `LAST_ADDR);

        link_streams.send(STREAMS_REQUEST { streamID: STREAMID_MEMTEST,
                                            stringID: STREAMS_MEMTEST_DONE,
                                            payload0: ?,
                                            payload1: ? });
        state <= STATE_finished;

    endrule

    rule finishup (state == STATE_finished && cooldown != 0);
        cooldown <= cooldown - 1;
    endrule

    rule finished (state == STATE_finished && cooldown == 0);
        $finish(0);
    endrule

    rule accept_invalidates(True);

        MEM_Addr addr = link_memory_inval.receive();
        link_memory_inval.deq();
        link_streams.send(STREAMS_REQUEST { streamID: STREAMID_MEMTEST,
                                            stringID: STREAMS_MEMTEST_INVAL,
                                            payload0: addr,
                                            payload1: ? });

    endrule

endmodule
