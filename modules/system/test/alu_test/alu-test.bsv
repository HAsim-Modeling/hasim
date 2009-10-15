//
// Copyright (C) 2008 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

//
// This module isn't meant to be especially pretty.  It is a simple test harness
// for testing arithmetic operations.  The test reads in a series of 64 bit
// number pairs and passes them to a calculate rule.
//
//                            * * * * * * * * * * *
// There are benchmarks under hasim/demos/test that provide data inputs for
// this test harness.
//                            * * * * * * * * * * *
//

import Vector::*;

`include "asim/provides/fpga_components.bsh"

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"
`include "asim/provides/scratchpad_memory_service.bsh"
`include "asim/provides/scratchpad_memory.bsh"

`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS_ALUTEST.bsh"
`include "asim/dict/STREAMS_MESSAGE.bsh"

`define LAST_ADDR 'h2000

typedef enum
{
    STATE_ready,
    STATE_awaitingResponse,
    STATE_finished,
    STATE_calc_start,
    STATE_calc_end,
    STATE_calc_end1
}
STATE
    deriving (Bits, Eq);


module [CONNECTED_MODULE] mkSystem ();

    Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE) link_memory <- mkConnection_Client("vdev_memory");
    Connection_Receive#(SCRATCHPAD_MEM_ADDRESS) link_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");
    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    Reg#(Bit#(32)) cooldown <- mkReg(1000);
    Reg#(SCRATCHPAD_MEM_ADDRESS) addr <- mkReg('h1000);
    Reg#(STATE) state <- mkReg(STATE_ready);
    Reg#(Bit#(2)) pos <- mkReg(0);

    Reg#(Bit#(64)) arg0 <- mkReg(0);
    Reg#(Bit#(64)) arg1 <- mkReg(0);


    // ====================================================================
    //
    // Calculate function.  Test harness sets two 64 bit arguments (arg0
    // and arg1) and sets the state to STATE_calc_start.  Once done the
    // calculator should set the state to STATE_ready.
    //
    // ====================================================================

    HASIM_COMPACT_MUL#(64) uMul <- mkCompactUnsignedMul();

    rule calc_start(state == STATE_calc_start);
        
        uMul.req(truncate(arg0), truncate(arg1));
        state <= STATE_calc_end;

    endrule

    Reg#(Bit#(64)) result_l <- mkRegU();

    rule calc_end(state == STATE_calc_end);
        
        let c <- uMul.resp();

        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ALUTEST,
                                            stringID: `STREAMS_ALUTEST_NUM64_SP,
                                            payload0: c[127:96],
                                            payload1: c[95:64] });

        result_l <= c[63:0];
        state <= STATE_calc_end1;

    endrule

    rule calc_end1(state == STATE_calc_end1);
        
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ALUTEST,
                                            stringID: `STREAMS_ALUTEST_NUM64_CR,
                                            payload0: result_l[63:32],
                                            payload1: result_l[31:0] });

        state <= STATE_ready;

    endrule


    // ====================================================================
    //
    // Below this point is just mechanics of reading in numbers,
    // terminating, etc.
    //
    // ====================================================================

    rule send_load_req(state == STATE_ready && addr != `LAST_ADDR);

        link_memory.makeReq(tagged SCRATCHPAD_MEM_READ addr);
        state <= STATE_awaitingResponse;

    endrule

    //
    // recv_load_resp --
    //     Group a set of 4 32 bit responses into a pair of 64 bit values
    //     that will be handed to the calc function.
    //
    rule recv_load_resp(state == STATE_awaitingResponse);

        SCRATCHPAD_MEM_VALUE v = link_memory.getResp();
        link_memory.deq();

        case (pos)
            0:
            begin
                arg0[31:0] <= v;
            end
            
            1:
            begin
                arg0[63:32] <= v;
                link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ALUTEST,
                                                    stringID: `STREAMS_ALUTEST_NUM64_SP,
                                                    payload0: v,
                                                    payload1: arg0[31:0] });
            end
            
            2:
            begin
                arg1[31:0] <= v;
            end
            
            3:
            begin
                arg1[63:32] <= v;
                link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ALUTEST,
                                                    stringID: `STREAMS_ALUTEST_NUM64_SP,
                                                    payload0: v,
                                                    payload1: arg1[31:0] });
            end
        endcase

        if (pos == 3)
            state <= STATE_calc_start;
        else
            state <= STATE_ready;

        pos   <= pos + 1;
        addr  <= addr + 4;

    endrule

    rule terminate (state == STATE_ready && addr == `LAST_ADDR);

        state <= STATE_finished;

    endrule

    rule finishup (state == STATE_finished && cooldown != 0);

        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_NULL,
                                            stringID: `STREAMS_MESSAGE_EXIT,
                                            payload0: 0,
                                            payload1: 0 });
        cooldown <= cooldown - 1;

    endrule

    rule accept_invalidates(True);

        SCRATCHPAD_MEM_ADDRESS addr = link_memory_inval.receive();
        link_memory_inval.deq();

    endrule

endmodule
