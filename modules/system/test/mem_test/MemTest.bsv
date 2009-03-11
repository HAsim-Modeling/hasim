//
// Copyright (C) 2009 Intel Corporation
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


import FIFO::*;
import Vector::*;


`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/platform_interface.bsh"

`include "asim/provides/scratchpad_memory.bsh"
`include "asim/provides/streams.bsh"
`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS_MEMTEST.bsh"
`include "asim/dict/STREAMS_MESSAGE.bsh"

`define START_ADDR 0
`define LAST_ADDR  'h100

typedef enum
{
    STATE_init,
    STATE_writing,
    STATE_reading,
    STATE_read_timing,
    STATE_read_timing_emit,
    STATE_finished,
    STATE_exit
}
STATE
    deriving (Bits, Eq);


typedef Bit#(32) CYCLE_COUNTER;


module [HASIM_MODULE] mkSystem ();

    Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE) link_memory <- mkConnection_Client("vdev_memory");
    Connection_Receive#(SCRATCHPAD_MEM_ADDRESS)                      link_memory_inval <- mkConnection_Receive("vdev_memory_invalidate");
    Connection_Send#(STREAMS_REQUEST)                                link_streams <- mkConnection_Send("vdev_streams");

    Reg#(CYCLE_COUNTER) cycle <- mkReg(0);
    Reg#(STATE) state <- mkReg(STATE_init);

    Reg#(SCRATCHPAD_MEM_ADDRESS) addr <- mkReg(`START_ADDR);

    
    (* fire_when_enabled *)
    rule cycleCount (True);
        cycle <= cycle + 1;
    endrule

    //
    // Delay starting to avoid startup problems in communication.  This hack
    // seems to work...
    //
    Reg#(Bit#(16)) initCnt <- mkReg(maxBound);
    rule init (state == STATE_init);
        if (initCnt == minBound)
            state <= STATE_writing;

        initCnt <= initCnt - 1;
    endrule


    //
    // Write values into memory
    //
    rule sendWrite (state == STATE_writing);
        // write address+1 to data
        SCRATCHPAD_MEM_VALUE data = (addr == 1) ? zeroExtend(pack(addr)) : ~zeroExtend(pack(addr));
        
        link_memory.makeReq(tagged SCRATCHPAD_MEM_STORE { addr: addr, val: data });
        
        if (addr == `LAST_ADDR)
        begin
            addr <= `START_ADDR;
            state <= STATE_reading;
        end
        else
        begin
            addr <= addr + 1;
        end
    endrule
    

    //
    // Read values back from memory and send to host
    //
    FIFO#(SCRATCHPAD_MEM_ADDRESS) readAddrQ <- mkSizedFIFO(32);
    Reg#(Bool) readDone <- mkReg(False);

    rule readReq (state == STATE_reading && ! readDone);
        link_memory.makeReq(tagged SCRATCHPAD_MEM_LOAD addr);
        readAddrQ.enq(addr);
        addr <= addr + 1;

        if (addr == `LAST_ADDR)
            readDone <= True;
    endrule

    rule readRecv (True);
        let r_addr = readAddrQ.first();
        readAddrQ.deq();

        SCRATCHPAD_MEM_VALUE v = link_memory.getResp();
        link_memory.deq();

        if (v != 0)
        begin
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                                stringID: `STREAMS_MEMTEST_DATA,
                                                payload0: zeroExtend(r_addr),
                                                payload1: truncate(v) });
        end
        
        if (r_addr == `LAST_ADDR)
        begin
            addr <= `START_ADDR;
            state <= STATE_read_timing;
        end
    endrule
    

    //
    // Time read latency
    //
    FIFO#(CYCLE_COUNTER) readCycleQ <- mkSizedFIFO(32);
    Reg#(Bit#(4)) readCycleReqIdx <- mkReg(0);
    Reg#(Bit#(4)) readCycleRespIdx <- mkReg(0);
    Reg#(Vector#(8, Bit#(8))) readCycles <- mkRegU();

    rule readTimeReq (state == STATE_read_timing && (readCycleReqIdx != 8));
        link_memory.makeReq(tagged SCRATCHPAD_MEM_LOAD addr);
        readCycleQ.enq(cycle);
        addr <= addr + 4;
        readCycleReqIdx <= readCycleReqIdx + 1;
    endrule

    rule readTimeRecv (True);
        let start_cycle = readCycleQ.first();
        readCycleQ.deq();

        SCRATCHPAD_MEM_VALUE v = link_memory.getResp();
        link_memory.deq();
        
        readCycles[readCycleRespIdx] <= truncate(cycle - start_cycle);
        readCycleRespIdx <= readCycleRespIdx + 1;

        if (readCycleRespIdx == 7)
        begin
            addr <= `START_ADDR;
            state <= STATE_read_timing_emit;
        end
    endrule
    
    rule readTimeEmit (state == STATE_read_timing_emit);
        Bit#(64) latency = pack(readCycles);
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                            stringID: `STREAMS_MEMTEST_LATENCY,
                                            payload0: latency[63:32],
                                            payload1: latency[31:0] });

        state <= STATE_finished;
    endrule


    //
    // End of program.
    //

    rule sendDone (state == STATE_finished);
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                            stringID: `STREAMS_MEMTEST_DONE,
                                            payload0: fromInteger(valueOf(SizeOf#(SCRATCHPAD_MEM_ADDRESS))),
                                            payload1: fromInteger(valueOf(SizeOf#(SCRATCHPAD_MEM_VALUE))) });
        state <= STATE_exit;
    endrule

    rule finished (state == STATE_exit);
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_NULL,
                                            stringID: `STREAMS_MESSAGE_EXIT,
                                            payload0: 0,
                                            payload1: 0 });
    endrule


    rule accept_invalidates(True);
        SCRATCHPAD_MEM_ADDRESS addr = link_memory_inval.receive();
        link_memory_inval.deq();
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                            stringID: `STREAMS_MEMTEST_INVAL,
                                            payload0: zeroExtend(addr),
                                            payload1: ? });
    endrule

endmodule
