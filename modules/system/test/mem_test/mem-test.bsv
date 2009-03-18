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
`include "asim/dict/VDEV_SCRATCH.bsh"
`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS_MEMTEST.bsh"
`include "asim/dict/STREAMS_MESSAGE.bsh"

`include "asim/dict/PARAMS_HASIM_SYSTEM.bsh"

`define START_ADDR 0
`define LAST_ADDR  'h7f

typedef enum
{
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

// Test that complex types can be passed to mkMemPack
typedef struct
{
    Bit#(10) x;
}
MEM_DATA_SM
    deriving (Bits, Eq);

typedef Bit#(7) MEM_ADDRESS;

module [HASIM_MODULE] mkSystem ()
    provisos (Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),

              Bits#(MEM_ADDRESS, t_MEM_ADDRESS_SZ),

              // Large data (multiple containers for single datum)
              Alias#(Int#(TAdd#(t_SCRATCHPAD_MEM_VALUE_SZ, 1)), t_MEM_DATA_LG),
              Bits#(t_MEM_DATA_LG, t_MEM_DATA_LG_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_MEM_ADDRESS_SZ, t_MEM_DATA_LG_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_ADDR_LG),

              // Medium data (same container size as data)
              Alias#(Bit#(TSub#(t_SCRATCHPAD_MEM_VALUE_SZ, 1)), t_MEM_DATA_MD),
              Bits#(t_MEM_DATA_MD, t_MEM_DATA_MD_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_MEM_ADDRESS_SZ, t_MEM_DATA_MD_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_ADDR_MD),

              // Small data (multiple data per container)
              Alias#(MEM_DATA_SM, t_MEM_DATA_SM),
              Bits#(t_MEM_DATA_SM, t_MEM_DATA_SM_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_MEM_ADDRESS_SZ, t_MEM_DATA_SM_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_ADDR_SM));

    //
    // Allocate containers and scratchpads
    //

    // Large data (multiple containers for single datum)
    MEMORY_IFC#(t_CONTAINER_ADDR_LG, SCRATCHPAD_MEM_VALUE) containerMemoryLG <- mkDirectScratchpad(`VDEV_SCRATCH_MEMTEST_LG);
    MEM_PACK_IFC#(MEM_ADDRESS, t_MEM_DATA_LG, t_CONTAINER_ADDR_LG, SCRATCHPAD_MEM_VALUE) memoryLG <- mkMemPack(containerMemoryLG);

    // Medium data (same container size as data)
    MEMORY_IFC#(t_CONTAINER_ADDR_MD, SCRATCHPAD_MEM_VALUE) containerMemoryMD <- mkDirectScratchpad(`VDEV_SCRATCH_MEMTEST_MD);
    MEM_PACK_IFC#(MEM_ADDRESS, t_MEM_DATA_MD, t_CONTAINER_ADDR_MD, SCRATCHPAD_MEM_VALUE) memoryMD <- mkMemPack(containerMemoryMD);

    // Small data (multiple data per container)
    MEMORY_IFC#(t_CONTAINER_ADDR_SM, SCRATCHPAD_MEM_VALUE) containerMemorySM <- mkDirectScratchpad(`VDEV_SCRATCH_MEMTEST_SM);
    MEM_PACK_IFC#(MEM_ADDRESS, t_MEM_DATA_SM, t_CONTAINER_ADDR_SM, SCRATCHPAD_MEM_VALUE) memorySM <- mkMemPack(containerMemorySM);

    // Dynamic parameters.
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();

    // Memory initialization (write) modes:
    //  0 -- normal
    //  1 -- write zeros
    //  2 -- no writes
    Param#(2) memInitMode <- mkDynamicParameter(`PARAMS_HASIM_SYSTEM_MEM_TEST_INIT_MODE, paramNode);

    // Streams (output)
    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    Reg#(CYCLE_COUNTER) cycle <- mkReg(0);
    Reg#(STATE) state <- mkReg(STATE_writing);

    Reg#(MEM_ADDRESS) addr <- mkReg(`START_ADDR);

    
    (* fire_when_enabled *)
    rule cycleCount (True);
        cycle <= cycle + 1;
    endrule


    // ====================================================================
    //
    // Write values into memory
    //
    // ====================================================================

    rule sendWrite (state == STATE_writing);
        //
        // Store different values in each of the memories to increase confidence
        // that data are being directed to the right places.
        //
        // There are three dynamic modes, useful for testing memory in case
        // the backing storage retains its state between runs.  Mode 0 is the
        // normal case, mode 1 writes zeros, and mode 2 skips the writes.
        //
        if (memInitMode != 2)
        begin
            t_MEM_DATA_LG dataLG = 0;
            t_MEM_DATA_MD dataMD = 0;
            t_MEM_DATA_SM dataSM = unpack(0);

            if (memInitMode == 0)
            begin
                dataLG = -(unpack(zeroExtend(pack(addr))) + 2);
                dataMD = unpack(zeroExtend(pack(addr))) + 1;
                dataSM = unpack(zeroExtend(pack(addr)));
            end

            memoryLG.write(addr, dataLG);
            memoryMD.write(addr, dataMD);
            memorySM.write(addr, dataSM);
        end
        
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
    

    // ====================================================================
    //
    // Read values back and dump them through streams
    //
    // ====================================================================

    FIFO#(MEM_ADDRESS) readAddrLGQ <- mkSizedFIFO(32);
    FIFO#(MEM_ADDRESS) readAddrMDQ <- mkSizedFIFO(32);
    FIFO#(MEM_ADDRESS) readAddrSMQ <- mkSizedFIFO(32);
    Reg#(Bool) readDone <- mkReg(False);
    Reg#(Bit#(2)) nCompleteReads <- mkReg(0);

    //
    // Initiate read request on each memory in parallel.
    //
    rule readReq (state == STATE_reading && ! readDone);
        memoryLG.readReq(addr);
        memoryMD.readReq(addr);
        memorySM.readReq(addr);

        readAddrLGQ.enq(addr);
        readAddrMDQ.enq(addr);
        readAddrSMQ.enq(addr);

        if (addr == `LAST_ADDR)
        begin
            addr <= `START_ADDR;
            readDone <= True;
        end
        else
        begin
            addr <= addr + 1;
        end
    endrule

    //
    // Individual rules to receive values and write them to the same stream.
    // The Bluespec scheduler will pick an order.
    //

    rule readRecvLG (True);
        let r_addr = readAddrLGQ.first();
        readAddrLGQ.deq();

        let v <- memoryLG.readRsp();

        // Convert value so it equals r_addr
        if (memInitMode == 0)
            v = -(v + 2);
        
        STREAMS_DICT_TYPE msg_id = `STREAMS_MEMTEST_DATA_LG;
        if (((memInitMode != 1) && (v != unpack(zeroExtend(pack(r_addr))))) ||
            ((memInitMode == 1) && (v != unpack(0))))
        begin
            msg_id = `STREAMS_MEMTEST_DATA_LG_ERR;
        end

        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                            stringID: msg_id,
                                            payload0: zeroExtend(r_addr),
                                            payload1: truncate(pack(v)) });
        
        if (r_addr == `LAST_ADDR)
        begin
            // All readers done?
            if (nCompleteReads == 2)
                state <= STATE_read_timing;
            
            nCompleteReads <= nCompleteReads + 1;
        end
    endrule

    rule readRecvMD (True);
        let r_addr = readAddrMDQ.first();
        readAddrMDQ.deq();

        let v <- memoryMD.readRsp();

        // Convert value so it equals r_addr
        if (memInitMode == 0)
            v = v - 1;

        STREAMS_DICT_TYPE msg_id = `STREAMS_MEMTEST_DATA_MD;
        if (((memInitMode != 1) && (v != unpack(zeroExtend(pack(r_addr))))) ||
            ((memInitMode == 1) && (v != unpack(0))))
        begin
            msg_id = `STREAMS_MEMTEST_DATA_MD_ERR;
        end

        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                            stringID: msg_id,
                                            payload0: zeroExtend(r_addr),
                                            payload1: truncate(pack(v)) });
        
        if (r_addr == `LAST_ADDR)
        begin
            // All readers done?
            if (nCompleteReads == 2)
                state <= STATE_read_timing;
            
            nCompleteReads <= nCompleteReads + 1;
        end
    endrule

    rule readRecvSM (True);
        let r_addr = readAddrSMQ.first();
        readAddrSMQ.deq();

        let v <- memorySM.readRsp();

        STREAMS_DICT_TYPE msg_id = `STREAMS_MEMTEST_DATA_SM;
        if (((memInitMode != 1) && (v != unpack(zeroExtend(pack(r_addr))))) ||
            ((memInitMode == 1) && (v != unpack(0))))
        begin
            msg_id = `STREAMS_MEMTEST_DATA_SM_ERR;
        end

        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                            stringID: msg_id,
                                            payload0: zeroExtend(r_addr),
                                            payload1: zeroExtend(pack(v)) });
        
        if (r_addr == `LAST_ADDR)
        begin
            // All readers done?
            if (nCompleteReads == 2)
                state <= STATE_read_timing;
            
            nCompleteReads <= nCompleteReads + 1;
        end
    endrule
    

    // ====================================================================
    //
    // Read latency test
    //
    // ====================================================================

    FIFO#(CYCLE_COUNTER) readCycleQ <- mkSizedFIFO(32);
    Reg#(Bit#(4)) readCycleReqIdx <- mkReg(0);
    Reg#(Bit#(4)) readCycleRespIdx <- mkReg(0);
    Reg#(Vector#(8, Bit#(8))) readCycles <- mkRegU();
    Reg#(Bit#(2)) timingPass <- mkReg(0);

    rule readTimeReq (state == STATE_read_timing && (readCycleReqIdx != 8));
        case (timingPass)
            0: memoryLG.readReq(addr);
            1: memoryMD.readReq(addr);
            2: memorySM.readReq(addr);
        endcase

        readCycleQ.enq(cycle);
        addr <= addr + 4;
        readCycleReqIdx <= readCycleReqIdx + 1;
    endrule

    rule readTimeRecv (state == STATE_read_timing);
        let start_cycle = readCycleQ.first();
        readCycleQ.deq();

        case (timingPass)
            0: let x <- memoryLG.readRsp();
            1: let y <- memoryMD.readRsp();
            2: let z <- memorySM.readRsp();
        endcase

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

        if (timingPass == 2)
        begin
            // Done with timing test
            state <= STATE_finished;
        end
        else
        begin
            // Another pass on a different access port
            state <= STATE_read_timing;
            readCycleReqIdx <= 0;
            readCycleRespIdx <= 0;
            timingPass <= timingPass + 1;
        end
    endrule


    // ====================================================================
    //
    // End of program.
    //
    // ====================================================================

    rule sendDone (state == STATE_finished);
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                            stringID: `STREAMS_MEMTEST_DONE,
                                            payload0: 0,
                                            payload1: 0 });
        state <= STATE_exit;
    endrule

    rule finished (state == STATE_exit);
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_NULL,
                                            stringID: `STREAMS_MESSAGE_EXIT,
                                            payload0: 0,
                                            payload1: 0 });
    endrule

endmodule
