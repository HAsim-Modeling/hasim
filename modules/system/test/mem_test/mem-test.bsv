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
import GetPut::*;
import LFSR::*;

`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/librl_bsv_storage.bsh"

`include "asim/provides/soft_connections.bsh"

`include "asim/provides/mem_services.bsh"
`include "asim/provides/common_services.bsh"

`include "asim/dict/VDEV_SCRATCH.bsh"
`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS_MEMTEST.bsh"
`include "asim/dict/STREAMS_MESSAGE.bsh"

`include "asim/dict/PARAMS_HARDWARE_SYSTEM.bsh"

// It is normally NOT necessary to include scratchpad_memory.bsh to use
// scratchpads.  mem-test includes it only to get the value of
// SCRATCHPAD_MEM_VALUE in order to pick data sizes that will force
// the three possible container scenarios:  multiple containers per
// datum, one container per datum, multiple data per container.
`include "asim/provides/scratchpad_memory.bsh"

`define START_ADDR 0
`define LAST_ADDR  'h1ff

typedef enum
{
    STATE_init,
    STATE_writing,
    STATE_read_random,
    STATE_read_sequential,
    STATE_read_timing,
    STATE_read_timing_emit0,
    STATE_read_timing_emit1,
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

typedef Bit#(13) MEM_ADDRESS;

module [CONNECTED_MODULE] mkSystem ()
    provisos (Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),

              // Large data (multiple containers for single datum)
              Alias#(Int#(TAdd#(t_SCRATCHPAD_MEM_VALUE_SZ, 1)), t_MEM_DATA_LG),

              // Medium data (same container size as data)
              Alias#(Bit#(TSub#(t_SCRATCHPAD_MEM_VALUE_SZ, 1)), t_MEM_DATA_MD),

              // Small data (multiple data per container)
              Alias#(MEM_DATA_SM, t_MEM_DATA_SM));

    //
    // Allocate scratchpads
    //

    let private_caches = (`MEM_TEST_PRIVATE_CACHES != 0 ? SCRATCHPAD_CACHED :
                                                          SCRATCHPAD_NO_PVT_CACHE);

    // Large data (multiple containers for single datum)
    MEMORY_IFC#(MEM_ADDRESS, t_MEM_DATA_LG) memoryLG <- mkScratchpad(`VDEV_SCRATCH_MEMTEST_LG, private_caches);

    // Medium data (same container size as data)
    MEMORY_IFC#(MEM_ADDRESS, t_MEM_DATA_MD) memoryMD <- mkScratchpad(`VDEV_SCRATCH_MEMTEST_MD, private_caches);

    // Small data (multiple data per container)
    MEMORY_IFC#(MEM_ADDRESS, t_MEM_DATA_SM) memorySM <- mkScratchpad(`VDEV_SCRATCH_MEMTEST_SM, private_caches);

    // Heap
    MEMORY_HEAP#(MEM_ADDRESS, t_MEM_DATA_SM) heap <- mkMemoryHeapUnionScratchpad(`VDEV_SCRATCH_MEMTEST_HEAP, private_caches);


    DEBUG_FILE debugLog <- mkDebugFile("mem_test.out");

    // Dynamic parameters.
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();

    // Memory initialization (write) modes:
    //  0 -- normal
    //  1 -- write zeros
    //  2 -- no writes
    Param#(2) memInitMode <- mkDynamicParameter(`PARAMS_HARDWARE_SYSTEM_MEM_TEST_INIT_MODE, paramNode);

    // Verbose mode
    //  0 -- quiet
    //  1 -- verbose
    Param#(1) verboseMode <- mkDynamicParameter(`PARAMS_HARDWARE_SYSTEM_MEM_TEST_VERBOSE, paramNode);
    let verbose = verboseMode == 1;

    // Heap enable -- allow heap tests?  Skip heap case (it used to cause deadlocks).
    //  0 -- disable
    //  1 -- enable
    Param#(1) heapTestMode <- mkDynamicParameter(`PARAMS_HARDWARE_SYSTEM_MEM_TEST_HEAP, paramNode);
    let enableHeap = heapTestMode == 1;

    // Streams (output)
    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    Reg#(CYCLE_COUNTER) cycle <- mkReg(0);
    Reg#(STATE) state <- mkReg(STATE_init);

    Reg#(MEM_ADDRESS) addr <- mkReg(`START_ADDR);

    // Random number generator
    LFSR#(Bit#(16)) lfsr <- mkLFSR_16();

    Reg#(Bit#(2)) nCompleteReads <- mkRegU();

    // If not doing heap tests then mark heap test done already
    function Bit#(2) completeReadsInitVal() = enableHeap ? 0 : 1;

    
    (* fire_when_enabled *)
    rule cycleCount (True);
        cycle <= cycle + 1;
    endrule

    rule doInit (state == STATE_init);
        nCompleteReads <= completeReadsInitVal();

        lfsr.seed(1);
        state <= STATE_writing;
    endrule


    // ====================================================================
    //
    // Write values into memory
    //
    // ====================================================================

    (* conservative_implicit_conditions *)
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
            t_MEM_DATA_SM dataH  = unpack(0);

            if (memInitMode == 0)
            begin
                dataLG = -(unpack(zeroExtend(pack(addr))) + 2);
                dataMD = unpack(zeroExtend(pack(addr))) + 1;
                dataSM = unpack(truncate(pack(addr)));
            end

            memoryLG.write(addr, dataLG);
            debugLog.record($format("writeLG: addr 0x%x, data 0x%x", addr, dataLG));

            memoryMD.write(addr, dataMD);
            debugLog.record($format("writeMD: addr 0x%x, data 0x%x", addr, dataMD));

            memorySM.write(addr, dataSM);
            debugLog.record($format("writeSM: addr 0x%x, data 0x%x", addr, dataSM));
            
            // Allocate a slot in the heap
            if (enableHeap)
            begin
                let heap_idx <- heap.malloc();
                debugLog.record($format("malloc: idx 0x%x", heap_idx));

                if (memInitMode == 0)
                begin
                    dataH  = unpack(~truncate(pack(heap_idx)));
                end

                heap.write(heap_idx, dataH);
                debugLog.record($format("writeH: idx 0x%x, data 0x%x", heap_idx, dataH));
            end
        end
        
        if (addr == `LAST_ADDR)
        begin
            addr <= `START_ADDR;
            state <= STATE_read_random;
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

    FIFO#(Tuple2#(MEM_ADDRESS, Bool)) readAddrLGQ <- mkSizedFIFO(32);
    FIFO#(Tuple2#(MEM_ADDRESS, Bool)) readAddrMDQ <- mkSizedFIFO(32);
    FIFO#(Tuple2#(MEM_ADDRESS, Bool)) readAddrSMQ <- mkSizedFIFO(32);
    FIFO#(Tuple2#(MEM_ADDRESS, Bool)) readAddrHQ  <- mkSizedFIFO(32);
    Reg#(Bool) readSeqDone <- mkReg(False);
    Reg#(Bit#(10)) randTrip <- mkReg(0);

    //
    // Initiate random read request on each memory in parallel.  This is mostly
    // a cache test.
    //
    rule readRandomReq (state == STATE_read_random && (randTrip != maxBound));
        MEM_ADDRESS r_addr = truncate(lfsr.value) & `LAST_ADDR;
        lfsr.next();

        memoryLG.readReq(r_addr);
        memoryMD.readReq(r_addr);
        memorySM.readReq(r_addr);
        if (enableHeap)
            heap.readReq(r_addr);

        let done = ((randTrip + 1) == maxBound);

        readAddrLGQ.enq(tuple2(r_addr, done));
        readAddrMDQ.enq(tuple2(r_addr, done));
        readAddrSMQ.enq(tuple2(r_addr, done));
        if (enableHeap)
            readAddrHQ.enq(tuple2(r_addr, done));

        debugLog.record($format("read RAND from all: addr 0x%x", r_addr));

        randTrip <= randTrip + 1;
    endrule

    //
    // Initiate sequential read request on each memory in parallel.
    //
    rule readSequentialReq (state == STATE_read_sequential && ! readSeqDone);
        memoryLG.readReq(addr);
        memoryMD.readReq(addr);
        memorySM.readReq(addr);
        if (enableHeap)
            heap.readReq(addr);

        let done = (addr == `LAST_ADDR);

        readAddrLGQ.enq(tuple2(addr, done));
        readAddrMDQ.enq(tuple2(addr, done));
        readAddrSMQ.enq(tuple2(addr, done));
        if (enableHeap)
            readAddrHQ.enq(tuple2(addr, done));

        debugLog.record($format("read SEQ from all: addr 0x%x", addr));

        // malloc on every 4th access just to keep things interesting.
        // The readRecvHeap rule is freeing every read address, so there
        // will be entries available.
        if (enableHeap && (addr[1:0] == 3))
        begin
            let m <- heap.malloc();
            debugLog.record($format("malloc: idx 0x%x", m));
        end

        if (done)
        begin
            addr <= `START_ADDR;
            readSeqDone <= True;
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

    rule readRecvLG ((state == STATE_read_random) || (state == STATE_read_sequential));
        match {.r_addr, .done} = readAddrLGQ.first();
        readAddrLGQ.deq();

        let v <- memoryLG.readRsp();
        debugLog.record($format("readLG: addr 0x%x, data 0x%x", r_addr, v));

        // Convert value so it equals r_addr
        if (memInitMode == 0)
            v = -(v + 2);
        
        Bool error = False;
        STREAMS_DICT_TYPE msg_id = `STREAMS_MEMTEST_DATA_LG;
        if (((memInitMode != 1) && (v != unpack(zeroExtend(pack(r_addr))))) ||
            ((memInitMode == 1) && (v != unpack(0))))
        begin
            msg_id = `STREAMS_MEMTEST_DATA_LG_ERR;
            error = True;
        end

        if (verbose || error)
        begin
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                                stringID: msg_id,
                                                payload0: zeroExtend(r_addr),
                                               payload1: truncate(pack(v)) });
        end
        
        if (done)
        begin
            // All readers done?
            if (nCompleteReads == 3)
            begin
                state <= unpack(pack(state) + 1);
                nCompleteReads <= completeReadsInitVal();
            end
            else
            begin
                nCompleteReads <= nCompleteReads + 1;
            end
        end
    endrule

    rule readRecvMD ((state == STATE_read_random) || (state == STATE_read_sequential));
        match {.r_addr, .done} = readAddrMDQ.first();
        readAddrMDQ.deq();

        let v <- memoryMD.readRsp();
        debugLog.record($format("readMD: addr 0x%x, data 0x%x", r_addr, v));

        // Convert value so it equals r_addr
        if (memInitMode == 0)
            v = v - 1;

        Bool error = False;
        STREAMS_DICT_TYPE msg_id = `STREAMS_MEMTEST_DATA_MD;
        if (((memInitMode != 1) && (v != unpack(zeroExtend(pack(r_addr))))) ||
            ((memInitMode == 1) && (v != unpack(0))))
        begin
            msg_id = `STREAMS_MEMTEST_DATA_MD_ERR;
            error = True;
        end

        if (verbose || error)
        begin
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                                stringID: msg_id,
                                                payload0: zeroExtend(r_addr),
                                                payload1: resize(pack(v)) });
        end
        
        if (done)
        begin
            // All readers done?
            if (nCompleteReads == 3)
            begin
                state <= unpack(pack(state) + 1);
                nCompleteReads <= completeReadsInitVal();
            end
            else
            begin
                nCompleteReads <= nCompleteReads + 1;
            end
        end
    endrule

    rule readRecvSM ((state == STATE_read_random) || (state == STATE_read_sequential));
        match {.r_addr, .done} = readAddrSMQ.first();
        readAddrSMQ.deq();

        let v <- memorySM.readRsp();
        debugLog.record($format("readSM: addr 0x%x, data 0x%x", r_addr, v));

        Bool error = False;
        STREAMS_DICT_TYPE msg_id = `STREAMS_MEMTEST_DATA_SM;
        if (((memInitMode != 1) && (v != unpack(truncate(pack(r_addr))))) ||
            ((memInitMode == 1) && (v != unpack(0))))
        begin
            msg_id = `STREAMS_MEMTEST_DATA_SM_ERR;
            error = True;
        end

        if (verbose || error)
        begin
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                                stringID: msg_id,
                                                payload0: zeroExtend(r_addr),
                                                payload1: zeroExtend(pack(v)) });
        end
        
        if (done)
        begin
            // All readers done?
            if (nCompleteReads == 3)
            begin
                state <= unpack(pack(state) + 1);
                nCompleteReads <= completeReadsInitVal();
            end
            else
            begin
                nCompleteReads <= nCompleteReads + 1;
            end
        end
    endrule

    (* descending_urgency = "readRecvHeap, readRecvSM, readRecvMD, readRecvLG" *)
    rule readRecvHeap ((state == STATE_read_random) || (state == STATE_read_sequential));
        match {.r_addr, .done} = readAddrHQ.first();
        readAddrHQ.deq();

        if (state == STATE_read_sequential)
        begin
            heap.free(r_addr);
            debugLog.record($format("free: idx 0x%x", r_addr));
        end

        let v <- heap.readRsp();
        debugLog.record($format("readH: idx 0x%x, data 0x%x", r_addr, v));

        if (memInitMode == 0)
            v = unpack(~pack(v));

        Bool error = False;
        STREAMS_DICT_TYPE msg_id = `STREAMS_MEMTEST_DATA_H;
        if (((memInitMode != 1) && (v != unpack(truncate(pack(r_addr))))) ||
            ((memInitMode == 1) && (v != unpack(0))))
        begin
            msg_id = `STREAMS_MEMTEST_DATA_H_ERR;
            error = True;
        end

        if (verbose || error)
        begin
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                                stringID: msg_id,
                                                payload0: zeroExtend(r_addr),
                                                payload1: zeroExtend(pack(v)) });
        end
        
        if (done)
        begin
            // All readers done?
            if (nCompleteReads == 3)
            begin
                state <= unpack(pack(state) + 1);
                nCompleteReads <= completeReadsInitVal();
            end
            else
            begin
                nCompleteReads <= nCompleteReads + 1;
            end
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
    Reg#(Vector#(8, Bit#(16))) readCycles <- mkRegU();
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
            state <= STATE_read_timing_emit0;
        end
    endrule
    
    rule readTimeEmit0 (state == STATE_read_timing_emit0);
        Bit#(128) latency = pack(readCycles);
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                            stringID: `STREAMS_MEMTEST_LATENCY,
                                            payload0: latency[63:32],
                                            payload1: latency[31:0] });

        state <= STATE_read_timing_emit1;
    endrule

    rule readTimeEmit1 (state == STATE_read_timing_emit1);
        Bit#(128) latency = pack(readCycles);
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MEMTEST,
                                            stringID: `STREAMS_MEMTEST_LATENCY,
                                            payload0: latency[127:96],
                                            payload1: latency[95:64] });

        if (timingPass == 2)
        begin
            // Done with timing test
            state <= STATE_finished;
        end
        else
        begin
            // Another pass on a different access port
            state <= STATE_read_timing;
            addr <= `START_ADDR;
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
