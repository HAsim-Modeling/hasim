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
// Test individual request and pipeline throughput of various BRAM sizes.
// Use "null" benchmark.  No input data required.
//


// Library imports.

import FIFO::*;
import Vector::*;

//
// The Bluespec library providing BRAM.  HAsim already provides a BRAM
// interface and mkBRAM module, so the Bluespec library must be accessed
// with BRAM::
//
import BRAM::*;
import ClientServer::*;
import GetPut::*;


`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"
`include "asim/provides/scratchpad_memory_service.bsh"

`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS_BRAMTEST.bsh"
`include "asim/dict/STREAMS_MESSAGE.bsh"


// HAsim or Bluespec BRAM?
`define HASIM_BRAM 1


// ========================================================================
//
//  Test module.  Builds a BRAM of requested size and provides a test
//  interface.
//
// ========================================================================

interface BRAM_TEST#(type t_INDEX, type t_DATA);
    method Action writeStart();
    method Action writeEnd();

    method Action readStart();
    method Action readEnd();

    method Action readDelayedStart();
    method Action readDelayedEnd();

    method Action readWriteStart();
    method Action readWriteEnd();
endinterface: BRAM_TEST


module mkBRAMTest#(FIFO#(STREAMS_REQUEST) streamsQ)
    // interface:
    (BRAM_TEST#(Bit#(indexBits), Bit#(dataBits)))
    provisos (Add#(a__, dataBits, 256),
              Add#(64, dataBits, TAdd#(dataBits, 64)),

              // These are for Bluespec BRAM
              Add#(x, 1, indexBits),
              Add#(y, 1, dataBits),

              Alias#(BRAMRequest#(Bit#(indexBits), Bit#(dataBits)), t_BRAM_REQ));
    
`ifdef HASIM_BRAM
    BRAM#(Bit#(indexBits), Bit#(dataBits)) ram <- mkBRAM();
`else
    BRAM::BRAM#(Bit#(indexBits), Bit#(dataBits)) ram <- BRAM::mkBRAM();
`endif

    Reg#(Bit#(64)) fpgaCycle <- mkReg(0);
    Reg#(Bit#(64)) lastCycle <- mkRegU();

    let writeDataStart = 'h12345678abcdef634828a88f321491aefda329658429f9929e9341758bc13463;

    Reg#(Bit#(indexBits)) writeIdx <- mkReg(0);
    Reg#(Bit#(256)) writeData <- mkRegU();

    Reg#(Bool) readDone <- mkRegU();
    Reg#(Bit#(indexBits)) readIdx <- mkReg(0);
    Reg#(Bit#(256)) expectReadData <- mkRegU();
    Reg#(Bit#(indexBits)) readWriteIdx <- mkReg(0);
    FIFO#(Tuple2#(Bool, Bit#(dataBits))) readQ <- mkSizedFIFO(16);
    Reg#(Bool) doingRW <- mkReg(False);

    Reg#(Bit#(indexBits)) readDelayedIdx <- mkReg(0);
    FIFO#(Tuple2#(Bool, Bit#(dataBits))) readDelayedQ <- mkSizedFIFO(16);

    Reg#(Bit#(64)) startCycle <- mkRegU();


    //
    // Functions for building Bluespec BRAM requests
    //
    function t_BRAM_REQ writeReq(Bit#(indexBits) idx, Bit#(dataBits) data);
        t_BRAM_REQ req;
        req.write = True;
        req.address = idx;
        req.datain = data;
        return req;
    endfunction

    function t_BRAM_REQ readReq(Bit#(indexBits) idx);
        t_BRAM_REQ req;
        req.write = False;
        req.address = idx;
        req.datain = ?;
        return req;
    endfunction


    //
    // doWrites --
    //     Write only test.
    //
    rule doWrites (writeIdx != 0);
`ifdef HASIM_BRAM
        ram.write(writeIdx, truncate(writeData));
`else
        ram.portA.request.put(writeReq(writeIdx, truncate(writeData)));
`endif
        
        // Update write data for next stage
        let new_data = (writeData << 1);
        new_data[0] = writeData[255];
        writeData <= new_data;

        // Compute single write latency one time
        if (writeIdx == 1)
        begin
            Bit#(32) write_cycles = truncate(fpgaCycle - lastCycle);
            streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                                stringID: `STREAMS_BRAMTEST_WRITE_1,
                                                payload0: fromInteger(valueOf(dataBits)),
                                                payload1: write_cycles });
        end

        lastCycle <= fpgaCycle;
        writeIdx <= writeIdx - 1;
    endrule


    //
    // startReads --
    //     Read only test.
    //
    rule startReads (readIdx != 0);
`ifdef HASIM_BRAM
        ram.readReq(readIdx);
`else
        ram.portB.request.put(readReq(readIdx));
`endif
        readQ.enq(tuple2(readIdx == 1, truncate(expectReadData)));
        lastCycle <= fpgaCycle;

        // Compute expected value of next read (assumes doWrites ran first)
        let new_read_data = (expectReadData << 1);
        new_read_data[0] = expectReadData[255];
        expectReadData <= new_read_data;

        readIdx <= readIdx - 1;
    endrule


    //
    // startReadWrites --
    //     Read & write dual test using separate ports.
    //
    rule startReadWrites (readWriteIdx != 0);
`ifdef HASIM_BRAM
        ram.write(readWriteIdx + 1, truncate(writeData));
`else
        ram.portA.request.put(writeReq(readWriteIdx + 1, truncate(writeData)));
`endif

        // Compute write value for next iternation
        let new_data = (writeData << 1);
        new_data[0] = writeData[255];
        writeData <= new_data;

`ifdef HASIM_BRAM
        ram.readReq(readWriteIdx);
`else
        ram.portB.request.put(readReq(readWriteIdx));
`endif
        readQ.enq(tuple2(readWriteIdx == 1, truncate(expectReadData)));

        // Compute expected value of next read (assumes doWrites ran first)
        let new_read_data = (expectReadData << 1);
        new_read_data[0] = expectReadData[255];
        expectReadData <= new_read_data;

        lastCycle <= fpgaCycle;
        readWriteIdx <= readWriteIdx - 1;
    endrule
    

    //
    // getReads --
    //     Read consumer for both startReads and startReadWrites.
    //
    rule getReads (True);
`ifdef HASIM_BRAM
        let v <- ram.readRsp();
`else
        let v <- ram.portB.response.get();
`endif

        // Internal status queue
        match { .is_last, .expected_val } = readQ.first();
        readQ.deq();
        
        // Is read data correct?
        Bool err = False;
        if (expected_val != v)
        begin
            Bit#(64) p_v = truncate({ 64'b0, expected_val });
            streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                           stringID: `STREAMS_BRAMTEST_ERR_VAL,
                                           payload0: p_v[63:32],
                                           payload1: p_v[31:0] });
            err = True;
        end

        // Display latency of a single read
        if (is_last)
        begin
            readDone <= True;

            if (! err)
            begin
                Bit#(32) read_cycles = truncate(fpgaCycle - lastCycle);
                streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                               stringID: doingRW ? `STREAMS_BRAMTEST_READWRITE_1 : `STREAMS_BRAMTEST_READ_1,
                                               payload0: fromInteger(valueOf(dataBits)),
                                               payload1: read_cycles });
            end
        end
    endrule


    //
    // startDelayedReads --
    //     Read only test with consumer that doesn't run every cycle.
    //
    rule startDelayedReads (readDelayedIdx != 0);
`ifdef HASIM_BRAM
        ram.readReq(readDelayedIdx);
`else
        ram.portB.request.put(readReq(readDelayedIdx));
`endif
        readDelayedQ.enq(tuple2(readDelayedIdx == 1, truncate(expectReadData)));
        lastCycle <= fpgaCycle;

        // Compute expected value of next read (assumes doWrites ran first)
        let new_read_data = (expectReadData << 1);
        new_read_data[0] = expectReadData[255];
        expectReadData <= new_read_data;

        readDelayedIdx <= readDelayedIdx - 1;
    endrule
    

    //
    // getReadsDelayed --
    //     Read consumer for startDelayedReads.  Doesn't consume a read every
    //     cycle as a test of protection logic in BRAM to avoid missing reads
    //     or returning bad values.
    //
    rule getReadsDelayed (fpgaCycle[2] == 0);
`ifdef HASIM_BRAM
        let v <- ram.readRsp();
`else
        let v <- ram.portB.response.get();
`endif

        // Internal status queue
        match { .is_last, .expected_val } = readDelayedQ.first();
        readDelayedQ.deq();
        
        // Is read data correct?
        if (expected_val != v)
        begin
            Bit#(64) p_v = truncate({ 64'b0, expected_val });
            streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                           stringID: `STREAMS_BRAMTEST_ERR_VAL,
                                           payload0: p_v[63:32],
                                           payload1: p_v[31:0] });
        end

        readDone <= is_last;
    endrule


    rule cycleCounter (True);
        fpgaCycle <= fpgaCycle + 1;
    endrule
    

    //
    // writeStart --
    //     Start write only test.  This must be run before the other two tests
    //     in order to initialize the BRAM for reads.
    //
    method Action writeStart();
        startCycle <= fpgaCycle;
        writeData <= writeDataStart;
        writeIdx <= 16;
    endmethod


    //
    // writeEnd --
    //     Wait for write-only test to complete and print summary.
    //
    method Action writeEnd() if (writeIdx == 0);
        Bit#(32) total_cycles = truncate(fpgaCycle - startCycle - 1);
        streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                       stringID: `STREAMS_BRAMTEST_WRITE_PIPE,
                                       payload0: fromInteger(valueOf(dataBits)),
                                       payload1: total_cycles });
    endmethod


    //
    // readStart --
    //     Start read-only test.  writeStart must be called before this test
    //     to initialize the BRAM.
    //
    method Action readStart();
        startCycle <= fpgaCycle;
        expectReadData <= writeDataStart;
        readDone <= False;
        readIdx <= 16;
    endmethod


    //
    // readEnd --
    //     Wait for read-only test to complete and print summary.
    //
    method Action readEnd() if (readDone);
        Bit#(32) total_cycles = truncate(fpgaCycle - startCycle - 2);
        streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                       stringID: `STREAMS_BRAMTEST_READ_PIPE,
                                       payload0: fromInteger(valueOf(dataBits)),
                                       payload1: total_cycles });
    endmethod


    //
    // readDelayedStart --
    //     Start read-only test with reader delays.  writeStart must be called
    //     before this test to initialize the BRAM.
    //
    method Action readDelayedStart();
        startCycle <= fpgaCycle;
        expectReadData <= writeDataStart;
        readDone <= False;
        readDelayedIdx <= 16;
    endmethod


    //
    // readDelayedEnd --
    //     Wait for read-only test with delays to complete and print summary.
    //
    method Action readDelayedEnd() if (readDone);
        Bit#(32) total_cycles = truncate(fpgaCycle - startCycle - 2);
        streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                       stringID: `STREAMS_BRAMTEST_READDELAY_PIPE,
                                       payload0: fromInteger(valueOf(dataBits)),
                                       payload1: total_cycles });
    endmethod


    //
    // readWriteStart --
    //     Read & write combined test.  readStart() may not be run immediately
    //     after this test as the values written are not what it expects.
    //
    method Action readWriteStart();
        startCycle <= fpgaCycle;
        readDone <= False;
        expectReadData <= writeDataStart;
        writeData <= writeDataStart;
        doingRW <= True;
        readWriteIdx <= 16;
    endmethod


    //
    // readWriteEnd --
    //     Wait for read & write test to complete and print summary.
    //
    method Action readWriteEnd() if (readDone);
        Bit#(32) total_cycles = truncate(fpgaCycle - startCycle - 2);
        streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                       stringID: `STREAMS_BRAMTEST_READWRITE_PIPE,
                                       payload0: fromInteger(valueOf(dataBits)),
                                       payload1: total_cycles });
        doingRW <= False;
    endmethod

endmodule


interface BRAM_TEST_DRIVER#(type t_INDEX, type t_DATA);
    method Action start();
    method Action finish();
endinterface: BRAM_TEST_DRIVER

module mkBRAMTestDriver#(FIFO#(STREAMS_REQUEST) streamsQ)
    // interface:
    (BRAM_TEST_DRIVER#(Bit#(indexBits), Bit#(dataBits)))
    provisos (Add#(a__, dataBits, 256),
              Add#(64, dataBits, TAdd#(dataBits, 64)),

              // These are for Bluespec BRAM
              Add#(x, 1, indexBits),
              Add#(y, 1, dataBits));

    
    BRAM_TEST#(Bit#(indexBits), Bit#(dataBits)) bram <- mkBRAMTest(streamsQ);
    Reg#(Bit#(5)) state <- mkReg(0);

    rule driverWriteStart (state == 1);
        bram.writeStart();
        state <= state + 1;
    endrule

    rule driverWriteEnd (state == 2);
        bram.writeEnd();
        state <= state + 1;
    endrule

    rule driverReadStart (state == 3);
        bram.readStart();
        state <= state + 1;
    endrule

    rule driverReadEnd (state == 4);
        bram.readEnd();
        state <= state + 1;
    endrule

    rule driverReadDelayedStart (state == 5);
        bram.readDelayedStart();
        state <= state + 1;
    endrule

    rule driverReadDelayedEnd (state == 6);
        bram.readDelayedEnd();
        state <= state + 1;
    endrule

    rule driverReadWriteStart (state == 7);
        bram.readWriteStart();
        state <= state + 1;
    endrule

    rule driverReadWriteEnd (state == 8);
        bram.readWriteEnd();
        state <= 0;
    endrule

    method Action start() if (state == 0);
        state <= 1;
    endmethod
    
    method Action finish() if (state == 0);
        noAction;
    endmethod

endmodule


// ========================================================================
//
//  Test driver
//
// ========================================================================

module [CONNECTED_MODULE] mkSystem ();

    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    FIFO#(STREAMS_REQUEST) streamsQ <- mkSizedFIFO(128);

    Reg#(Bit#(5)) state <- mkReg(0);

    rule start (state == 0);
        streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                       stringID: `STREAMS_BRAMTEST_START,
                                       payload0: 0,
                                       payload1: 0 });
        state <= state + 1;
    endrule


    BRAM_TEST_DRIVER#(Bit#(10), Bit#(8)) bram8Test <- mkBRAMTestDriver(streamsQ);

    rule bram8_Start (state == 1);
        bram8Test.start();
        state <= state + 1;
    endrule

    rule bram8_Finish (state == 2);
        bram8Test.finish();
        state <= state + 1;
    endrule


    BRAM_TEST_DRIVER#(Bit#(10), Bit#(64)) bram64Test <- mkBRAMTestDriver(streamsQ);

    rule bram64_Start (state == 3);
        bram64Test.start();
        state <= state + 1;
    endrule

    rule bram64_Finish (state == 4);
        bram64Test.finish();
        state <= state + 1;
    endrule


    BRAM_TEST_DRIVER#(Bit#(5), Bit#(210)) bram210Test <- mkBRAMTestDriver(streamsQ);

    rule bram210_Start (state == 5);
        bram210Test.start();
        state <= state + 1;
    endrule

    rule bram210_Finish (state == 6);
        bram210Test.finish();
        state <= state + 1;
    endrule


    rule doneMessage (state == 7);
        streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_BRAMTEST,
                                       stringID: `STREAMS_BRAMTEST_DONE,
                                       payload0: 0,
                                       payload1: 0 });
        state <= state + 1;
    endrule

    rule exit (state == 8);
        streamsQ.enq(STREAMS_REQUEST { streamID: `STREAMID_NULL,
                                       stringID: `STREAMS_MESSAGE_EXIT,
                                       payload0: 0,
                                       payload1: 0 });
    endrule

    //
    // streams --
    //     Monitor streams queue and emit messages to host.
    //
    rule streams (True);
        let msg = streamsQ.first();
        streamsQ.deq();
        
        link_streams.send(msg);
    endrule

endmodule
