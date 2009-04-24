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
import FIFOF::*;
import Vector::*;
import GetPut::*;
import LFSR::*;

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/platform_interface.bsh"

`include "asim/provides/streams.bsh"
`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS_PIPETEST.bsh"
`include "asim/dict/STREAMS_MESSAGE.bsh"

typedef enum
{
    STATE_init,
    STATE_enq,
    STATE_deq,
    STATE_finished,
    STATE_exit
}
STATE
    deriving (Bits, Eq);


typedef Bit#(`PIPE_TEST_DATA_BITS) PIPE_TEST_DATA;
typedef Bit#(TLog#(`PIPE_TEST_NUM_PIPES)) PIPELINE_IDX;


interface PIPELINE_TEST#(numeric type n_STAGES, numeric type n_PARALLEL_PIPES);
    interface Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA)) pipes;
endinterface: PIPELINE_TEST


module [HASIM_MODULE] mkSystem ();

    Reg#(STATE) state <- mkReg(STATE_init);

    // Streams (output)
    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    // Instantiate the test pipelines
    PIPELINE_TEST#(`PIPE_TEST_STAGES, `PIPE_TEST_NUM_PIPES) pipes;
    if (`PIPE_TEST_LOOP_MODE == 0)
        pipes <- mkSharedPipeTest();
    else if (`PIPE_TEST_LOOP_MODE == 1)
        pipes <- mkSharedTreePipeTest();
    else
        pipes <- mkPrivatePipeTest();

    // Random number generator
    LFSR#(Bit#(32)) lfsr_0 <- mkLFSR_32();
    LFSR#(Bit#(32)) lfsr_1 <- mkLFSR_32();

    rule doInit (state == STATE_init);
        lfsr_0.seed(1);
        lfsr_1.seed(2);
        state <= STATE_enq;
    endrule

    // ====================================================================
    //
    // Enqueue data to the pipes
    //
    // ====================================================================

    Reg#(PIPELINE_IDX) pipeIdx <- mkReg(0);
    Reg#(Bit#(1)) pipeTrips <- mkReg(0);

    rule doEnq (state == STATE_enq);
        // Pass random data so no optimizer can reduce pipeline sizes
        let v0 = lfsr_0.value();
        lfsr_0.next();
        let v1 = lfsr_1.value();
        lfsr_1.next();

        PIPE_TEST_DATA v;
        if (`PIPE_TEST_LOOP_MODE == 1)
        begin
            // Data driven routing.  Low bits of data indicate path.  Add two
            // numbers together so it isn't a constant.
            PIPELINE_IDX tgt = pipeIdx + zeroExtend(pipeTrips);
            v = truncate({v0, v1, tgt});
        end
        else
        begin
            // Not data driven routing.
            v = truncate({v0, v1});
        end

        pipes.pipes[pipeIdx].enq(v);
        
        // Enqueue to pipelines sequentially
        if (pipeIdx == maxBound)
        begin
            // Make multiple trips through the pipelines
            if (pipeTrips == maxBound)
            begin
                state <= STATE_deq;
            end

            pipeTrips <= pipeTrips + 1;
        end

        pipeIdx <= pipeIdx + 1;
    endrule


    // ====================================================================
    //
    // Dequeue data from the pipes
    //
    // ====================================================================

    Reg#(PIPE_TEST_DATA) outData <- mkReg(0);

    rule doDeq (state == STATE_deq);
        let d = pipes.pipes[pipeIdx].first();
        pipes.pipes[pipeIdx].deq();
        
        // Consume the data so it can't be optimized away
        outData <= outData ^ d;

        // Dequeue from pipelines sequentially
        if (pipeIdx == maxBound)
        begin
            if (pipeTrips == maxBound)
            begin
                state <= STATE_finished;
            end

            pipeTrips <= pipeTrips + 1;
        end

        pipeIdx <= pipeIdx + 1;
    endrule


    // ====================================================================
    //
    // End of program.
    //
    // ====================================================================

    rule sendDone (state == STATE_finished);
        Bit#(64) d = zeroExtend(outData);

        // Write the data so it can't be optimized away
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_PIPETEST,
                                            stringID: `STREAMS_PIPETEST_DONE,
                                            payload0: d[63:32],
                                            payload1: d[31:0] });
        state <= STATE_exit;
    endrule

    rule finished (state == STATE_exit);
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_NULL,
                                            stringID: `STREAMS_MESSAGE_EXIT,
                                            payload0: 0,
                                            payload1: 0 });
    endrule

endmodule


// ========================================================================
//
// Implementation of the pipelines in which a single rule manages all
// pipelines for a given stage.
//
// ========================================================================

module mkSharedPipeTest
    // interface:
    (PIPELINE_TEST#(n_STAGES, n_PARALLEL_PIPES));
    
    //
    // FIFOs
    //
    Vector#(n_STAGES, Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA))) fifos <-
        replicateM(replicateM(mkFIFOF()));

    //
    // Parallel pipelines must be written round-robin.  These control registers
    // manage which of the parallel pipelines are read in each stage.
    //
    Vector#(n_STAGES, Reg#(Bit#(TLog#(n_PARALLEL_PIPES)))) curPipe <- replicateM(mkReg(0));

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        rule pipeStage (True);
            let d = fifos[s][curPipe[s]].first();
            fifos[s][curPipe[s]].deq();

            fifos[s + 1][curPipe[s]].enq(d);
            curPipe[s] <= curPipe[s] + 1;
        endrule
    end

    //
    // Methods
    //

    Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA)) pipesLocal = newVector();

    for (Integer p = 0; p < valueOf(n_PARALLEL_PIPES); p = p + 1)
    begin
        pipesLocal[p] =
            interface FIFOF#(PIPE_TEST_DATA);
                method Action enq(PIPE_TEST_DATA d) = fifos[0][p].enq(d);

                method Action deq() = fifos[valueOf(n_STAGES) - 1][p].deq();
                method PIPE_TEST_DATA first() = fifos[valueOf(n_STAGES) - 1][p].first();

                method Bool notFull() = fifos[0][p].notFull();
                method Bool notEmpty() = fifos[valueOf(n_STAGES) - 1][p].notEmpty();

                method Action clear();
                    noAction;
                endmethod
            endinterface;
    end

    interface pipes = pipesLocal;
endmodule


// ========================================================================
//
// Implementation of the pipelines in which a single rule manages all
// pipelines for a given stage.  The destination FIFO is a function of the
// data, so the network is a tree instead of individual pipelines.
//
// ========================================================================

module mkSharedTreePipeTest
    // interface:
    (PIPELINE_TEST#(n_STAGES, n_PARALLEL_PIPES));
    
    //
    // FIFOs
    //
    Vector#(n_STAGES, Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA))) fifos <-
        replicateM(replicateM(mkFIFOF()));

    //
    // Parallel pipelines must be written round-robin.  These control registers
    // manage which of the parallel pipelines are read in each stage.
    //
    Vector#(n_STAGES, Reg#(Bit#(TLog#(n_PARALLEL_PIPES)))) curPipe <- replicateM(mkReg(0));

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        rule pipeStage (True);
            let d = fifos[s][curPipe[s]].first();
            fifos[s][curPipe[s]].deq();

            Bit#(TLog#(n_PARALLEL_PIPES)) tgt = truncateNP(d);
            fifos[s + 1][tgt].enq(d);
            curPipe[s] <= curPipe[s] + 1;
        endrule
    end

    //
    // Methods
    //

    Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA)) pipesLocal = newVector();

    for (Integer p = 0; p < valueOf(n_PARALLEL_PIPES); p = p + 1)
    begin
        pipesLocal[p] =
            interface FIFOF#(PIPE_TEST_DATA);
                method Action enq(PIPE_TEST_DATA d) = fifos[0][p].enq(d);

                method Action deq() = fifos[valueOf(n_STAGES) - 1][p].deq();
                method PIPE_TEST_DATA first() = fifos[valueOf(n_STAGES) - 1][p].first();

                method Bool notFull() = fifos[0][p].notFull();
                method Bool notEmpty() = fifos[valueOf(n_STAGES) - 1][p].notEmpty();

                method Action clear();
                    noAction;
                endmethod
            endinterface;
    end

    interface pipes = pipesLocal;
endmodule


// ========================================================================
//
// Implementation of the pipelines in which each pipeline in each stage has
// its own, private, rule.
//
// ========================================================================

module mkPrivatePipeTest
    // interface:
    (PIPELINE_TEST#(n_STAGES, n_PARALLEL_PIPES));
    
    //
    // FIFOs
    //
    Vector#(n_STAGES, Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA))) fifos <-
        replicateM(replicateM(mkFIFOF()));

    //
    // Parallel pipelines must be written round-robin.  These control registers
    // manage which of the parallel pipelines are read in each stage.
    //
    Vector#(n_STAGES, Reg#(Bit#(TLog#(n_PARALLEL_PIPES)))) curPipe <- replicateM(mkReg(0));

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        for (Integer p = 0; p < valueOf(n_PARALLEL_PIPES); p = p + 1)
        begin
            rule pipeStage (curPipe[s] == fromInteger(p));
                let d = fifos[s][p].first();
                fifos[s][p].deq();

                fifos[s + 1][p].enq(d);
                curPipe[s] <= curPipe[s] + 1;
            endrule
        end
    end

    //
    // Methods
    //

    Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA)) pipesLocal = newVector();

    for (Integer p = 0; p < valueOf(n_PARALLEL_PIPES); p = p + 1)
    begin
        pipesLocal[p] =
            interface FIFOF#(PIPE_TEST_DATA);
                method Action enq(PIPE_TEST_DATA d) = fifos[0][p].enq(d);

                method Action deq() = fifos[valueOf(n_STAGES) - 1][p].deq();
                method PIPE_TEST_DATA first() = fifos[valueOf(n_STAGES) - 1][p].first();

                method Bool notFull() = fifos[0][p].notFull();
                method Bool notEmpty() = fifos[valueOf(n_STAGES) - 1][p].notEmpty();

                method Action clear();
                    noAction;
                endmethod
            endinterface;
    end

    interface pipes = pipesLocal;
endmodule
