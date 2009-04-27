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

function PIPELINE_IDX getPipeIdx(PIPE_TEST_DATA d);

    return truncateNP(d);

endfunction

interface PIPELINE_TEST#(numeric type n_STAGES, numeric type n_PARALLEL_PIPES);
    interface Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA)) pipes;
endinterface: PIPELINE_TEST

module mkTestFIFOF (FIFOF#(t)) provisos (Bits#(t, t_SZ));

    FIFOF#(t) q <- (`PIPE_TEST_GUARDED_FIFOS != 0) ? mkFIFOF() : mkUGFIFOF();

    return q;

endmodule


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
    else if (`PIPE_TEST_LOOP_MODE == 2)
        pipes <- mkPrivatePipeTest();
    else if (`PIPE_TEST_LOOP_MODE == 3)
        pipes <- mkDuplicatedPipeTest();
    else if (`PIPE_TEST_LOOP_MODE == 4)
        pipes <- mkPipelineStageControllerTest();
    else
        pipes <- error("ERROR: PIPE_TEST_LOOP_MODE out of bounds.");

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

    rule doEnq (state == STATE_enq  && pipes.pipes[pipeIdx].notFull());
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

    rule doDeq (state == STATE_deq && pipes.pipes[pipeIdx].notEmpty());
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
        replicateM(replicateM(mkTestFIFOF()));

    //
    // Parallel pipelines must be written round-robin.  These control registers
    // manage which of the parallel pipelines are read in each stage.
    //
    Vector#(n_STAGES, Reg#(Bit#(TLog#(n_PARALLEL_PIPES)))) curPipe <- replicateM(mkReg(0));

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        rule pipeStage (fifos[s][curPipe[s]].notEmpty() && fifos[s + 1][curPipe[s]].notFull());
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
        replicateM(replicateM(mkTestFIFOF()));

    //
    // Parallel pipelines must be written round-robin.  These control registers
    // manage which of the parallel pipelines are read in each stage.
    //
    Vector#(n_STAGES, Reg#(Bit#(TLog#(n_PARALLEL_PIPES)))) curPipe <- replicateM(mkReg(0));

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        rule pipeStage (fifos[s][curPipe[s]].first() matches .d &&& getPipeIdx(d) matches .tgt &&& fifos[s][curPipe[s]].notEmpty() &&& fifos[s + 1][tgt].notFull());
            fifos[s][curPipe[s]].deq();
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
        replicateM(replicateM(mkTestFIFOF()));

    //
    // Parallel pipelines must be written round-robin.  These control registers
    // manage which of the parallel pipelines are read in each stage.
    //
    Vector#(n_STAGES, Reg#(Bit#(TLog#(n_PARALLEL_PIPES)))) curPipe <- replicateM(mkReg(0));

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        for (Integer p = 0; p < valueOf(n_PARALLEL_PIPES); p = p + 1)
        begin
            rule pipeStage (curPipe[s] == fromInteger(p) && fifos[s][p].notEmpty() && fifos[s + 1][p].notFull());
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

// ========================================================================
//
// Implementation of the pipelines in which we just duplicate the pipeline.
//
// ========================================================================

module mkDuplicatedPipeTest
    // interface:
    (PIPELINE_TEST#(n_STAGES, n_PARALLEL_PIPES));
    
    //
    // FIFOs
    //
    Vector#(n_PARALLEL_PIPES, PIPELINE_TEST#(n_STAGES, 1)) dupedPipes <-
        replicateM(mkPipeTest());

    //
    // Methods
    //

    Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA)) pipesLocal = newVector();

    for (Integer p = 0; p < valueOf(n_PARALLEL_PIPES); p = p + 1)
    begin
        pipesLocal[p] =
            interface FIFOF#(PIPE_TEST_DATA);
                method Action enq(PIPE_TEST_DATA d) = dupedPipes[p].pipes[0].enq(d);

                method Action deq() = dupedPipes[p].pipes[0].deq();
                method PIPE_TEST_DATA first() = dupedPipes[p].pipes[0].first();

                method Bool notFull() = dupedPipes[p].pipes[0].notFull();
                method Bool notEmpty() = dupedPipes[p].pipes[0].notEmpty();

                method Action clear();
                    noAction;
                endmethod
            endinterface;
    end

    interface pipes = pipesLocal;

endmodule


module mkPipeTest (PIPELINE_TEST#(n_STAGES, 1));

    //
    // FIFOs
    //

    Vector#(n_STAGES, FIFOF#(PIPE_TEST_DATA)) fifos <- replicateM(mkTestFIFOF());

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        rule pipeStage (fifos[s].notEmpty() && fifos[s + 1].notFull());
            let d = fifos[s].first();
            fifos[s].deq();

            fifos[s + 1].enq(d);
        endrule
    end

    Vector#(1, FIFOF#(PIPE_TEST_DATA)) pipesLocal = newVector();

    //
    // Methods
    //

    pipesLocal[0] =
        interface FIFOF#(PIPE_TEST_DATA);
            method Action enq(PIPE_TEST_DATA d) = fifos[0].enq(d);

            method Action deq() = fifos[valueOf(n_STAGES) - 1].deq();
            method PIPE_TEST_DATA first() = fifos[valueOf(n_STAGES) - 1].first();

            method Bool notFull() = fifos[0].notFull();
            method Bool notEmpty() = fifos[valueOf(n_STAGES) - 1].notEmpty();

            method Action clear();
                noAction;
            endmethod
        endinterface;

    interface pipes = pipesLocal;

endmodule

// ========================================================================
//
// Implementation of the pipelines in which a single rule manages all
// pipelines for a given stage.
//
// ========================================================================

module mkPipelineStageControllerTest
    // interface:
    (PIPELINE_TEST#(n_STAGES, n_PARALLEL_PIPES));
    
    //
    // FIFOs
    //
    Vector#(n_STAGES, Vector#(n_PARALLEL_PIPES, FIFOF#(PIPE_TEST_DATA))) fifos <-
        replicateM(replicateM(mkTestFIFOF()));

    Vector#(n_STAGES, PIPELINE_STAGE_CONTROLLER#(n_PARALLEL_PIPES)) stageCtrl = newVector();

    for (Integer s = 0; s < valueOf(n_STAGES) - 1; s = s + 1)
    begin
        
        let inqs  = map(fifofToPortControl, fifos[s]);
        let outqs = map(fifofToPortControl, fifos[s+1]);
        
        stageCtrl[s] <- mkPipelineStageController(cons(inqs, nil), cons(outqs, nil), s == 0);
    
        rule pipeStage (True); // Note: no explicit conditions necessary. Guarded by nextReadyInstance.
            let iid <- stageCtrl[s].nextReadyInstance();
            let d = fifos[s][iid].first();
            fifos[s][iid].deq();

            fifos[s + 1][iid].enq(d);
            stageCtrl[s + 1].ready(iid);
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


typedef Bit#(TLog#(t_NUM_INSTANCES)) INSTANCE_ID#(type t_NUM_INSTANCES);

interface PORT_CONTROL;

    method Bool full();
    method Bool empty();

endinterface

function PORT_CONTROL fifofToPortControl(FIFOF#(a) q);

    return (interface PORT_CONTROL;
               method Bool full() = !q.notFull();
               method Bool empty() = !q.notEmpty();
            endinterface);

endfunction

interface PIPELINE_STAGE_CONTROLLER#(type t_NUM_INSTANCES);

    method ActionValue#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
    method Action ready(INSTANCE_ID#(t_NUM_INSTANCES) iid);

endinterface

module mkPipelineStageController#(Vector#(n, Vector#(ni, PORT_CONTROL)) inports, Vector#(m, Vector#(ni, PORT_CONTROL)) outports, Bool initRdy)
    //interface:
        (PIPELINE_STAGE_CONTROLLER#(ni));

    Vector#(ni, PulseWire)    startRunningW <- replicateM(mkPulseWire());
    Vector#(ni, PulseWire)      readyW <- replicateM(mkPulseWire());
    
    
    // Vector of ready instances.
    Reg#(Vector#(ni, Bool)) instanceReadies <- mkReg(replicate(initRdy));

    function Bool allTrue(Vector#(k, Bool) v);
        return foldr(\&& , True, v);
    endfunction

    // This function will determine the next instance in a non-round-robin manner when we're ready
    // to go that route. Currently this is unused.

    function Bool instanceReady(INSTANCE_ID#(ni) iid);
        
        Bool canRead  = True;
        Bool canWrite = True;

        // Can we read/write all of the ports?
        for (Integer x = 0; x < valueOf(n); x = x + 1)
            canRead = canRead && !inports[x][iid].empty();

        for (Integer x = 0; x < valueOf(m); x = x + 1)
            canWrite = canWrite && !outports[x][iid].full();

        // An instance is ready to go only if it's not currently running.
        return instanceReadies[iid] && canRead && canWrite;

    endfunction

    function Bool someInstanceReady();
        
        Bool res = False;

        for (Integer x = 0; x < valueof(ni); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) || res;
        end
        
        return res;
    
    endfunction


    rule updateReadies (True);
    
        Vector#(ni, Bool) new_readies = instanceReadies;

        for (Integer x = 0; x < valueOf(ni); x = x + 1)
        begin
            if (!instanceReadies[x] || startRunningW[x])
                new_readies[x] = readyW[x];
        end
        
        instanceReadies <= new_readies;
    
    endrule

    method ActionValue#(INSTANCE_ID#(ni)) nextReadyInstance() if (someInstanceReady());
    
        INSTANCE_ID#(ni) res = 0;

        for (Integer x = 0; x < valueof(ni); x = x + 1)
        begin
            res = instanceReady(fromInteger(x)) ? fromInteger(x) : res;
        end
        
        startRunningW[res].send();

        return res;

    endmethod

    method Action ready(INSTANCE_ID#(ni) iid);
    
        readyW[iid].send();
    
    endmethod
    
endmodule
