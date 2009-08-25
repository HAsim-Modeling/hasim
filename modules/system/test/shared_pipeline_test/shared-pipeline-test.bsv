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

import Vector::*;
import FIFOF::*;
import GetPut::*;
import LFSR::*;

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"

`include "asim/provides/pipetest_common.bsh"
`include "asim/provides/pipeline_test.bsh"

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


module [CONNECTED_MODULE] mkSystem ();

    Reg#(STATE) state <- mkReg(STATE_init);

    // Streams (output)
    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    // Instantiate the test pipelines
    PIPELINE_TEST#(`PIPE_TEST_STAGES, `PIPE_TEST_NUM_PIPES) pipes <- mkPipeTest();

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
        // Data driven routing.  Low bits of data indicate path.  Add two
        // numbers together so it isn't a constant.
        PIPELINE_IDX tgt = pipeIdx + zeroExtend(pipeTrips);
        v = truncate({v0, v1, tgt});

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

