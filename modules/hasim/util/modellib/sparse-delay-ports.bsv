//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

//
// Sparse delay ports impose latency that may be greater than the buffered
// capacity of the port.  These may be used to simulate long delays where
// the number of messages in flight is less than the delay.
//

`include "awb/provides/librl_bsv_base.bsh"
`include "awb/provides/fpga_components.bsh"


interface PORT_SPARSE_DELAY_MULTIPLEXED#(numeric type t_NUM_INSTANCES,
                                         type t_MSG,
                                         numeric type t_ENTRIES_PER_INSTANCE,
                                         numeric type t_MAX_LATENCY);

    //
    // canEnq --
    //   Returns whether the port is full.  If true, either doEnq or noEnq
    //   may be called.  If false, noEnq must be called during this instance's
    //   simulated model cycle.
    //
    method Bool canEnq(INSTANCE_ID#(t_NUM_INSTANCES) iid);

    //
    // doEnq --
    //   Unguarded enque of a new message.  canEnq() must be true to call
    //   this.  Only one of doEnq() and noEnq() should be called for a given
    //   instance in a given model cycle.
    //
    method Action doEnq(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_MSG msg);

    //
    // noEnq --
    //   Called when no message will be enqueued with doEnq() in a given model
    //   cycle for an instance.
    //
    method Action noEnq(INSTANCE_ID#(t_NUM_INSTANCES) iid);


    method Action doDeq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    method Action noDeq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) iid);
endinterface


module [CONNECTED_MODULE] mkPortSparseDelay_Multiplexed#(
    t_LATENCY latency)
    // Interface:
    (PORT_SPARSE_DELAY_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG,
                                    t_ENTRIES_PER_INSTANCE, t_MAX_LATENCY))
    provisos (Bits#(t_MSG, t_MSG_SZ),

              // Representation of latency as bits
              Alias#(t_LATENCY, Bit#(TLog#(t_MAX_LATENCY))),

              // Meta-data for each instance's FIFO
              Alias#(t_FIFO_META, FUNC_FIFO_IDX#(t_ENTRIES_PER_INSTANCE)),

              // Global index of buffered port entries with all multiplexed
              // ports sharing a single address space.
              Alias#(t_SHARED_FIFO_IDX,
                     Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES),
                             Bit#(TLog#(t_ENTRIES_PER_INSTANCE)))),
              // Data stored in the FIFO
              Alias#(t_FIFO_DATA, Tuple2#(t_LATENCY, t_MSG)));

    // Port (FIFO) metadata
    LUTRAM#(INSTANCE_ID#(t_NUM_INSTANCES), t_FIFO_META) fifoMeta <-
        mkLUTRAM(funcFIFO_IDX_Init());

    // Port (FIFO) contents
    LUTRAM#(t_SHARED_FIFO_IDX, t_FIFO_DATA) fifoData <- mkLUTRAMU();

    LUTRAM#(INSTANCE_ID#(t_NUM_INSTANCES),
            Tuple2#(Bool, t_LATENCY)) portEnqState <-
        mkLUTRAM(tuple2(True, latency));

    LUTRAM#(INSTANCE_ID#(t_NUM_INSTANCES), t_LATENCY) portDeqState <- mkLUTRAM(0);


    method Bool canEnq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        // Use a cached copy of the notFull state to avoid adding another
        // read port to fifoMeta.
        return tpl_1(portEnqState.sub(iid));
    endmethod

    method Action doEnq(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_MSG msg);
        //
        // Read current state of the inbound port.  Pipeline latency is
        // simulated by counting the number of cycles since the last message
        // was enqueued.  The receiver will have to wait at least this many
        // cycles before forwarding the message.
        //
        let cycles_since_last_msg = tpl_2(portEnqState.sub(iid));
        let fifo_meta = fifoMeta.sub(iid);

        // Update the FIFO with the new message.
        match {.fifo_meta_new, .idx} = funcFIFO_IDX_UGenq(fifo_meta);

        //
        // Impose a latency.  If there are other messages in flight then
        // the latency is a pipelined latency:  the number of cycles since
        // the older message was queued.
        //
        let delay = cycles_since_last_msg;

        fifoData.upd(tuple2(iid, idx), tuple2(delay, msg));
    
        // Write back the metadata associated with the port's FIFO
        fifoMeta.upd(iid, fifo_meta_new);
        portEnqState.upd(iid, tuple2(funcFIFO_IDX_notFull(fifo_meta_new), 0));
    endmethod

    method Action noEnq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        match {.can_enq, .cycles_since_last_msg} = portEnqState.sub(iid);
        let fifo_meta = fifoMeta.sub(iid);

        if (cycles_since_last_msg < latency)
        begin
            cycles_since_last_msg = cycles_since_last_msg + 1;
        end

        portEnqState.upd(iid, tuple2(funcFIFO_IDX_notFull(fifo_meta),
                                     cycles_since_last_msg));
    endmethod


    method Action doDeq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        let fifo_meta = fifoMeta.sub(iid);
        fifoMeta.upd(iid, funcFIFO_IDX_UGdeq(fifo_meta));
        portDeqState.upd(iid, 0);
    endmethod

    method Action noDeq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        let fifo_meta = fifoMeta.sub(iid);

        // If there is a message waiting then update the latency counter to
        // simulate delay.
        let idle_cycles = portDeqState.sub(iid);
        if (funcFIFO_IDX_notEmpty(fifo_meta))
        begin
            if (idle_cycles < latency)
            begin
                idle_cycles = idle_cycles + 1;
            end
        end
        else
        begin
            // No message in flight.
            idle_cycles = 0;
        end

        portDeqState.upd(iid, idle_cycles);
    endmethod

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        let fifo_meta = fifoMeta.sub(iid);
        let idx = funcFIFO_IDX_UGfirst(fifo_meta);
        match {.delay, .msg} = fifoData.sub(tuple2(iid, idx));

        let idle_cycles = portDeqState.sub(iid);

        if (funcFIFO_IDX_notEmpty(fifo_meta) &&
            (idle_cycles >= delay))
        begin
            return tagged Valid msg;
        end
        else
        begin
            return tagged Invalid;
        end
    endmethod
endmodule
