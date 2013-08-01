//
// Copyright (C) 2013 Intel Corporation
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
// Sparse delay ports impose latency that may be greater than the buffered
// capacity of the port.  These may be used to simulate long delays where
// the number of messages in flight is less than the delay.
//
// Only receivers are declared here, since latency is always imposed by
// the receiver.
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
    MEMORY_IFC#(t_SHARED_FIFO_IDX, t_FIFO_DATA) fifoData <- mkBRAM();

    LUTRAM#(INSTANCE_ID#(t_NUM_INSTANCES),
            Tuple2#(Bool, Bit#(TLog#(t_MAX_LATENCY)))) portEnqState <-
        mkLUTRAM(tuple2(True, 0));


    method Bool canEnq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
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
        fifoData.write(tuple2(iid, idx), tuple2(cycles_since_last_msg, msg));
    
        // Write back the metadata associated with the port's FIFO
        fifoMeta.upd(iid, fifo_meta_new);
        portEnqState.upd(iid, tuple2(funcFIFO_IDX_notFull(fifo_meta_new), 0));
    endmethod

    method Action noEnq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        match {.can_enq, .cycles_since_last_msg} = portEnqState.sub(iid);

        if (cycles_since_last_msg < latency)
        begin
            cycles_since_last_msg = cycles_since_last_msg + 1;
        end

        portEnqState.upd(iid, tuple2(can_enq, cycles_since_last_msg));
    endmethod


    method Action doDeq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    endmethod

    method Action noDeq(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    endmethod

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        return ?;
    endmethod
endmodule
