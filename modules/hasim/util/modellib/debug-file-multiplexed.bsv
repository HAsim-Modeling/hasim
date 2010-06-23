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

`include "asim/provides/hasim_common.bsh"

// ========================================================================
//
//   Interfaces for various debug file with model cycle and multiple
//   instances.
//
// ========================================================================

//
// TIMEP_DEBUG_FILE_MULTIPLEXED
//
// A debug file which has an idea of model cycle versus FPGA and multiple
// instances. ni is the number of instances multiplexed over.
//
interface TIMEP_DEBUG_FILE_MULTIPLEXED#(type ni);
    // Normal message
    method Action record(INSTANCE_ID#(ni) iid, Fmt fmt);

    // Hack for printing message with model cycle number + 1.  This can be useful
    // when it is most convent to increment the model cycle at the head of a rule
    // and have debug log messages in the same rule that logically belong with
    // the next incremented cycle number.
    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt);

    // Simple message (no context or model cycle)
    method Action record_simple(Fmt fmt);

    // Emit message for all contexts
    method Action record_all(Fmt fmt);

    method Action nextModelCycle(INSTANCE_ID#(ni) iid);
endinterface


//
// TIMEP_DEBUG_FILE_SHARED_CYCLE_MULTIPLEXED
//
// Similar to TIMEP_DEBUG_FILE_MULTIPLEXED by all instances execute the same
// simulated cycle.
//
interface TIMEP_DEBUG_FILE_SHARED_CYCLE_MULTIPLEXED#(type ni);
    // Normal message
    method Action record(INSTANCE_ID#(ni) iid, Fmt fmt);

    // Hack for printing message with model cycle number + 1.  This can be useful
    // when it is most convent to increment the model cycle at the head of a rule
    // and have debug log messages in the same rule that logically belong with
    // the next incremented cycle number.
    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt);

    // Simple message (no context or model cycle)
    method Action record_simple(Fmt fmt);

    // Emit message for all contexts
    method Action record_all(Fmt fmt);
    method Action record_all_next_cycle(Fmt fmt);

    method Action nextModelCycle();
endinterface


// ========================================================================
//
//   Implementations.
//
// ========================================================================

//
// Null implementation
//
module mkTIMEPDebugFileNull_Multiplexed
    // interface:
    (TIMEP_DEBUG_FILE_MULTIPLEXED#(ni));

    method Action record(INSTANCE_ID#(ni) iid, Fmt fmt) = ?;
    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt) = ?;
    method Action record_simple(Fmt fmt) = ?;
    method Action record_all(Fmt fmt) = ?;
    method Action nextModelCycle(INSTANCE_ID#(ni) iid) = ?;
endmodule


//
// mkTIMEPDebugFile_Multiplexed
//
// Standard simulation debugging file for the timing partition where
// contexts have their own cycle counters.
//
module mkTIMEPDebugFile_Multiplexed#(String fname)
    // interface:
    (TIMEP_DEBUG_FILE_MULTIPLEXED#(ni));

`ifndef SYNTH

    COUNTER#(32) fpga_cycle  <- mkLCounter(0);

    Vector#(ni, COUNTER#(32)) model_cycle = newVector();
    for (Integer c = 0; c < valueOf(ni); c = c + 1)
    begin
        model_cycle[c] <- mkLCounter(~0);
    end

    Reg#(File) debugLog <- mkReg(InvalidFile);
    Reg#(Bool) initialized <- mkReg(False);

    rule open (initialized == False);
        let fd <- $fopen(debugPath(fname), "w");
        if (fd == InvalidFile)
        begin
            $display("Error opening debugging logfile " + debugPath(fname));
            $finish(1);
        end

        debugLog <= fd;
        initialized <= True;
    endrule

    rule inc (True);
        fpga_cycle.up();
    endrule

    method Action record(INSTANCE_ID#(ni) iid, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpga_cycle.value(), iid, model_cycle[iid].value()) + fmt);
    endmethod

    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpga_cycle.value(), iid, model_cycle[iid].value() + 1) + fmt);
    endmethod

    method Action record_simple(Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: ", fpga_cycle.value()) + fmt);
    endmethod

    method Action record_all(Fmt fmt);
        for (Integer c = 0; c < valueOf(ni); c = c + 1)
        begin
            INSTANCE_ID#(ni) iid = fromInteger(c);
            $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpga_cycle.value(), iid, model_cycle[iid].value()) + fmt);
        end
    endmethod

    method Action nextModelCycle(INSTANCE_ID#(ni) iid) if (initialized);
        model_cycle[iid].up();
    endmethod

`else

    // No point in wasting space on debug file for synthesized build.  Xst
    // doesn't get rid of it all.
    TIMEP_DEBUG_FILE_MULTIPLEXED#(ni) n <- mkTIMEPDebugFileNull_Multiplexed();
    return n;

`endif

endmodule



// ========================================================================
//
//   Debug file with model cycle and thread contexts.
//
// ========================================================================

// TIMEP_DEBUG_FILE_MULTICTX
typedef TIMEP_DEBUG_FILE_MULTIPLEXED#(NUM_CONTEXTS) TIMEP_DEBUG_FILE_MULTICTX;

// mkTIMEPDebugFile_MultiCtx

// Standard simulation debugging file for the timing partition.
// Special case of Multiplexed where the number of instances is NUM_CONTEXTS
module mkTIMEPDebugFile_MultiCtx#(String fname)
    // interface:
    (TIMEP_DEBUG_FILE_MULTICTX);

    TIMEP_DEBUG_FILE_MULTICTX ifc <- mkTIMEPDebugFile_Multiplexed(fname);
    return ifc;
endmodule



// ========================================================================
//
//   Debug file with model with shared cycles.
//
// ========================================================================

//
// Null implementation
//
module mkTIMEPDebugFileNull_SharedCycle_Multiplexed
    // interface:
    (TIMEP_DEBUG_FILE_SHARED_CYCLE_MULTIPLEXED#(ni));

    method Action record(INSTANCE_ID#(ni) iid, Fmt fmt) = ?;
    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt) = ?;
    method Action record_simple(Fmt fmt) = ?;
    method Action record_all(Fmt fmt) = ?;
    method Action record_all_next_cycle(Fmt fmt) = ?;
    method Action nextModelCycle() = ?;
endmodule


//
// mkTIMEPDebugFile_SharedCycle_MultiCtx --
//     All contexts are locked to the same cycle.  Basically the same code
//     as above, but only keep one copy of the model cycle.
//
module mkTIMEPDebugFile_SharedCycle_Multiplexed#(String fname)
    // interface:
    (TIMEP_DEBUG_FILE_SHARED_CYCLE_MULTIPLEXED#(ni));

`ifndef SYNTH

    COUNTER#(32) fpga_cycle  <- mkLCounter(0);
    COUNTER#(32) model_cycle <- mkLCounter(~0);

    Reg#(File) debugLog <- mkReg(InvalidFile);
    Reg#(Bool) initialized <- mkReg(False);

    rule open (initialized == False);
        let fd <- $fopen(debugPath(fname), "w");
        if (fd == InvalidFile)
        begin
            $display("Error opening debugging logfile " + debugPath(fname));
            $finish(1);
        end

        debugLog <= fd;
        initialized <= True;
    endrule

    rule inc (True);
        fpga_cycle.up();
    endrule

    method Action record(INSTANCE_ID#(ni) iid, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpga_cycle.value(), iid, model_cycle.value()) + fmt);
    endmethod

    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpga_cycle.value(), iid, model_cycle.value() + 1) + fmt);
    endmethod

    method Action record_simple(Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: ", fpga_cycle.value()) + fmt);
    endmethod

    method Action record_all(Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpga_cycle.value(), model_cycle.value()) + fmt);
    endmethod

    method Action record_all_next_cycle(Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpga_cycle.value(), model_cycle.value() + 1) + fmt);
    endmethod

    method Action nextModelCycle() if (initialized);
        model_cycle.up();
    endmethod

`else

    // No point in wasting space on debug file for synthesized build.  Xst
    // doesn't get rid of it all.
    TIMEP_DEBUG_FILE_SHARED_CYCLE_MULTIPLEXED#(ni) n <-
        mkTIMEPDebugFileNull_SharedCycle_Multiplexed();
     return n;

`endif

endmodule
