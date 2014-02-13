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

    // Use record() instead!
    //
    // This method used to add 1 to the model cycle before printing and was
    // used in the same cycle that nextModelCycle() was called.  The
    // implementations are now more clever and this method is no longer
    // necessary.  The rule remains for compatibility, but is identical
    // to record().
    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt);

    // Simple message (no context or model cycle)
    method Action record_simple(Fmt fmt);

    // Simple message (no model cycle)
    method Action record_simple_ctx(INSTANCE_ID#(ni) iid, Fmt fmt);

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

    // Use record() instead!
    //
    // This method used to add 1 to the model cycle before printing and was
    // used in the same cycle that nextModelCycle() was called.  The
    // implementations are now more clever and this method is no longer
    // necessary.  The rule remains for compatibility, but is identical
    // to record().
    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt);

    // Simple message (no context or model cycle)
    method Action record_simple(Fmt fmt);

    // Simple message (no model cycle)
    method Action record_simple_ctx(INSTANCE_ID#(ni) iid, Fmt fmt);

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
    method Action record_simple_ctx(INSTANCE_ID#(ni) iid, Fmt fmt) = ?;
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

    COUNTER#(32) fpgaCycle  <- mkLCounter(0);

    Vector#(ni, COUNTER#(32)) modelCycle = newVector();
    for (Integer iid = 0; iid < valueOf(ni); iid = iid + 1)
    begin
        modelCycle[iid] <- mkLCounter(~0);
    end

    Reg#(File) debugLog <- mkReg(InvalidFile);
    Reg#(Bool) initialized <- mkReg(False);

    function getModelCycle(INSTANCE_ID#(ni) iid) = modelCycle[iid].updatedValue();

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
        fpgaCycle.up();
    endrule

    method Action record(INSTANCE_ID#(ni) iid, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpgaCycle.value(), iid, getModelCycle(iid)) + fmt);
    endmethod

    // Now equivalent to record().  See interface for details.
    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpgaCycle.value(), iid, getModelCycle(iid)) + fmt);
    endmethod

    method Action record_simple(Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: ", fpgaCycle.value()) + fmt);
    endmethod

    method Action record_simple_ctx(INSTANCE_ID#(ni) iid, Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpgaCycle.value(), iid) + fmt);
    endmethod

    method Action record_all(Fmt fmt);
        for (Integer c = 0; c < valueOf(ni); c = c + 1)
        begin
            INSTANCE_ID#(ni) iid = fromInteger(c);
            $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpgaCycle.value(), iid, getModelCycle(iid)) + fmt);
        end
    endmethod

    method Action nextModelCycle(INSTANCE_ID#(ni) iid) if (initialized);
        modelCycle[iid].up();
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
    method Action record_simple_ctx(INSTANCE_ID#(ni) iid, Fmt fmt) = ?;
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

    COUNTER#(32) fpgaCycle  <- mkLCounter(0);
    COUNTER#(32) modelCycle <- mkLCounter(~0);

    Reg#(File) debugLog <- mkReg(InvalidFile);
    Reg#(Bool) initialized <- mkReg(False);

    function getModelCycle() = modelCycle.updatedValue();

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
        fpgaCycle.up();
    endrule

    method Action record(INSTANCE_ID#(ni) iid, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpgaCycle.value(), iid, getModelCycle()) + fmt);
    endmethod

    // Identical to record().  See interface for details.
    method Action record_next_cycle(INSTANCE_ID#(ni) iid, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpgaCycle.value(), iid, getModelCycle()) + fmt);
    endmethod

    method Action record_simple(Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: ", fpgaCycle.value()) + fmt);
    endmethod

    method Action record_simple_ctx(INSTANCE_ID#(ni) iid, Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpgaCycle.value(), iid) + fmt);
    endmethod

    method Action record_all(Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpgaCycle.value(), getModelCycle()) + fmt);
    endmethod

    // Identical to record_all().  See interface for details.
    method Action record_all_next_cycle(Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpgaCycle.value(), getModelCycle()) + fmt);
    endmethod

    method Action nextModelCycle() if (initialized);
        modelCycle.up();
    endmethod

`else

    // No point in wasting space on debug file for synthesized build.  Xst
    // doesn't get rid of it all.
    TIMEP_DEBUG_FILE_SHARED_CYCLE_MULTIPLEXED#(ni) n <-
        mkTIMEPDebugFileNull_SharedCycle_Multiplexed();
     return n;

`endif

endmodule
