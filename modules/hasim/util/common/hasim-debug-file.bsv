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

// Debugging facilities for software simulation.

import Vector::*;

// All debug output files go in a subdirectory
function String debugPath(String fname) = `DEBUG_LOG_DIR + "/" + fname;


// ========================================================================
//
//   Debug file with model cycle.  No thread context.
//
// ========================================================================

// TIMEP_DEBUG_FILE

// A debug file which has an idea of model cycle versus FPGA.

interface TIMEP_DEBUG_FILE;

    // Normal message
    method Action record(Fmt fmt);

    // Hack for printing message with model cycle number + 1.  This can be useful
    // when it is most convent to increment the model cycle at the head of a rule
    // and have debug log messages in the same rule that logically belong with
    // the next incremented cycle number.
    method Action record_next_cycle(Fmt fmt);

    method Action nextModelCycle();

endinterface


// mkTIMEPDebugFile

// Standard simulation debugging file for the timing partition.

module mkTIMEPDebugFile#(String fname)
    // interface:
        (TIMEP_DEBUG_FILE);

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

    method Action record(Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpga_cycle.value(), model_cycle.value()) + fmt);
    endmethod

    method Action record_next_cycle(Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpga_cycle.value(), model_cycle.value() + 1) + fmt);
    endmethod

    method Action nextModelCycle() if (initialized);
        model_cycle.up();
    endmethod

endmodule
    

    
// ========================================================================
//
//   Debug file with model cycle and thread context.
//
// ========================================================================

// TIMEP_DEBUG_FILE_MULTICTX

// A debug file which has an idea of model cycle versus FPGA and multiple
// contexts.

interface TIMEP_DEBUG_FILE_MULTICTX;

    // Normal message
    method Action record(CONTEXT_ID ctxId, Fmt fmt);

    // Hack for printing message with model cycle number + 1.  This can be useful
    // when it is most convent to increment the model cycle at the head of a rule
    // and have debug log messages in the same rule that logically belong with
    // the next incremented cycle number.
    method Action record_next_cycle(CONTEXT_ID ctxId, Fmt fmt);

    // Simple message (no context or model cycle)
    method Action record_simple(Fmt fmt);

    // Emit message for all contexts
    method Action record_all(Fmt fmt);

    method Action nextModelCycle(CONTEXT_ID ctxId);

endinterface


// mkTIMEPDebugFile_MultiCtx

// Standard simulation debugging file for the timing partition.

module mkTIMEPDebugFile_MultiCtx#(String fname)
    // interface:
        (TIMEP_DEBUG_FILE_MULTICTX);

    COUNTER#(32) fpga_cycle  <- mkLCounter(0);

    Vector#(NUM_CONTEXTS, COUNTER#(32)) model_cycle = newVector();
    for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
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

    method Action record(CONTEXT_ID ctxId, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpga_cycle.value(), ctxId, model_cycle[ctxId].value()) + fmt);
    endmethod

    method Action record_next_cycle(CONTEXT_ID ctxId, Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpga_cycle.value(), ctxId, model_cycle[ctxId].value() + 1) + fmt);
    endmethod

    method Action record_simple(Fmt fmt);
        $fdisplay(debugLog, $format("[%d]: ", fpga_cycle.value()) + fmt);
    endmethod

    method Action record_all(Fmt fmt);
        for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
        begin
            CONTEXT_ID ctx_id = fromInteger(c);
            $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpga_cycle.value(), ctx_id, model_cycle[ctx_id].value()) + fmt);
        end
    endmethod

    method Action nextModelCycle(CONTEXT_ID ctxId) if (initialized);
        model_cycle[ctxId].up();
    endmethod

endmodule
