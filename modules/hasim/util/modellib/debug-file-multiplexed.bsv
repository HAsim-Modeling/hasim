`include "asim/provides/hasim_common.bsh"

// ========================================================================
//
//   Debug file with model cycle and multiple instances.
//
// ========================================================================

// TIMEP_DEBUG_FILE_MULTIPLEXED

// A debug file which has an idea of model cycle versus FPGA and multiple
// instances. ni is the number of instances multiplexed over.

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


// mkTIMEPDebugFile_Multiplexed

// Standard simulation debugging file for the timing partition.

module mkTIMEPDebugFile_Multiplexed#(String fname)
    // interface:
        (TIMEP_DEBUG_FILE_MULTIPLEXED#(ni));

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
        $fdisplay(debugLog, $format("[%d]: <%d / %d>: ", fpga_cycle.value(),
iid, model_cycle[iid].value()) + fmt);
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

