//Debugging facilities for software simulation.

import Counter::*;

// DEBUG_FILE

// A wrapper for a simulation debugging file.

interface DEBUG_FILE;

    method Action record(Fmt fmt);

endinterface


// mkDebugFile

// Standard simulation debugging file.

module mkDebugFile#(String fname)
    // interface:
        (DEBUG_FILE);

    Counter#(32)  fpga_cycle <- mkCounter(0);

    Reg#(File) debugLog <- mkReg(InvalidFile);
    Reg#(Bool) initialized <- mkReg(False);

    rule open (initialized == False);

        if (debugLog == InvalidFile)
        begin
            let fd <- $fopen(fname, "w");

            if (fd == InvalidFile)
            begin
                $display("Error opening debugging logfile " + fname);
                $finish(1);
            end

            debugLog <= fd;
        end
        
        initialized <= True;

    endrule

    rule inc (True);

        fpga_cycle.up();

    endrule

    method Action record(Fmt fmt) if (initialized);

        $fdisplay(debugLog, $format("[%d]: ", fpga_cycle.value()) + fmt);

    endmethod

endmodule



// TIMEP_DEBUG_FILE

// A debug file which has an idea of model cycle versus FPGA.

interface TIMEP_DEBUG_FILE;

    method Action record(Fmt fmt);
    method Action nextModelCycle();

endinterface


// mkTIMEPDebugFile

// Standard simulation debugging file for the timing partition.

module mkTIMEPDebugFile#(String fname)
    // interface:
        (TIMEP_DEBUG_FILE);

    Counter#(32)  fpga_cycle  <- mkCounter(0);
    Counter#(32)  model_cycle <- mkCounter(0);

    Reg#(File) debugLog <- mkReg(InvalidFile);
    Reg#(Bool) initialized <- mkReg(False);

    rule open (initialized == False);

        if (debugLog == InvalidFile)
        begin
            let fd <- $fopen(fname, "w");

            if (fd == InvalidFile)
            begin
                $display("Error opening debugging logfile " + fname);
                $finish(1);
            end

            debugLog <= fd;
        end
        
        initialized <= True;

    endrule

    rule inc (True);

        fpga_cycle.up();

    endrule

    method Action record(Fmt fmt) if (initialized);

        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpga_cycle.value(), model_cycle.value()) + fmt);

    endmethod

    method Action nextModelCycle() if (initialized);
        model_cycle.up();
    endmethod

endmodule
