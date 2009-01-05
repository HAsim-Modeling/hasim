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

import Counter::*;

// DEBUG_FILE

// A wrapper for a simulation debugging file.

interface DEBUG_FILE;

    method Action record(Fmt fmt);

endinterface


// All debug output files go in a subdirectory
function String debugPath(String fname) = "hasim_debug/" + fname;


// mkDebugFile

// Standard simulation debugging file.

module mkDebugFile#(String fname)
    // interface:
        (DEBUG_FILE);

    COUNTER#(32)  fpga_cycle <- mkLCounter(0);

    Reg#(File) debugLog <- mkReg(InvalidFile);
    Reg#(Bool) initialized <- mkReg(False);

    rule open (initialized == False);

        if (debugLog == InvalidFile)
        begin
            let fd <- $fopen(debugPath(fname), "w");

            if (fd == InvalidFile)
            begin
                $display("Error opening debugging logfile " + debugPath(fname));
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

    COUNTER#(32)  fpga_cycle  <- mkLCounter(0);
    COUNTER#(32)  model_cycle <- mkLCounter(0);

    Reg#(File) debugLog <- mkReg(InvalidFile);
    Reg#(Bool) initialized <- mkReg(False);

    rule open (initialized == False);

        if (debugLog == InvalidFile)
        begin
            let fd <- $fopen(debugPath(fname), "w");

            if (fd == InvalidFile)
            begin
                $display("Error opening debugging logfile " + debugPath(fname));
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

    method Action record_next_cycle(Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpga_cycle.value(), model_cycle.value() + 1) + fmt);
    endmethod

    method Action nextModelCycle() if (initialized);
        model_cycle.up();
    endmethod

endmodule
