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

`include "awb/provides/model_params.bsh"


//
// STDIO conditional printf bit mask allocation.  (See mkStdIO_CondPrintf)
//
Integer ioMask_FUNCP_REGMGR = 0;
Integer ioMask_FUNCP_MEMSTATE = 1;

// First position available to the timing partition.  Leave it to the timing
// partition to expand the space.
Integer ioMask_TIMEP_START = 8;


// All debug output files go in a subdirectory
function String debugPath(String fname) = `LEAP_DEBUG_PATH + "/" + fname;


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

    // Use record() instead!
    //
    // This method used to add 1 to the model cycle before printing and was
    // used in the same cycle that nextModelCycle() was called.  The
    // implementations are now more clever and this method is no longer
    // necessary.  The rule remains for compatibility, but is identical
    // to record().
    method Action record_next_cycle(Fmt fmt);

    method Action nextModelCycle();

endinterface


//
// mkTIMEPDebugFileNull --
//     Null debug file, will drop everything on the floor. 
//
module mkTIMEPDebugFileNull#(String fname)
    // interface:
    (TIMEP_DEBUG_FILE);

    method record = ?;
    method record_next_cycle = ?;
    method nextModelCycle = ?;
endmodule


//
// mkTIMEPDebugFile --
//
//     Standard simulation debugging file for the timing partition.
//
module mkTIMEPDebugFile#(String fname)
    // interface:
    (TIMEP_DEBUG_FILE);

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

    method Action record(Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpgaCycle.value(), getModelCycle()) + fmt);
    endmethod

    // Now equivalent to record().  See interface for details.
    method Action record_next_cycle(Fmt fmt) if (initialized);
        $fdisplay(debugLog, $format("[%d]: <%d>: ", fpgaCycle.value(), getModelCycle()) + fmt);
    endmethod

    method Action nextModelCycle() if (initialized);
        modelCycle.up();
    endmethod
    
`else

    // No point in wasting space on debug file for synthesized build.  Xst
    // doesn't get rid of it all.
    TIMEP_DEBUG_FILE n <- mkTIMEPDebugFileNull(fname);
    return n;

`endif

endmodule
