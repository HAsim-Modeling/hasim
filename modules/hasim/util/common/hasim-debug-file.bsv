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

// Debugging facilities for software simulation.

import Vector::*;

`include "awb/provides/model_params.bsh"
`include "awb/provides/librl_bsv_base.bsh"


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

`ifdef SYNTH_Z

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
