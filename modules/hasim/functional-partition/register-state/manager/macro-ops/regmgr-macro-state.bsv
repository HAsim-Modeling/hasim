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
// This code used to be much more complicated than it is now because there
// was a single finite state machine for the entire register state pipeline.
// Now that there are separate state machines for each pipeline the code
// here is responsible for global control:  blocking individual contexts
// from entering the functional pipelines during instruction emulation and
// rewind handling.
//

`include "awb/provides/soft_connections.bsh"
`include "awb/provides/soft_services_lib.bsh"
`include "awb/provides/soft_services.bsh"
`include "awb/provides/soft_services_deps.bsh"

`include "awb/provides/physical_platform.bsh"
`include "awb/provides/debug_scan_service.bsh"


// REGMGR_STATE

// A type to indicating what we're doing on a high level.

typedef enum
{
    RSM_Running,
    RSM_Initializing
}
REGMGR_STATE_ENUM
    deriving (Eq, Bits);


interface REGMGR_STATE;

    method REGMGR_STATE_ENUM getState();
    method Action setState(REGMGR_STATE_ENUM newState);

    method Action setEmulate(CONTEXT_ID ctxId);
    method Action clearEmulate();

    method Action setRewind(CONTEXT_ID ctxId);
    method Action clearRewind();

    // Ready to start a new operation?
    method Bool readyToBegin(CONTEXT_ID ctxId);
    
    // Ok to continue an operation already in progress?
    method Bool readyToContinue();

endinterface: REGMGR_STATE


module [HASIM_MODULE] mkRegmanagerState#(REGMGR_STATE_ENUM init)
    // interface:
        (REGMGR_STATE);

    Reg#(REGMGR_STATE_ENUM) state <- mkReg(init);
    Reg#(Bit#(1)) bscSchedHint <- mkReg(0);

    Reg#(Maybe#(CONTEXT_ID)) emulateCtxId <- mkReg(tagged Invalid);
    Reg#(Maybe#(CONTEXT_ID)) rewindCtxId <- mkReg(tagged Invalid);


    DEBUG_SCAN_FIELD_LIST dbg_list = List::nil;
    dbg_list <- addDebugScanField(dbg_list, "State", state);
    dbg_list <- addDebugScanMaybeField(dbg_list, "Rewind context ID", rewindCtxId);
    dbg_list <- addDebugScanMaybeField(dbg_list, "Emulate context ID", emulateCtxId);

    let dbgNode <- mkDebugScanNode("FUNCP REGMGR State", dbg_list);


    method REGMGR_STATE_ENUM getState();
        return state;
    endmethod

    method Action setState(REGMGR_STATE_ENUM newState);
        state      <= newState;
    
        // Hint to the Bluespec scheduler that exactly one rule with setState
        // may fire in a single cycle.  Operators above just write so they
        // don't force mutual exclusion.
        bscSchedHint <= ~bscSchedHint;
    endmethod


    method Action setEmulate(CONTEXT_ID ctxId);
        emulateCtxId <= tagged Valid ctxId;
    endmethod

    method Action clearEmulate();
        emulateCtxId <= tagged Invalid;
    endmethod


    method Action setRewind(CONTEXT_ID ctxId);
        rewindCtxId <= tagged Valid ctxId;
    endmethod

    method Action clearRewind();
        rewindCtxId <= tagged Invalid;
    endmethod


    method Bool readyToBegin(CONTEXT_ID ctxId);
        //
        // Ready to begin if state is RSM_Running and the context is not either
        // blocked for emulation or a rewind.
        //
        return (state == RSM_Running) &&
               (! isValid(emulateCtxId) || (validValue(emulateCtxId) != ctxId)) &&
               (! isValid(rewindCtxId) || (validValue(rewindCtxId) != ctxId));
    endmethod

    method Bool readyToContinue();
        return (state == RSM_Running);
    endmethod

endmodule
