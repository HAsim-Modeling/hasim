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

//
// This code used to be much more complicated than it is now because there
// was a single finite state machine for the entire register state pipeline.
// Now that there are separate state machines for each pipeline the code
// here is responsible for global control:  blocking individual contexts
// from entering the functional pipelines during instruction emulation and
// exception handling.
//

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

    method Action setException(CONTEXT_ID ctxId);
    method Action clearException();

    // Ready to start a new operation?
    method Bool readyToBegin(CONTEXT_ID ctxId);
    
    // Ok to continue an operation already in progress?
    method Bool readyToContinue();

endinterface: REGMGR_STATE


module mkRegmanagerState#(REGMGR_STATE_ENUM init)
    // interface:
        (REGMGR_STATE);

    Reg#(REGMGR_STATE_ENUM) state <- mkReg(init);
    Reg#(Bit#(1)) bscSchedHint <- mkReg(0);

    Reg#(Maybe#(CONTEXT_ID)) emulateCtxId <- mkReg(tagged Invalid);
    Reg#(Maybe#(CONTEXT_ID)) exceptionCtxId <- mkReg(tagged Invalid);


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


    method Action setException(CONTEXT_ID ctxId);
        exceptionCtxId <= tagged Valid ctxId;
    endmethod

    method Action clearException();
        exceptionCtxId <= tagged Invalid;
    endmethod


    method Bool readyToBegin(CONTEXT_ID ctxId);
        //
        // Ready to begin if state is RSM_Running and the context is not either
        // blocked for emulation or an exception.
        //
        return (state == RSM_Running) &&
               (! isValid(emulateCtxId) || (validValue(emulateCtxId) != ctxId)) &&
               (! isValid(exceptionCtxId) || (validValue(exceptionCtxId) != ctxId));
    endmethod

    method Bool readyToContinue();
        return (state == RSM_Running);
    endmethod

endmodule
