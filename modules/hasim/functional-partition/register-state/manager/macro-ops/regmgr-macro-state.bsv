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
// Maintain "state" in regstate manager, governing locks between rules.
// At its core the state is a single variable of type REGMGR_STATE_ENUM.
// However, some rules are allowed to fire under multiple states.  The
// result is the predicate for rules may be relatively long latency
// combinational tests.  The regstate manager state interface here sets
// some boolean predicates that are functions of the state each time
// it is updated.  This puts the work of generating a complicated predicate
// in the parallel part of a rule body, instead of the predicate.
//

// REGMGR_STATE

// A type to indicating what we're doing on a high level.

typedef enum
{
    // RSM_Running and Draining entries are put first to make the readyToContinue
    // test more efficient.
    RSM_Running,
    RSM_DrainingForFault,
    RSM_DrainingForRewind,
    RSM_DrainingForEmulate,

    RSM_Initializing,
    RSM_HandleFault,
    RSM_HandleFaultRewindDone,
    RSM_ReadyToRewind,
    RSM_Rewinding,
    RSM_RewindingWaitForSlowRemap,
    RSM_EmulateGenRegMap,
    RSM_SyncingRegisters,
    RSM_RequestingEmulation,
    RSM_UpdatingRegisters
}
REGMGR_STATE_ENUM
    deriving (Eq, Bits);


interface REGMGR_STATE;

    method REGMGR_STATE_ENUM getState();
    method Action setState(REGMGR_STATE_ENUM newState);

    // Ready to start a new operation?
    method Bool readyToBegin();
    
    // Ok to continue an operation already in progress?
    method Bool readyToContinue();

endinterface: REGMGR_STATE


module mkRegmanagerState#(REGMGR_STATE_ENUM init)
    // interface:
        (REGMGR_STATE);

    function Bool readyToBeginFromState(REGMGR_STATE_ENUM s);
        return s == RSM_Running;
    endfunction

    function Bool readyToContinueFromState(REGMGR_STATE_ENUM s);
        return s == RSM_Running ||
               s == RSM_DrainingForRewind ||
               s == RSM_DrainingForFault ||
               s == RSM_DrainingForEmulate;
    endfunction

    Reg#(REGMGR_STATE_ENUM) state <- mkReg(init);
    Reg#(Bool) okBegin    <- mkReg(readyToBeginFromState(init));
    Reg#(Bool) okContinue <- mkReg(readyToContinueFromState(init));
    Reg#(Bit#(1)) bscSchedHint <- mkReg(0);

    method REGMGR_STATE_ENUM getState();
        return state;
    endmethod

    method Action setState(REGMGR_STATE_ENUM newState);
        state      <= newState;
        okBegin    <= readyToBeginFromState(newState);
        okContinue <= readyToContinueFromState(newState);
    
        // Hint to the Bluespec scheduler that exactly one rule with setState
        // may fire in a single cycle.  Operators above just write so they
        // don't force mutual exclusion.
        bscSchedHint <= ~bscSchedHint;
    endmethod

    method Bool readyToBegin();
        return okBegin;
    endmethod

    method Bool readyToContinue();
        return okContinue;
    endmethod

endmodule
