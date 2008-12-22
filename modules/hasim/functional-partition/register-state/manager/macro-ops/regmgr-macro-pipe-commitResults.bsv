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

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_modellib.bsh"
 
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"


// ========================================================================
//
//   Internal data structures
//
// ========================================================================


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_CommitResults#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_REG_MAPPING_COMMITRESULTS regMapping,
    FUNCP_FREELIST freelist)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_commitResults.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_COMMIT_RESULTS,
                       FUNCP_RSP_COMMIT_RESULTS) linkCommitResults <- mkConnection_Server("funcp_commitResults");


    // ====================================================================
    //
    //   Local names for global data 
    //
    // ====================================================================

    let state = glob.state;
    let assertion = glob.assertion;
    let tokScoreboard = glob.tokScoreboard;


    // ====================================================================
    //
    //   Local state
    //
    // ====================================================================

    FIFO#(TOKEN) commQ   <- mkFIFO();

    // Does the commit stage have to free more registers?
    Reg#(Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) additionalRegsToFree <- mkReg(Vector::replicate(tagged Invalid));


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* commitResults ******* //

    // 2 stage macro operation which commits all local results.
    // If a token has more than 1 destination, an additional stage commits them.

    // When:   When the timing model requests it.
    // Effect: For each allocated physical register destination, we free the "old writer"
    //         of that destination. Or, if there was no destination, free the "dummy" register.
    // Soft Inputs:  Token
    // Soft Returns: Token
    
    // commitResults1

    // When:   When the timing model starts a commitResults().
    // Effect: Lookup the destinations of this token, and the registers to free.

    rule commitResults1 (state.readyToBegin());

        // Get the input from the timing model. Begin macro-operation.
        let req = linkCommitResults.getReq();
        linkCommitResults.deq();
        let tok = req.token;

        // Log it.
        debugLog.record($format("TOKEN %0d: CommitResults: Begin.", tok.index)); 

        // Confirm timing model propagated poison bit correctly
        assertion.poisonBit(tokIsPoisoned(tok) == isValid(tokScoreboard.getFault(tok.index)));

        if (tokScoreboard.getFault(tok.index) matches tagged Valid .fault)
        begin
            // Timing model tried to commit an instruction with an exception.
            assertion.commitFaultingInstr(False);
            debugLog.record($format("TOKEN %0d: CommitResults: FAULTING instruction!  Aborting.", tok.index)); 
        end

        // Update the scoreboard.
        tokScoreboard.commitStart(tok.index);

        // Request the registers to be freed.
        regMapping.readRewindReq(tok);

        // Pass to the next stage.
        commQ.enq(tok);

    endrule

    // commitResults2
    
    // When:   After a commitResults1 AND commitResultsAdditional is not occuring.
    // Effect: Free the appropriate physical register and respond to the timing model.
    //         If there is more work to do, the next rule will handle it.
    //         Note that it is safe to "short path" the response because the committing of more
    //         results has higher priority than starting the commit of a new token.

    // There are more registers to free if any member of the the vector is valid.

    Bool moreRegsToFree = Vector::any(isValid, additionalRegsToFree);

    rule commitResults2 (state.readyToContinue() && !moreRegsToFree);

        // Get the input from the previous stage.
        let tok = commQ.first();
        commQ.deq();

        assertion.expectedOldestTok(tok.index == tokScoreboard.oldest());
        if (tok.index != tokScoreboard.oldest())
            debugLog.record($format("TOKEN %0d: commitResults1:  Token is not oldest! (Oldest: %0d)", tok.index, tokScoreboard.oldest()));

        // Retrieve the registers to be freed.
        let rewind_info <- regMapping.readRewindRsp();
        let regs_to_free = validValue(rewind_info).regsToFree;

        // Go ahead and free the first register, if present.
        case (regs_to_free[0]) matches
            tagged Invalid:  noAction;
            tagged Valid .r: freelist.free(r);
        endcase

        // Store all the remaining register names for a later stage to handle.
        regs_to_free[0] = tagged Invalid;
        additionalRegsToFree <= regs_to_free;

        // Update the scoreboard so the token can be reused.
        tokScoreboard.deallocate(tok.index);

        // Respond to the timing model. End of macro-operation (except any more registers below).
        linkCommitResults.makeResp(initFuncpRspCommitResults(tok));
        debugLog.record($format("TOKEN %0d: CommitResults: End.", tok.index)); 

    endrule

    // commitResultsAdditional
    
    // When:   After a commitResults2 AND there are more physical registers to free.
    // Effect: Free the appropriate physical register.
 
    rule commitResultsAdditional (findIndex(isValid, additionalRegsToFree) matches tagged Valid .idx);

        let r = validValue(additionalRegsToFree[idx]);
        debugLog.record($format("Committing Additional PR: %0d", r)); 
        freelist.free(r);
        additionalRegsToFree[idx] <= tagged Invalid;

    endrule
    
endmodule
