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
 
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"


// ========================================================================
//
//   Internal data structures
//
// ========================================================================


// Which path are we sending the next response from?

typedef union tagged
{
    Tuple2#(TOKEN, Maybe#(STORE_TOKEN)) COMMIT2_commitRsp;
    Bool COMMIT2_faultRsp;
}
COMMIT2_PATH deriving (Eq, Bits);



module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_CommitResults#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_REG_MAPPING_COMMITRESULTS regMapping,
    FUNCP_FREELIST freelist,
    REGSTATE_MEMORY_QUEUE linkToMem,
    BROM#(TOKEN_INDEX, REGMGR_DST_REGS) tokDsts)
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

    // Committing a faulting token can implicitly trigger a fault.
    Connection_Client#(FUNCP_REQ_HANDLE_FAULT,
                       FUNCP_RSP_HANDLE_FAULT) linkHandleFault <- mkConnection_Client("funcp_handleFault");

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

    FIFO#(COMMIT2_PATH) commQ   <- mkFIFO();

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

    rule commitResults1 (linkCommitResults.getReq().token matches .tok &&&
                         state.readyToBegin(tokContextId(tok)) &&&
                         tokScoreboard.canStartCommit(tok.index));

        // Get the input from the timing model. Begin macro-operation.
        let req = linkCommitResults.getReq();
        linkCommitResults.deq();

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": CommitResults: Begin.")); 

        // Confirm timing model propagated poison bit correctly
        assertion.poisonBit(tokIsPoisoned(tok) == isValid(tokScoreboard.getFault(tok.index)));

        if (tokScoreboard.getFault(tok.index) matches tagged Valid .fault)
        begin

            // Timing model tried to commit an instruction with an exception.
            // Instead we'll handle the fault and pass back the new PC.
            // Instead of committing the token we'll undo its effects.
            // The timing model may also do a rewindToToken to undo the effect of following instructions.
            debugLog.record(fshow(tok.index) + $format(": CommitResults: Faulting instruction, redirecting to handleFault."));
            linkHandleFault.makeReq(initFuncpReqHandleFault(tok));

            // Request the registers to be freed.
            regMapping.readRewindReq(tok);
            tokDsts.readReq(tok.index);

            // Drop any stores associated with this token.
            if (tokScoreboard.isStore(tok.index))
            begin
            
                // Tell the store buffer to drop any stores associated with this token.
                let m_req = MEMSTATE_REQ_REWIND {rewind_to: tok.index - 1, rewind_from: tok.index};
                linkToMem.makeReq(tagged REQ_REWIND m_req);

                commQ.enq(tagged COMMIT2_faultRsp True);
        
            end
            else
            begin

                // Make sure no responses pass this response.
                commQ.enq(tagged COMMIT2_faultRsp False);

            end

        end
        else
        begin

            // Update the scoreboard.
            tokScoreboard.commitStart(tok.index);

            // Convert the token to a store token
            Maybe#(STORE_TOKEN) store_token = tagged Invalid;
            if (tokScoreboard.isStore(tok.index))
            begin
                let store_tok_idx <- tokScoreboard.allocateStore(tok.index);
                let store_tok = STORE_TOKEN { index: store_tok_idx };

                debugLog.record(fshow(tok.index) + $format(": commitResults1:  Allocate ") + fshow(store_tok));

                linkToMem.makeReq(tagged REQ_COMMIT MEMSTATE_REQ_COMMIT {tok: tok, storeTok: store_tok});
                store_token = tagged Valid store_tok;
            end

            // Request the registers to be freed.
            regMapping.readRewindReq(tok);

            // Pass to the next stage.
            commQ.enq(tagged COMMIT2_commitRsp tuple2(tok, store_token));
        
        end

    endrule

    // commitResults2
    
    // When:   After a commitResults1 AND commitResultsAdditional is not occuring.
    // Effect: Free the appropriate physical register and respond to the timing model.
    //         If there is more work to do, the next rule will handle it.
    //         Note that it is safe to "short path" the response because the committing of more
    //         results has higher priority than starting the commit of a new token.

    rule commitResults2 (commQ.first() matches tagged COMMIT2_commitRsp {.tok, .store_token} &&& state.readyToContinue());

        // Get the input from the previous stage.
        commQ.deq();

        let ctx_id = tokContextId(tok);
        assertion.expectedOldestTok(tok.index == tokScoreboard.oldest(ctx_id));
        if (tok.index != tokScoreboard.oldest(ctx_id))
            debugLog.record(fshow(tok.index) + $format(": commitResults1:  Token is not oldest!  Oldest: ") + fshow(tokScoreboard.oldest(ctx_id)));

        if (store_token matches tagged Valid .s_tok)
        begin
            linkToMem.deq();
        end

        // Retrieve the registers to be freed.
        let rewind_info <- regMapping.readRewindRsp();
        let regs_to_free = validValue(rewind_info).regsToFree;
        
        // Free the registers which used to be mapped to the destination registers.
        freelist.freeRegs(regs_to_free);

        // Update the scoreboard so the token can be reused.
        tokScoreboard.deallocate(tok.index);
        tokScoreboard.commitFinish(tok.index);

        // Respond to the timing model. End of macro-operation.
        linkCommitResults.makeResp(initFuncpRspCommitResults(tok, store_token, tagged Invalid));
        debugLog.record(fshow(tok.index) + $format(": CommitResults: End.")); 

    endrule

    rule commitFaultRsp (commQ.first() matches tagged COMMIT2_faultRsp .is_store);
        commQ.deq();
    
        // Get the response from the fault handler to pass it on to the timing model.
        let rsp = linkHandleFault.getResp();
        linkHandleFault.deq();

        if (is_store)
        begin
            linkToMem.deq();
        end

        // Retrieve the registers to be freed.
        let rewind_info <- regMapping.readRewindRsp();
        
        // Get the current mapping.
        let dsts <- tokDsts.readRsp();

        let ctx_id = tokContextId(rsp.token);

        // Undo this token's mappings, similar to a one-time rewindToToken.

        if (rewind_info matches tagged Valid .rw)
        begin

            REGSTATE_NEW_MAPPINGS new_map = ?;
            new_map.context_id = ctx_id;

            ISA_INST_DSTS dead_pregs = newVector();

            for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
            begin
                if (dsts.ar[x] matches tagged Valid .ar &&&
                    rw.regsToFree[x] matches tagged Valid .pr_free)
                begin
                    // Set the mapping back
                    new_map.mappings[x] = tagged Valid tuple2(ar, pr_free);
                    debugLog.record($format("commitResults: ") + fshow(rsp.token) + $format(": Remapping (%0d/%0d)", ar, pr_free));
                end
                else
                begin
                    new_map.mappings[x] = tagged Invalid;
                end

                if (dsts.pr[x] matches tagged Valid .pr)
                begin
                    // The current destination must be freed.
                    dead_pregs[x] = tagged Valid pr;
                    debugLog.record($format("commitResults: ") + fshow(rsp.token) + $format(": Telling Free list to free PR ", pr));
                end
                else
                begin
                    dead_pregs[x] = tagged Invalid;
                end
            end

            // Re-map the registers to the registers which used to be mapped to those registers.
            regMapping.updateMap(new_map);

            // Free the current destinations for re-use.
            freelist.freeRegs(dead_pregs);

        end
        
        // Update the scoreboard so the token can be reused.
        tokScoreboard.deallocate(rsp.token.index);

        // Respond to the timing model.
        linkCommitResults.makeResp(initFuncpRspCommitResults(rsp.token, tagged Invalid, tagged Valid rsp.nextInstructionAddress));
    
    endrule

endmodule
