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


//
// Read state controls access to register file BRAM
//
typedef enum
{
    COMMIT_STATE_READY,
    COMMIT_STATE_FAULT_START,
    COMMIT_STATE_FAULT_FIX_REGS,
    COMMIT_STATE_FAULT_END
}
COMMIT_STATE
    deriving (Eq, Bits);


// Which path are we sending the next response from?

typedef union tagged
{
    Tuple2#(TOKEN, Maybe#(STORE_TOKEN)) COMMIT_COMMIT_RSP;
    Tuple2#(TOKEN, Bool) COMMIT_FAULT_RSP;
}
COMMIT_PATH deriving (Eq, Bits);



module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_CommitResults#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_REG_MAPPING_COMMITRESULTS regMapping,
    REGSTATE_PHYSICAL_REGS_RW_REG prf,
    FUNCP_FREELIST freelist,
    REGSTATE_MEMORY_QUEUE linkToMem,
    BROM#(TOKEN_INDEX, REGMGR_DST_REGS) tokDsts)
    //interface:
    ()
    provisos (Alias#(UInt#(TLog#(TAdd#(1, ISA_MAX_DSTS))), t_REG_IDX));

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
                       FUNCP_RSP_COMMIT_RESULTS) linkCommitResults <-
        mkFUNCPInterfaceServer("funcp_commitResults");

    // Committing a faulting token can implicitly trigger a fault.
    Connection_Client#(FUNCP_REQ_HANDLE_FAULT,
                       FUNCP_RSP_HANDLE_FAULT) linkHandleFault <-
        mkConnection_Client("funcp_handleFault");

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

    Reg#(COMMIT_STATE) commitState <- mkReg(COMMIT_STATE_READY);
    FIFO#(Tuple4#(TOKEN, Bool, Bool, STORE_TOKEN)) comm2Q <- mkSizedFIFO(8);
    FIFO#(COMMIT_PATH) comm3Q <- mkFIFO();
    FIFO#(Tuple2#(t_REG_IDX, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) fixFaultRegsQ <- mkFIFO();
    Reg#(t_REG_IDX) faultRegIdx <- mkRegU();
    Reg#(REGMGR_DST_REGS) faultDstMap <- mkRegU();
    Reg#(REGSTATE_REWIND_INFO) faultRewindInfo <- mkRegU();
    Reg#(TOKEN) faultRewindTok <- mkRegU();
    

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
    // Effect: Make request to memory (stores only)

    rule commitResults1 (commitState == COMMIT_STATE_READY &&&
                         linkCommitResults.getReq().token matches .tok &&&
                         state.readyToBegin(tokContextId(tok)) &&&
                         tokScoreboard.canStartCommit(tok.index));

        // Get the input from the timing model. Begin macro-operation.
        let req = linkCommitResults.getReq();
        linkCommitResults.deq();

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": CommitResults: Begin.")); 

        // Confirm timing model propagated poison bit correctly
        assertion.poisonBit(tokIsPoisoned(tok) == isValid(tokScoreboard.getFault(tok.index)));

        Bool is_active_store = (tokScoreboard.isStore(tok.index) &&
                                ! tokScoreboard.isStoreSquashed(tok.index));

        if (tokScoreboard.getFault(tok.index) matches tagged Valid .fault)
        begin
            // Drop any stores associated with this token.
            if (is_active_store)
            begin
                // Tell the store buffer to drop any stores associated with this token.
                let m_req = MEMSTATE_REQ_REWIND { rewind_to: tok.index - 1,
                                                  rewind_from: tok.index};
                linkToMem.makeReq(tagged REQ_REWIND m_req);
            end

            // Pass to the next stage.
            comm2Q.enq(tuple4(tok, True, is_active_store, ?));
        end
        else
        begin
            // Convert the token to a store token
            STORE_TOKEN store_token = ?;

            if (is_active_store)
            begin
                let store_tok_idx <- tokScoreboard.allocateStore(tok.index);
                store_token = STORE_TOKEN { index: store_tok_idx };

                debugLog.record(fshow(tok.index) + $format(": commitResults1:  Allocate ") + fshow(store_token));

                linkToMem.makeReq(tagged REQ_COMMIT memStateReqCommit(tok, store_token));
            end

            // Pass to the next stage.
            comm2Q.enq(tuple4(tok, False, is_active_store, store_token));
        end
    endrule


    // commitResults2

    // When:   When the timing model starts a commitResults().
    // Effect: Lookup the destinations of this token, and the registers to free.

    rule commitResults2 ((commitState == COMMIT_STATE_READY) && state.readyToContinue());
        match {.tok, .is_fault, .is_active_store, .store_token} = comm2Q.first();
        comm2Q.deq();

        if (is_active_store)
        begin
            linkToMem.deq();
        end

        if (is_fault)
        begin
            // Timing model tried to commit an instruction with an exception.
            // Instead we'll handle the fault and pass back the new PC.
            // Instead of committing the token we'll undo its effects.
            // The timing model may also do a rewindToToken to undo the effect of following instructions.
            debugLog.record(fshow(tok.index) + $format(": CommitResults2: Faulting instruction, redirecting to handleFault."));
            linkHandleFault.makeReq(initFuncpReqHandleFault(tok));

            // Request the registers to be freed.
            regMapping.readRewindReq(tok);
            tokDsts.readReq(tok.index);

            commitState <= COMMIT_STATE_FAULT_START;
            comm3Q.enq(tagged COMMIT_FAULT_RSP tuple2(tok, is_active_store));
        end
        else
        begin
            // Update the scoreboard.
            tokScoreboard.commitStart(tok.index);

            // Request the registers to be freed.
            regMapping.readRewindReq(tok);

            // Pass to the next stage.
            Maybe#(STORE_TOKEN) m_store_token = is_active_store ? tagged Valid store_token :
                                                                  tagged Invalid;
            comm3Q.enq(tagged COMMIT_COMMIT_RSP tuple2(tok, m_store_token));
        end
    endrule


    // commitResults3
    
    // When:   After a commitResults1 AND commitResultsAdditional is not occuring.
    // Effect: Free the appropriate physical register and respond to the timing model.
    //         If there is more work to do, the next rule will handle it.
    //         Note that it is safe to "short path" the response because the committing of more
    //         results has higher priority than starting the commit of a new token.

    rule commitResults3 (comm3Q.first() matches tagged COMMIT_COMMIT_RSP {.tok, .store_token} &&& state.readyToContinue());

        // Get the input from the previous stage.
        comm3Q.deq();

        let ctx_id = tokContextId(tok);
        assertion.expectedOldestTok(tok.index == tokScoreboard.oldest(ctx_id));
        if (tok.index != tokScoreboard.oldest(ctx_id))
            debugLog.record(fshow(tok.index) + $format(": commitResults1:  Token is not oldest!  Oldest: ") + fshow(tokScoreboard.oldest(ctx_id)));

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


    //
    // commitFaultStart --
    //     The beginning of the fault handling path for poisoned instructions.
    //     During this path, the instruction will be turned into a NOP by
    //     copying the previous AR value of each output register of the
    //     instruction to the PR now mapped to those registers.  We used to
    //     do a rewind-style remapping of registers, as though the faulting
    //     instruction was killed.  This suffered from a race with younger
    //     instructions that will eventually be killed.  If those younger
    //     instructions modify ARs written by this instruction, the mapping
    //     table would be updated incorrectly.
    //
    rule commitFaultStart (commitState == COMMIT_STATE_FAULT_START &&&
                           comm3Q.first() matches tagged COMMIT_FAULT_RSP {.tok, .is_store});
        comm3Q.deq();
    
        // Retrieve the registers to be freed.
        let rewind_info <- regMapping.readRewindRsp();
        faultRewindInfo <= validValue(rewind_info);
        faultRewindTok <= tok;
        
        // Get the current mapping.
        let dsts <- tokDsts.readRsp();
        faultDstMap <= dsts;

        faultRegIdx <= 0;

        if (isValid(rewind_info))
        begin
            commitState <= COMMIT_STATE_FAULT_FIX_REGS;
            debugLog.record($format("CommitResults: FAULT start reg fixup"));
        end
        else
        begin
            // No register bindings to fix.
            commitState <= COMMIT_STATE_FAULT_END;
            debugLog.record($format("CommitResults: FAULT no dst regs"));
        end
    endrule


    //
    // commitFaultReadOldRegs --
    //     Read the old values of the registers written by the faulting
    //     instruction.
    //
    rule commitFaultReadOldRegs ((commitState == COMMIT_STATE_FAULT_FIX_REGS) &&
                                 (faultRegIdx < fromInteger(valueOf(ISA_MAX_DSTS))));
        //
        // Find the old mapping.
        //
        if (faultDstMap.ar[faultRegIdx] matches tagged Valid .ar &&&
            faultDstMap.pr[faultRegIdx] matches tagged Valid .pr &&&
            faultRewindInfo.regsToFree[faultRegIdx] matches tagged Valid .pr_prev)
        begin
            // Read the old value
            prf.readReq(faultRewindTok, pr_prev);
            debugLog.record($format("CommitResults: FAULT read old PR%0d", pr_prev));

            fixFaultRegsQ.enq(tuple2(faultRegIdx, tagged Valid pr));
        end
        else
        begin
            fixFaultRegsQ.enq(tuple2(faultRegIdx, tagged Invalid));
        end

        faultRegIdx <= faultRegIdx + 1;
    endrule


    //
    // commitFaultWriteNewRegs --
    //     Receive old register values requested by commitFaultReadOldRegs and
    //     copy the values to the corresponding output physical register of
    //     the faulting instruction.  This effectively turns the faulting
    //     instruction into a NOP with no register side effects.
    //
    rule commitFaultWriteNewRegs (commitState == COMMIT_STATE_FAULT_FIX_REGS);
        match {.r_idx, .fix_pr} = fixFaultRegsQ.first();
        fixFaultRegsQ.deq();
        
        if (fix_pr matches tagged Valid .pr)
        begin
            let old_val <- prf.readRsp();
            prf.write(faultRewindTok, pr, old_val);

            debugLog.record($format("CommitResults: FAULT write PR%0d <- 0x%0x", pr, old_val)); 
        end

        if (r_idx == fromInteger(valueOf(ISA_MAX_DSTS) - 1))
        begin
            commitState <= COMMIT_STATE_FAULT_END;
        end
    endrule


    //
    // commitFaultEnd --
    //     Done with fault processing.  Return result to timing model.
    //
    rule commitFaultEnd (commitState == COMMIT_STATE_FAULT_END);
        // Get the response from the fault handler to pass it on to the timing model.
        let rsp = linkHandleFault.getResp();
        linkHandleFault.deq();

        // Free the registers which used to be mapped to the destination registers.
        freelist.freeRegs(faultRewindInfo.regsToFree);

        // Update the scoreboard so the token can be reused.
        tokScoreboard.deallocate(rsp.token.index);

        // Respond to the timing model.
        linkCommitResults.makeResp(initFuncpRspCommitResults(rsp.token, tagged Invalid, tagged Valid rsp.nextInstructionAddress));
    
        commitState <= COMMIT_STATE_READY;

        debugLog.record(fshow(rsp.token.index) + $format(": CommitResults: FAULT End")); 
    endrule

endmodule
