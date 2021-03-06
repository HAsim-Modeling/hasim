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
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_services_lib.bsh"
`include "asim/provides/soft_services_deps.bsh"
`include "asim/provides/fpga_components.bsh"
`include "awb/provides/debug_scan_service.bsh"

// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"


// ========================================================================
//
//   Internal data structures
//
// ========================================================================


//
// Rewind pipeline states.
//
typedef enum
{
    RSM_REW_Running,
    RSM_REW_DrainingForRewind,
    RSM_REW_ReadyToRewind,
    RSM_REW_Rewinding,
    RSM_REW_RewindingWaitForSlowRemap
}
REGMGR_EXC_STATE_ENUM
    deriving (Eq, Bits);


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_Rewind#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_MEMORY_QUEUE linkToMem,
    REGSTATE_REG_MAPPING_REWIND regMapping,
    BROM#(TOKEN_INDEX, REGMGR_DST_REGS) tokDsts,
    FUNCP_FREELIST freelist,
    STDIO#(Bit#(32)) stdio)
    //interface:
    ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_rewind.out");

    let msgRecvReq <- getGlobalStringUID("FUNCP REWIND: start TOKEN (%d, %d)\n");
    let msgSendRsp <- getGlobalStringUID("FUNCP REWIND: done TOKEN %d\n");

    let stdioRW <- mkStdIO_CondPrintf(ioMask_FUNCP_REGMGR, stdio);

    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_REWIND_TO_TOKEN,
                       FUNCP_RSP_REWIND_TO_TOKEN) linkRewindToToken <-
        mkFUNCPInterfaceServer("funcp_rewindToToken");

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

    FIFOF#(Tuple3#(TOKEN_INDEX, Bool, Bool)) rewindQ <- mkFIFOF();

    // Rewind state
    Reg#(FUNCP_REQ_REWIND_TO_TOKEN) rewindReq <- mkRegU();

    Reg#(TOKEN) rewindTok <- mkRegU();
    Reg#(TOKEN_INDEX) rewindCur <- mkRegU();

    Reg#(REGMGR_EXC_STATE_ENUM) state_rew <- mkReg(RSM_REW_Running);

    DEBUG_SCAN_FIELD_LIST dbg_list = List::nil;
    dbg_list <- addDebugScanField(dbg_list, "State", state_rew);
    dbg_list <- addDebugScanField(dbg_list, "rewindQ notFull", rewindQ.notFull);
    dbg_list <- addDebugScanField(dbg_list, "rewindQ notEmpty", rewindQ.notEmpty);

    RWire#(Bool) dbgRewindQIsDone <- mkRWire();
    rule checkRewindQ (True);
        dbgRewindQIsDone.wset(tpl_3(rewindQ.first()));
    endrule

    dbg_list <- addDebugScanMaybeField(dbg_list, "rewindQ tagged done", dbgRewindQIsDone.wget);
    dbg_list <- addDebugScanField(dbg_list, "rewindCur", rewindCur);
    dbg_list <- addDebugScanField(dbg_list, "rewindTok", rewindTok.index);
    // These aren't left on all the time since they need extra read ports
    //dbg_list <- addDebugScanField(dbg_list, "destWritesInFlight", tokScoreboard.destWritesInFlight(rewindCur));
    //dbg_list <- addDebugScanField(dbg_list, "Current Token is LOAD", tokScoreboard.isLoad(rewindCur));
    //dbg_list <- addDebugScanField(dbg_list, "Current Token is POISONED", isValid(tokScoreboard.getFault(rewindCur)));

    let dbgNode <- mkDebugScanNode("FUNCP REGMGR rewind", dbg_list);


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================

    // ******* rewindToToken ******* //

    // 2-stage macro operation which undoes the effects of tokens by backing up the maptable.

    // When:   When the timing model requests it.
    // Effect: Walk back through token history and undo register mappings
    // Soft Inputs:  Token
    // Soft Returns: None

    // rewindToTokenS

    // When:   When the timing model starts a rewindToToken()
    // Effect: Prepare to rewind, making it impossible for other functional
    //         rules to start processing new requests.
    //
    rule rewindToTokenS (state.readyToBegin(tokContextId(linkRewindToToken.getReq().token)) &&
                         (state_rew == RSM_REW_Running));

        // Message arriving from timing model?
        let req = linkRewindToToken.getReq();
        linkRewindToToken.deq();
        rewindReq <= req;

        // Log it.
        debugLog.record($format("Rewind: Preparing rewind to ") + fshow(req.token.index));
        stdioRW.printf(msgRecvReq, list2(zeroExtend(tokContextId(req.token)),
                                         zeroExtend(tokTokenId(req.token))));

        state.setRewind(tokContextId(req.token));
        state_rew <= RSM_REW_DrainingForRewind;

    endrule

    // rewindToToken1

    // When:   Follows rewindToTokenS
    // Effect: Wait for other functional activity to stop.  The state change
    //         to rewinding is on a timing critical path, so the rule doesn't
    //         do anything else.

    rule rewindToToken1 ((state_rew == RSM_REW_DrainingForRewind) && tokScoreboard.canRewind(rewindReq.token.index));

        let req = rewindReq;

        let tok = req.token;
        let ctx_id = tokContextId(tok);

        debugLog.record($format("Rewind: Ready to rewind to ") + fshow(tok.index) + $format(" youngest: ") + fshow(tokScoreboard.youngest(ctx_id)));
        state_rew <= RSM_REW_ReadyToRewind;

    endrule

    // rewindToToken2

    // When:   Follows rewindToToken1
    // Effect: Lookup the destinations of this token, and the registers to free.

    rule rewindToToken2 (state_rew == RSM_REW_ReadyToRewind);
      
        let req = rewindReq;
        let tok = req.token;
        let ctx_id = tokContextId(tok);

        // Tell the memory to drop non-committed stores.
        let m_req = MEMSTATE_REQ_REWIND {rewind_to: tok.index, rewind_from: tokScoreboard.youngest(ctx_id)};
        linkToMem.makeReq(tagged REQ_REWIND m_req);

        // Log our failure.
        debugLog.record($format("Rewind: Initiating rewind (Oldest: ") + fshow(tokScoreboard.oldest(tokContextId(tok))));
        
        // Stop when we get to the token.
        rewindTok <= tok;

        // Start at the youngest and go backward.
        rewindCur <= tokScoreboard.youngest(ctx_id);

        // Proceed with rewind.
        state_rew <= RSM_REW_Rewinding;
    
    endrule


    //
    // rewindToToken3 --
    //   Walk the tokens in age order and reconstruct the maptable.  Wait if
    //   the token has begun execution and the destination registers are not
    //   yet written.  The writes could be in flight even though the execute
    //   stage is no longer busy (e.g. Alpha emulated FP instructions).
    //   The only exception for waiting is loads that have completed execute
    //   but may not have updated their target registers.  In that case, the
    //   future register write is supressed because the token will be dead.
    //
    (* conservative_implicit_conditions *)
    rule rewindToToken3 ((state_rew == RSM_REW_Rewinding) &&
                         (! tokScoreboard.destWritesInFlight(rewindCur) ||
                          tokScoreboard.isLoad(rewindCur)));
    
        // Look up the token properties
        regMapping.readRewindReq(rewindCur);
        tokDsts.readReq(rewindCur);

        // Pass it to the next stage who will free it.
        let done = (rewindCur == rewindTok.index);
        let tok_active = tokScoreboard.isAllocated(rewindCur, ALLOC_RP_REWIND);
        rewindQ.enq(tuple3(rewindCur, tok_active, done));

        rewindCur <= rewindCur - 1;

        if (done)
        begin
            // Confirm memory rewind reached memory subsystem
            linkToMem.deq();

            // No more tokens.  Wait for remapping to finish.
            state_rew <= RSM_REW_RewindingWaitForSlowRemap;
        end
    endrule


    //
    // Free registers for tokens coming from rewindToToken3.
    //
    // The predicate on the rule could be simply "True" but the more complicated
    // predicate makes it clearer to the Bluespec scheduler when the rule fires
    // and whether it may conflict with the standard pipeline.
    //
    // The rule blocks if the token has been through getResults and the
    // register updates are still in progress.
    //
    (* conservative_implicit_conditions *)
    rule rewindToToken4 (! tpl_3(rewindQ.first()));

        match { .tok_idx, .tok_active, .done } = rewindQ.first();
        rewindQ.deq();

        let rewind_info <- regMapping.readRewindRsp();
        let dsts <- tokDsts.readRsp();

        let ctx_id = tok_idx.context_id;

        //
        // Unwind register mappings if token has been through getDeps and
        // thus has physical registers allocated.
        //
        if (rewind_info matches tagged Valid .rw)
        begin
            //
            // Rewind register mappings if not at the target state
            //
            if (tok_active)
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
                        debugLog.record($format("Rewind: ") + fshow(tok_idx) + $format(": Remapping (%0d/%0d)", ar, pr_free));
                    end
                    else
                    begin
                        new_map.mappings[x] = tagged Invalid;
                    end

                    if (dsts.pr[x] matches tagged Valid .pr)
                    begin
                        // The current destination must be freed.
                        dead_pregs[x] = tagged Valid pr;
                        debugLog.record($format("Rewind: ") + fshow(tok_idx) + $format(": Telling Free list to free PR ", pr));
                    end
                    else
                    begin
                        dead_pregs[x] = tagged Invalid;
                    end
                end

                regMapping.updateMap(new_map);
                freelist.freeRegs(dead_pregs);
            end
        end

        tokScoreboard.deallocateForRewind(tok_idx);

    endrule


    (* conservative_implicit_conditions *)
    rule rewindToToken5 ((state_rew == RSM_REW_RewindingWaitForSlowRemap) &&
                         tpl_3(rewindQ.first()));

        match { .tok_idx, .tok_active, .done } = rewindQ.first();
        rewindQ.deq();

        let rewind_info <- regMapping.readRewindRsp();
        let dsts <- tokDsts.readRsp();

        if (isValid(rewind_info))
        begin
            debugLog.record($format("Rewind: Lookup last ") + fshow(tok_idx));
        end

        debugLog.record($format("Rewind: Done."));  
        stdioRW.printf(msgSendRsp, list2(zeroExtend(tokContextId(rewindTok)),
                                         zeroExtend(tokTokenId(rewindTok))));

        tokScoreboard.rewindTo(rewindTok.index);
        // Return response
        linkRewindToToken.makeResp(initFuncpRspRewindToToken(rewindTok));
        state.clearRewind();
        state_rew <= RSM_REW_Running;

    endrule

endmodule
