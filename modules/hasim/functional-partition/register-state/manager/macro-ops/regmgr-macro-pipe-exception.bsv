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


//
// Fault / exception pipeline states.
//
typedef enum
{
    RSM_EXC_Running,
    RSM_EXC_DrainingForFault,
    RSM_EXC_DrainingForRewind,
    RSM_EXC_HandleFault,
    RSM_EXC_HandleFaultRewindDone,
    RSM_EXC_ReadyToRewind,
    RSM_EXC_Rewinding,
    RSM_EXC_RewindingWaitForSlowRemap
}
REGMGR_EXC_STATE_ENUM
    deriving (Eq, Bits);


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_Exception#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_TLB_FAULT link_itlb_fault,
    REGSTATE_TLB_FAULT link_dtlb_fault,
    REGSTATE_MEMORY_QUEUE linkToMem,
    REGSTATE_REG_MAPPING_EXCEPTION regMapping,
    BROM#(TOKEN_INDEX, REGMGR_DST_REGS) tokDsts,
    FUNCP_FREELIST freelist,
    BROM#(TOKEN_INDEX, ISA_ADDRESS) tokAddr,
    BROM#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr,
    LUTRAM#(CONTEXT_ID, TOKEN_BRANCH_EPOCH) branchEpoch,
    LUTRAM#(CONTEXT_ID, TOKEN_FAULT_EPOCH) faultEpoch)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_exception.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_HANDLE_FAULT,
                       FUNCP_RSP_HANDLE_FAULT)    linkHandleFault   <- mkConnection_Server("funcp_handleFault");

    Connection_Server#(FUNCP_REQ_REWIND_TO_TOKEN,
                       FUNCP_RSP_REWIND_TO_TOKEN) linkRewindToToken <- mkConnection_Server("funcp_rewindToToken");

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

    FIFO#(TOKEN) faultQ <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, ISA_ADDRESS)) faultResumeQ <- mkFIFO();
    FIFO#(Tuple3#(TOKEN_INDEX, Bool, Bool)) rewindQ <- mkFIFO();

    // Rewind state
    Reg#(FUNCP_REQ_REWIND_TO_TOKEN) rewindReq <- mkRegU();

    Reg#(TOKEN) rewindTok <- mkRegU();
    Reg#(TOKEN_INDEX) rewindCur <- mkRegU();
    Reg#(Bool) rewindForFault <- mkRegU();

    Reg#(REGMGR_EXC_STATE_ENUM) state_exc <- mkReg(RSM_EXC_Running);


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* handleFault ******* //

    // Handle a fault raised in an earlier stage.  Returns the address from
    // which fetch should resume.

    // When:   When the timing model requests it.
    // Effect: Do fault action & rewind
    // Soft Inputs:  Token
    // Soft Returns: Token & next fetch address

    rule handleFaultS (state.readyToBegin(tokContextId(linkHandleFault.getReq().token)) &&
                       (state_exc == RSM_EXC_Running));

        // Timing model requested fault handling?
        let req = linkHandleFault.getReq();

        // Log it.
        debugLog.record(fshow(req.token.index) + $format(": Preparing to handle fault")); 

        state.setException(tokContextId(req.token));
        state_exc <= RSM_EXC_DrainingForFault;

    endrule

    // Wait for all activity to stop and start the fault handler

    rule handleFault1 ((state_exc == RSM_EXC_DrainingForFault) && tokScoreboard.canRewind(linkHandleFault.getReq().token.index));

        // Get the input from the timing model.
        let req = linkHandleFault.getReq();
        linkHandleFault.deq();
        let tok = req.token;
        
        debugLog.record(fshow(req.token.index) + $format(": Ready to handle fault")); 

        // Read all possibly interesting addresses
        tokAddr.readReq(tok.index);           // PC
        tokMemAddr.readReq(tok.index);
        
        state_exc <= RSM_EXC_HandleFault;
        faultQ.enq(tok);

    endrule


    (* conservative_implicit_conditions *)
    rule handleFault2 (state_exc == RSM_EXC_HandleFault);

        // Instruction & data addresses
        let iAddr <- tokAddr.readRsp();
        let dAddr <- tokMemAddr.readRsp();

        match { .iAddr_aligned, .iAddr_offset } = isaAlignAddress(iAddr);
        match { .dAddr_aligned, .dAddr_offset } = isaAlignAddress(dAddr);

        let tok = faultQ.first();
        faultQ.deq();

        if (tokScoreboard.getFault(tok.index) matches tagged Valid .fault)
        begin
            let mem_ref_bytes = fromInteger(valueOf(SizeOf#(MEM_VALUE)) / 8);

            //
            // handleTLBPageFault mode for TLBs returns no response.  Just triggering
            // the request is enough to allocate the page.
            //

            case (fault)
            FAULT_ITRANS:
            begin
                let addr = iAddr_aligned;
                debugLog.record(fshow(tok.index) + $format(": handleFault2: ITRANS (VA: 0x%h)", addr)); 
                link_itlb_fault.pageFault(handleTLBPageFault(tok, addr));
            end

            FAULT_ITRANS2:
            begin
                let addr = iAddr_aligned + mem_ref_bytes;
                debugLog.record(fshow(tok.index) + $format(": handleFault2: ITRANS2 (VA: 0x%h)", addr)); 
                link_itlb_fault.pageFault(handleTLBPageFault(tok, addr));
            end

            FAULT_DTRANS:
            begin
                let addr = dAddr_aligned;
                debugLog.record(fshow(tok.index) + $format(": handleFault2: DTRANS (VA: 0x%h)", addr)); 
                link_dtlb_fault.pageFault(handleTLBPageFault(tok, addr));
            end

            FAULT_DTRANS2:
            begin
                let addr = dAddr_aligned + mem_ref_bytes;
                debugLog.record(fshow(tok.index) + $format(": handleFault2: DTRANS2 (VA: 0x%h)", addr)); 
                link_dtlb_fault.pageFault(handleTLBPageFault(tok, addr));
            end

            default:
            begin
                assertion.illegalInstruction(False);
                debugLog.record(fshow(tok.index) + $format(": handleFault2: No handler for fault %d", fault)); 
            end
            endcase
        end
        else
        begin
            assertion.illegalInstruction(False);
            debugLog.record(fshow(tok.index) + $format(": handleFault2: Instruction did not fault"));
        end

        //
        // Start a rewind, killing all younger than the faulting token and
        // the faulting token.  Fast rewind won't work because the faulting token
        // is being killed.
        //
        // Make a dummy token since rewind really needs tok.index - 1
        //
        let rewind_to = TOKEN {index: (tok.index - 1),
                               poison: False,
                               epoch: ?,
                               timep_info: ?};

        rewindTok <= rewind_to;
        rewindForFault <= True;
        let rewind_from = tokScoreboard.youngestDecoded_Safe(rewind_to.index);
        rewindCur <= rewind_from;
        
        // Tell the memory to drop non-committed stores.
        let m_req = MEMSTATE_REQ_REWIND {rewind_to: rewind_to.index, rewind_from: rewind_from};
        linkToMem.makeReq(tagged REQ_REWIND m_req);

        debugLog.record($format("Rewind: Initiating rewind to ") + fshow(rewind_to.index));
                
        // After rewind, fetch should resume at this token's address
        faultResumeQ.enq(tuple2(tok, iAddr));

        // Start at the youngest and go backward.

        // Proceed with rewind.
        state_exc <= RSM_EXC_Rewinding;

    endrule

    //
    // handleFault3 -- Control returns here following rewind.
    //
    rule handleFault3 (state_exc == RSM_EXC_HandleFaultRewindDone);

        match { .tok, .resumeInstrAddr } = faultResumeQ.first();
        faultResumeQ.deq();

        let ctx_id = tokContextId(tok);

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": handleFault3: Restart at 0x%h", resumeInstrAddr)); 

        // Update the fault epoch so we can discard appropriate updates.
        let new_fault_epoch = faultEpoch.sub(ctx_id) + 1;
        faultEpoch.upd(ctx_id, new_fault_epoch);

        // Send response to timing model
        linkHandleFault.makeResp(initFuncpRspHandleFault(tok, resumeInstrAddr, initEpoch(branchEpoch.sub(ctx_id), new_fault_epoch)));

        state.clearException();
        state_exc <= RSM_EXC_Running;

    endrule


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
    //         Tests linkHandleFault to be sure only one of handleFaultS and
    //         rewindTotokenS fires.
    rule rewindToTokenS (state.readyToBegin(tokContextId(linkRewindToToken.getReq().token)) &&
                         (state_exc == RSM_EXC_Running) &&
                         ! linkHandleFault.reqNotEmpty());

        // Message arriving from timing model?
        let req = linkRewindToToken.getReq();
        linkRewindToToken.deq();
        rewindReq <= req;

        // Log it.
        debugLog.record($format("Rewind: Preparing rewind to ") + fshow(req.token.index));

        state.setException(tokContextId(req.token));
        state_exc <= RSM_EXC_DrainingForRewind;

    endrule

    // rewindToToken1

    // When:   Follows rewindToTokenS
    // Effect: Wait for other functional activity to stop.  The state change
    //         to rewinding is on a timing critical path, so the rule doesn't
    //         do anything else.

    rule rewindToToken1 ((state_exc == RSM_EXC_DrainingForRewind) && tokScoreboard.canRewind(rewindReq.token.index));

        let req = rewindReq;

        let tok = req.token;
        let ctx_id = tokContextId(tok);

        debugLog.record($format("Rewind: Ready to rewind to ") + fshow(tok.index) + $format(" (youngest: ") + fshow(tokScoreboard.youngest(ctx_id)) + $format(" / youngest decoded: ") + fshow(tokScoreboard.youngestDecoded(ctx_id)));
        state_exc <= RSM_EXC_ReadyToRewind;

    endrule

    // rewindToToken2

    // When:   Follows rewindToToken1
    // Effect: Lookup the destinations of this token, and the registers to free.

    rule rewindToToken2 (state_exc == RSM_EXC_ReadyToRewind);
      
        let req = rewindReq;
        let tok = req.token;
        let ctx_id = tokContextId(tok);

        // Tell the memory to drop non-committed stores.
        let m_req = MEMSTATE_REQ_REWIND {rewind_to: tok.index, rewind_from: tokScoreboard.youngestDecoded_Safe(tok.index)};
        linkToMem.makeReq(tagged REQ_REWIND m_req);

        // Update the epoch so we can discard appropriate updates.
        branchEpoch.upd(ctx_id, branchEpoch.sub(ctx_id) + 1);

        // Log our failure.
        debugLog.record($format("Rewind: Initiating rewind (Oldest: ") + fshow(tokScoreboard.oldest(tokContextId(tok))));
        
        // Stop when we get to the token.
        rewindTok <= tok;

        // Normal rewind
        rewindForFault <= False;

        // Start at the youngest and go backward.
        rewindCur <= tokScoreboard.youngestDecoded_Safe(tok.index);

        // Proceed with rewind.
        state_exc <= RSM_EXC_Rewinding;
    
    endrule


    //
    // rewindToToken3 --
    //   Walk the tokens in age order and reconstruct the maptable
    //
    (* conservative_implicit_conditions *)
    rule rewindToToken3 (state_exc == RSM_EXC_Rewinding);
    
        // Look up the token properties
        regMapping.readRewindReq(rewindCur);
        tokDsts.readReq(rewindCur);

        // Pass it to the next stage who will free it.
        let done = (rewindCur == rewindTok.index);
        let tok_active = tokScoreboard.isAllocated(rewindCur);
        rewindQ.enq(tuple3(rewindCur, tok_active, done));

        rewindCur <= rewindCur - 1;

        if (done)
        begin
            // Confirm memory rewind reached memory subsystem
            linkToMem.deq();

            // No more tokens.  Wait for remapping to finish.
            state_exc <= RSM_EXC_RewindingWaitForSlowRemap;
        end
    endrule


    //
    // Free registers for tokens coming from rewindToToken3.
    //
    // The predicate on the rule could be simply "True" but the more complicated
    // predicate makes it clearer to the Bluespec scheduler when the rule fires
    // and whether it may conflict with the standard pipeline.
    //
    (* conservative_implicit_conditions *)
    rule rewindToToken4 ((state_exc == RSM_EXC_Rewinding) ||
                         (state_exc == RSM_EXC_RewindingWaitForSlowRemap));

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
            if (!done && tok_active)
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

            if (done)
                debugLog.record($format("Rewind: Lookup last ") + fshow(tok_idx));
        end

        // Done with rewind?
        if (done)
        begin
            debugLog.record($format("Rewind: Done."));  
            tokScoreboard.rewindTo(rewindTok.index);
            if (! rewindForFault)
            begin
                // Normal rewind -- return response
                linkRewindToToken.makeResp(initFuncpRspRewindToToken(rewindTok, initEpoch(branchEpoch.sub(ctx_id), faultEpoch.sub(ctx_id) )));
                state.clearException();
                state_exc <= RSM_EXC_Running;
            end
            else
            begin
                // Rewind for fault handler -- resume fault handler path
                state_exc <= RSM_EXC_HandleFaultRewindDone;
            end
        end

    endrule

endmodule
