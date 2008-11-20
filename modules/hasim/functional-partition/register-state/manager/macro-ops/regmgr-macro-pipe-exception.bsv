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


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_Exception#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_TLB_FAULT link_itlb_fault,
    REGSTATE_TLB_FAULT link_dtlb_fault,
    REGSTATE_MEMORY_QUEUE linkToMem,
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokAddr,
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_INSTRUCTION) tokInst,
    FUNCP_SNAPSHOT snapshots,
    FUNCP_FREELIST freelist,
    BRAM#(TOKEN_INDEX, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) tokFreeListPos,
    BRAM#(TOKEN_INDEX, ISA_INST_DSTS) tokRegsToFree,
    Reg#(Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) maptable,
    BRAM_MULTI_READ#(3, TOKEN_INDEX, ISA_INST_DSTS) tokDsts,
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr,
    Reg#(TOKEN_BRANCH_EPOCH) branchEpoch,
    Reg#(TOKEN_FAULT_EPOCH) faultEpoch)
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
    FIFO#(Tuple2#(TOKEN_INDEX, Bool)) rewindQ <- mkFIFO();

    // Rewind state
    Reg#(FUNCP_REQ_REWIND_TO_TOKEN) rewindReq <- mkRegU();
    Reg#(Maybe#(FUNCP_SNAPSHOT_INDEX)) rewindSnapIdx <- mkRegU();

    // Is it a fast rewind or a slow one?
    Reg#(Bool) fastRewind <- mkReg(False);

    // These support "slow rewinds"
    Reg#(TOKEN) rewindTok <- mkRegU();
    Reg#(TOKEN_INDEX) rewindCur <- mkRegU();
    Reg#(Bool) rewindForFault <- mkRegU();


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

    rule handleFaultS (state.readyToBegin() && linkHandleFault.reqNotEmpty());

        // Timing model requested fault handling?
        let req = linkHandleFault.getReq();

        // Log it.
        debugLog.record($format("TOKEN %0d: Preparing to handle fault", req.token.index)); 

        state.setState(RSM_DrainingForFault);

    endrule

    // Wait for all activity to stop and start the fault handler

    rule handleFault1 (state.getState() == RSM_DrainingForFault && tokScoreboard.canRewind());

        // Get the input from the timing model.
        let req = linkHandleFault.getReq();
        linkHandleFault.deq();
        let tok = req.token;
        
        debugLog.record($format("TOKEN %0d: Ready to handle fault", req.token.index)); 

        // Read all possibly interesting addresses
        tokAddr.readReq(tok.index);           // PC
        tokMemAddr.readReq(tok.index);
        
        state.setState(RSM_HandleFault);
        faultQ.enq(tok);

    endrule


    rule handleFault2 (state.getState() == RSM_HandleFault);

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
                debugLog.record($format("TOKEN %0d: handleFault2: ITRANS (VA: 0x%h)", tok.index, addr)); 
                link_itlb_fault.pageFault(handleTLBPageFault(tok, addr));
            end

            FAULT_ITRANS2:
            begin
                let addr = iAddr_aligned + mem_ref_bytes;
                debugLog.record($format("TOKEN %0d: handleFault2: ITRANS2 (VA: 0x%h)", tok.index, addr)); 
                link_itlb_fault.pageFault(handleTLBPageFault(tok, addr));
            end

            FAULT_DTRANS:
            begin
                let addr = dAddr_aligned;
                debugLog.record($format("TOKEN %0d: handleFault2: DTRANS (VA: 0x%h)", tok.index, addr)); 
                link_dtlb_fault.pageFault(handleTLBPageFault(tok, addr));
            end

            FAULT_DTRANS2:
            begin
                let addr = dAddr_aligned + mem_ref_bytes;
                debugLog.record($format("TOKEN %0d: handleFault2: DTRANS2 (VA: 0x%h)", tok.index, addr)); 
                link_dtlb_fault.pageFault(handleTLBPageFault(tok, addr));
            end

            default:
            begin
                assertion.illegalInstruction(False);
                debugLog.record($format("TOKEN %0d: handleFault2: No handler for fault %d", tok.index, fault)); 
            end
            endcase
        end
        else
        begin
            assertion.illegalInstruction(False);
            debugLog.record($format("TOKEN %0d: handleFault2: Instruction did not fault", tok.index));
        end

        //
        // Start a slow rewind, killing all younger than the faulting token and
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
        fastRewind <= False;
        rewindForFault <= True;
        rewindCur <= tokScoreboard.youngest();
        
        // Tell the memory to drop non-committed stores.
        let m_req = MEMSTATE_REQ_REWIND {rewind_to: rewind_to.index, rewind_from: tokScoreboard.youngest()};
        linkToMem.makeReq(tagged REQ_REWIND m_req);

        debugLog.record($format("Rewind: Initiating slow rewind to token %0d", rewind_to.index));
                
        // After rewind, fetch should resume at this token's address
        faultResumeQ.enq(tuple2(tok, iAddr));

        // Start at the youngest and go backward.

        // Proceed with rewind.
        state.setState(RSM_Rewinding);

    endrule

    //
    // handleFault3 -- Control returns here following rewind.
    //
    rule handleFault3 (state.getState() == RSM_HandleFaultRewindDone);

        match { .tok, .resumeInstrAddr } = faultResumeQ.first();
        faultResumeQ.deq();

        // Log it.
        debugLog.record($format("TOKEN %0d: handleFault3: Restart at 0x%h", tok.index, resumeInstrAddr)); 

        // Update the fault epoch so we can discard appropriate updates.
        let new_fault_epoch = faultEpoch + 1;
        faultEpoch <= new_fault_epoch;

        // Send response to timing model
        linkHandleFault.makeResp(initFuncpRspHandleFault(tok, resumeInstrAddr, initEpoch(branchEpoch, new_fault_epoch)));

        state.setState(RSM_Running);

    endrule


    // ******* rewindToToken ******* //

    // 2-stage macro operation which undoes the effects of tokens by backing up the maptable.

    // When:   When the timing model requests it.
    // Effect: If we have a snapshot we can quickly back up to that snapshot. Otherwise we XXX
    // Soft Inputs:  Token
    // Soft Returns: None

    // rewindToTokenS

    // When:   When the timing model starts a rewindToToken()
    // Effect: Prepare to rewind, making it impossible for other functional
    //         rules to start processing new requests.

    rule rewindToTokenS (state.readyToBegin() && linkRewindToToken.reqNotEmpty());

        // Message arriving from timing model?
        let req = linkRewindToToken.getReq();
        linkRewindToToken.deq();
        rewindReq <= req;

        // Log it.
        debugLog.record($format("Rewind: Preparing rewind to TOKEN %0d (youngest: %0d)", req.token.index, tokScoreboard.youngest())); 

        state.setState(RSM_DrainingForRewind);

    endrule

    // rewindToToken1

    // When:   Follows rewindToTokenS
    // Effect: Wait for other functional activity to stop.  The state change
    //         to rewinding and the snapshot.hasSnapshot() method are both on
    //         timing critical paths, so the rule doesn't anything else.

    rule rewindToToken1 (state.getState() == RSM_DrainingForRewind && tokScoreboard.canRewind());

        let req = rewindReq;
        debugLog.record($format("Rewind: Ready to rewind to TOKEN %0d (youngest: %0d)", req.token.index, tokScoreboard.youngest())); 
        state.setState(RSM_ReadyToRewind);

        // Check to see if we have a snapshot.
        let snap = snapshots.hasSnapshot(req.token.index);
        rewindSnapIdx <= snap;

    endrule

    // rewindToToken2

    // When:   Follows rewindToToken1
    // Effect: Lookup the destinations of this token, and the registers to free.

    rule rewindToToken2 (state.getState() == RSM_ReadyToRewind);
      
        let req = rewindReq;
        let tok = req.token;

        // Get Maybe#(snapshot idx) from previous stage
        let midx = rewindSnapIdx;

        // Tell the memory to drop non-committed stores.
        let m_req = MEMSTATE_REQ_REWIND {rewind_to: tok.index, rewind_from: tokScoreboard.youngest()};
        linkToMem.makeReq(tagged REQ_REWIND m_req);

        // Update the epoch so we can discard appropriate updates.
        branchEpoch <= branchEpoch + 1;

        // Alright did we find anything?
        case (midx) matches
            tagged Valid .idx:
            begin 

                // Log our success!
                debugLog.record($format("Rewind: Fast Rewind confirmed with Snapshot %0d", idx));

                // Rewind the scoreboard.
                tokScoreboard.rewindTo(tok.index);

                // Retrieve the snapshots.
                snapshots.requestSnapshot(idx);
                tokFreeListPos.readReq(tok.index);
                
                fastRewind <= True;

            end
            tagged Invalid:
            begin

                // Log our failure.
                debugLog.record($format("Rewind: Initiating slow rewind (Oldest: %0d)", tokScoreboard.oldest()));
                
                fastRewind <= False;

            end
        endcase
        
        // Stop when we get to the token.
        rewindTok <= tok;

        // Normal rewind
        rewindForFault <= False;

        // Start at the youngest and go backward.
        rewindCur <= tokScoreboard.youngest();

        // Proceed with rewind.
        state.setState(RSM_Rewinding);
    
    endrule

    // rewindToToken2

    // When:   After rewindToToken1 AND we have a snapshot.
    // Effect: Use the snapshot to overwrite existing values. Reply to the timing partition.

    rule rewindToToken3Fast (state.getState() == RSM_Rewinding && fastRewind);

        // Confirm memory rewind reached memory subsystem
        linkToMem.deq();

        // Get the snapshots.
        let snp_map <- snapshots.returnSnapshot();
        let snp_fl <- tokFreeListPos.readRsp();

        // Update the maptable.
        maptable <= snp_map;
        
        // Update the freelist.  Must be valid since snapshot and freelist position
        // are both set in same phase of getDependencies.
        freelist.backTo(validValue(snp_fl));

        // Log it.
        debugLog.record($format("Fast Rewind finished."));  

        // We're done. End of macro-operation (path 1).
        state.setState(RSM_Running);
        linkRewindToToken.makeResp(initFuncpRspRewindToToken(rewindTok, initEpoch(branchEpoch, faultEpoch )));

    endrule

    //Slow rewind. Walk the tokens in age order
    //and reconstruct the maptable

    rule rewindToToken3Slow (state.getState() == RSM_Rewinding && !fastRewind);
    
        // Look up the token properties
        tokRegsToFree.readReq(rewindCur);
        tokFreeListPos.readReq(rewindCur);
        tokInst.readPorts[1].readReq(rewindCur);
        tokDsts.readPorts[1].readReq(rewindCur);

        // Pass it to the next stage who will free it.
        let done = (rewindCur == rewindTok.index);
        rewindQ.enq(tuple2(rewindCur, done));

        rewindCur <= rewindCur - 1;

        if (done)
        begin
            // Confirm memory rewind reached memory subsystem
            linkToMem.deq();

            // No more tokens.  Wait for remapping to finish.
            state.setState(RSM_RewindingWaitForSlowRemap);
        end
    endrule


    //
    // Free registers for tokens coming from rewindToTokenSlow1
    //
    rule rewindToToken4 (True);

        match { .tok_idx, .done } = rewindQ.first();
        rewindQ.deq();

        let regs_to_remap <- tokRegsToFree.readRsp();
        let freelist_pos <- tokFreeListPos.readRsp();
        let inst <- tokInst.readPorts[1].readRsp();
        let dsts <- tokDsts.readPorts[1].readRsp();

        //
        // Unwind register mappings if token has been through getDeps and
        // thus has physical registers allocated.
        //
        if (freelist_pos matches tagged Valid .fr_pos)
        begin
            //
            // Rewind register mappings if not at the target state
            //
            if (!done && tokScoreboard.isAllocated(tok_idx))
            begin
                debugLog.record($format("Slow Rewind: Lookup TOKEN %0d", tok_idx));  

                Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) new_maptable = maptable;

                for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
                begin
                    if (isaGetDst(inst, x) matches tagged Valid .arc_r &&&
                        regs_to_remap[x] matches tagged Valid .r)
                    begin
                        // Set the mapping back
                        debugLog.record($format("Slow Rewind: TOKEN %0d: Remapping (%0d/%0d)", tok_idx, arc_r, r));
                        new_maptable = update(new_maptable, pack(arc_r), r);
                    end
                end

                maptable <= new_maptable;
            end
            
            if (done)
                debugLog.record($format("Slow Rewind: Lookup last TOKEN %0d", tok_idx));  

            freelist.backTo(fr_pos);
        end

        // Done with slow rewind?
        if (done)
        begin
            debugLog.record($format("Slow Rewind: Done."));  
            tokScoreboard.rewindTo(rewindTok.index);
            if (! rewindForFault)
            begin
                // Normal rewind -- return response
                linkRewindToToken.makeResp(initFuncpRspRewindToToken(rewindTok, initEpoch(branchEpoch, faultEpoch )));
                state.setState(RSM_Running);
            end
            else
            begin
                // Rewind for fault handler -- resume fault handler path
                state.setState(RSM_HandleFaultRewindDone);
            end
        end

    endrule

endmodule
