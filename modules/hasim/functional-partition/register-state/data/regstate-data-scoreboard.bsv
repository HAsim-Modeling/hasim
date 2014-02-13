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

// A scoreboard to track information of the status of in-flight instructions.

// Note: We use a One-Hot encoding to improve throughput by reducing rule conflicts.

// Note: We allocate only half the tokens at once.
//       This allows the user to check the relative age between two arbitrary tokens.

// Library imports

import Vector::*;

// Project imports

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"
`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"
`include "asim/provides/fpga_components.bsh"

// RRR includes
`include "asim/rrr/service_ids.bsh"

// Dictionary includes
`include "asim/dict/ASSERTIONS_REGSTATE_SCOREBOARD.bsh"

//
// FUNCP_FAULTS
//    Trap codes for faults raised during execution.
//
typedef enum
{
    FAULT_ILLEGAL_INSTR,          // Illegal instruction
    FAULT_ITRANS,                 // ITranslate fault
    FAULT_ITRANS2,                // ITranslate fault in 2nd half of unaligned ref.
    FAULT_DTRANS,                 // DTranslate fault
    FAULT_DTRANS2                 // DTranslate fault in 2nd half of unaligned ref.
}
FUNCP_FAULT
    deriving (Eq, Bits);

//
// FUNCP_ALLOC_RP
//    The isAllocated test has so many consumers that read ports must be managed
//    explicitly.
typedef enum
{
    ALLOC_RP_ALLOC0,
    ALLOC_RP_ALLOC1,
    ALLOC_RP_GETRES,
    ALLOC_RP_DOLOADS,
    ALLOC_RP_DOSTORES,
    ALLOC_RP_REWIND,
    // Must be last and may not be used as a read port
    ALLOC_RP_LAST_SLOT
}
FUNCP_ALLOC_RP
    deriving (Eq, Bits);

typedef 6 ALLOC_NUM_READ_PORTS;


// TOKEN_SCOREBOARD

//
// Scoreboard accomplishes two things:
//   1.  It provies two write ports (reset and upd)
//   2.  It takes advantage of the fact that only half the token space
//       is valid at any given time, mapping the TOKEN_INDEX to a smaller
//       LIVE_TOKEN_INDEX.
//
interface TOKEN_SCOREBOARD;
    method Action upd(TOKEN_INDEX addr, Bool d);
    method Action reset(TOKEN_INDEX addr);
    method Bool sub(TOKEN_INDEX addr);
endinterface

module mkLiveTokenScoreboardU
    // Interface:
    (TOKEN_SCOREBOARD);

    LUTRAM_DUAL_WRITE#(LIVE_TOKEN_INDEX, Bool) mem <- mkDualWriteLUTRAM(mkLUTRAMU);

    method Action upd(TOKEN_INDEX addr, Bool d) = mem.upd(liveTokenIdx(addr), d);
    method Action reset(TOKEN_INDEX addr) = mem.updB(liveTokenIdx(addr), False);
    method Bool sub(TOKEN_INDEX addr)= mem.sub(liveTokenIdx(addr));
endmodule


// FUNCP_SCOREBOARD

// The interface to our scoreboard.

interface FUNCP_SCOREBOARD;

  // Allocate the next available token.
  method ActionValue#(TOKEN_INDEX) allocate(CONTEXT_ID ctx_id);
  // Finish a token and free it for reuse.
  method Action deallocate(TOKEN_INDEX t);
  
  // Allocate a store token given a token.  Should be called during normal
  // instruction commit for stores.
  method ActionValue#(STORE_TOKEN_INDEX) allocateStore(TOKEN_INDEX t);

  // These methods track the internal status of which macro-operation a token is in.
  method Action decFinish(TOKEN_INDEX t);
  method Action exeStart(TOKEN_INDEX t);
  method Action exeFinish(TOKEN_INDEX t);
  method Action dTransStart(TOKEN_INDEX t);
  method Action dTransFinish(TOKEN_INDEX t);
  method Action loadStart(TOKEN_INDEX t);
  method Action loadFinish(TOKEN_INDEX t);
  method Action storeStart(TOKEN_INDEX t);
  method Action storeFinish(TOKEN_INDEX t);
  method Action commitStart(TOKEN_INDEX t);
  method Action commitFinish(TOKEN_INDEX t);
  
  // Set the offsets after we align the address.
  method Action setMemOpOffset(TOKEN_INDEX t, MEM_OFFSET o);
  
  // Set the memory type that we use for accessing memory.
  method Action setLoadType(TOKEN_INDEX t, Maybe#(ISA_MEMOP_TYPE) mt);
  method Action setStoreType(TOKEN_INDEX t, Maybe#(ISA_MEMOP_TYPE) mt);
  method Action setStoreDataValid(TOKEN_INDEX t);
  method Action setStoreSquashed(TOKEN_INDEX t);
  
  // Set when all destinations have been written.
  method Action setAllDestsValid(TOKEN_INDEX t);

  // Set whether or not the instruction should be emulated in software.
  method Action setEmulation(TOKEN_INDEX t, Bool em);
  
  // Faults
  method Action setFault(TOKEN_INDEX t, FUNCP_FAULT fault_code);
  
  // Rollback the allocations younger than t.
  method Action deallocateForRewind(TOKEN_INDEX t);
  method Action rewindTo(TOKEN_INDEX t);
  
  // Test if it is possible for a token to start a given state.
  // These return false if the previous stage is still unfinished.
  method Bool canStartExe(TOKEN_INDEX t);
  method Bool canStartDTrans(TOKEN_INDEX t);
  method Bool canStartLoad(TOKEN_INDEX t);
  method Bool canStartStore(TOKEN_INDEX t);
  method Bool canStartCommit(TOKEN_INDEX t);
  
  // Accessor methods.
  method Bool isAllocated(TOKEN_INDEX t, FUNCP_ALLOC_RP readPort);
  method Bool isLoad(TOKEN_INDEX t);
  method Bool isStore(TOKEN_INDEX t);
  method Bool isStoreDataValid(TOKEN_INDEX t);
  method Bool isStoreSquashed(TOKEN_INDEX t);
  method Bool allDestsValid(TOKEN_INDEX t);
  method Bool destWritesInFlight(TOKEN_INDEX t);
  method Bool emulateInstruction(TOKEN_INDEX t);
  method Maybe#(FUNCP_FAULT) getFault(TOKEN_INDEX t);
  method MEM_OFFSET getMemOpOffset(TOKEN_INDEX t);
  method ISA_MEMOP_TYPE getLoadType(TOKEN_INDEX t);
  method ISA_MEMOP_TYPE getStoreType(TOKEN_INDEX t);
  method TOKEN_INDEX youngest(CONTEXT_ID ctx_id);
  method TOKEN_INDEX oldest(CONTEXT_ID ctx_id);

  method Bool canEmulate(TOKEN_INDEX t);
  method Bool canRewind(TOKEN_INDEX t);
  
endinterface

// mkFUNCP_Scoreboard

module [Connected_Module] mkFUNCP_Scoreboard 
    // interface:
        (FUNCP_SCOREBOARD);

    // ***** Local State ***** //

    LUTRAM_MULTI_READ#(ALLOC_NUM_READ_PORTS, TOKEN_INDEX, Bool) alloc <-
        mkMultiReadLUTRAM(False);

    if (ALLOC_RP_LAST_SLOT != unpack(fromInteger(valueOf(ALLOC_NUM_READ_PORTS))))
    begin
        error("ALLOC_RP_LAST_SLOT != valueOf(FUNCP_ALLOC_RP))");
    end


    //
    // The actual scoreboards.  Scoreboards in the decode pipeline are
    // less expensive mkLiveTokenLUTRAMU because they are written only
    // once as part of reset/initialization.  All other tokens are both
    // reset during allocation in the decode pipeline and then set
    // in their main pipelines.
    //
    LUTRAM#(TOKEN_INDEX, Bool) finishedDEC <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, Bool) tokIsLoad   <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, Bool) tokIsStore  <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD startedEXE    <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD finishedEXE   <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD startedDTR    <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD finishedDTR   <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD startedLOA    <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD finishedLOA   <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD startedSTO    <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD finishedSTO   <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD startedCOM    <- mkLiveTokenScoreboardU();

    TOKEN_SCOREBOARD tokStoreDataValid <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD tokStoreSquashed <- mkLiveTokenScoreboardU();

    TOKEN_SCOREBOARD tokAllDestsValid <- mkLiveTokenScoreboardU();
    LUTRAM#(TOKEN_INDEX, Bool) tokIsEmulated <- mkLiveTokenLUTRAMU();

    LUTRAM#(TOKEN_INDEX, MEM_OFFSET)          memopOffset <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, ISA_MEMOP_TYPE)      loadType    <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, ISA_MEMOP_TYPE)      storeType   <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, TOKEN_ID)            nextTok     <- mkLiveTokenLUTRAMU();
    
    // Fault is stored as separate arrays for each fault type to avoid
    // causing cross-dependence between functional partition rules that
    // raise faults.
    TOKEN_SCOREBOARD faultIllegalInstr  <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD faultITrans        <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD faultITrans2       <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD faultDTrans        <- mkLiveTokenScoreboardU();
    TOKEN_SCOREBOARD faultDTrans2       <- mkLiveTokenScoreboardU();

    // A pointer to the next token to be allocated.
    Reg#(Vector#(NUM_CONTEXTS, TOKEN_ID)) nextFreeTok <- mkReg(replicate(0));

    // A pointer to the next store token to be allocated.
    Reg#(Vector#(NUM_CONTEXTS, STORE_TOKEN_ID)) nextFreeStoreTok <- mkReg(replicate(0));

    // A pointer to the oldest active token.
    Reg#(Vector#(NUM_CONTEXTS, TOKEN_ID)) oldestTok <- mkReg(replicate(0));
    
    // A register tracking how many tokens are active in pipelines.
    Vector#(NUM_CONTEXTS, COUNTER_Z#(TOKEN_ID_SIZE)) numInDEC = newVector();
    Vector#(NUM_CONTEXTS, COUNTER_Z#(TOKEN_ID_SIZE)) numInEXE = newVector();
    Vector#(NUM_CONTEXTS, COUNTER_Z#(TOKEN_ID_SIZE)) numInDTR = newVector();
    Vector#(NUM_CONTEXTS, COUNTER_Z#(TOKEN_ID_SIZE)) numInLOA = newVector();
    Vector#(NUM_CONTEXTS, COUNTER_Z#(TOKEN_ID_SIZE)) numInSTO = newVector();
    Vector#(NUM_CONTEXTS, COUNTER_Z#(TOKEN_ID_SIZE)) numInCOM = newVector();

    for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
    begin
        numInDEC[c] <- mkLCounter_Z(0);
        numInEXE[c] <- mkLCounter_Z(0);
        numInDTR[c] <- mkLCounter_Z(0);
        numInLOA[c] <- mkLCounter_Z(0);
        numInSTO[c] <- mkLCounter_Z(0);
        numInCOM[c] <- mkLCounter_Z(0);
    end


    // ***** Assertion Checkers ***** //

    // Use multiple assertion nodes because we have so many assertions.
    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_REGSTATE_SCOREBOARD__BASE);

    // Do we have enough tokens to do everything the timing model wants us to?
    ASSERTION assertEnoughTokens <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_OUT_OF_TOKENS, ASSERT_ERROR, assertNode);

    // Don't allocate a token which is already allocated.
    ASSERTION assert_token_is_not_allocated <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_REALLOCATE, ASSERT_ERROR, assertNode);

    // Poisoned instruction
    ASSERTION assertPoisonInstr           <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_COMMIT_POISON_INSTR, ASSERT_ERROR, assertNode);

    // Store lifetime too long before commitStores
    ASSERTION assertTokenStoreLifetime   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_COMMIT_STORE_LIFETIME, ASSERT_ERROR, assertNode);

    // Store lifetime too long before commitStores
    ASSERTION assertIllegalStoreToken   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_ILLEGAL_STORE_TOKEN, ASSERT_ERROR, assertNode);

    // The following assertions make sure things happen at the right time.
    ASSERTION assertTokenCanFinishDEC   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_DECODE, ASSERT_ERROR, assertNode);
    ASSERTION assertTokenCanFinishEXE   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_EXECUTE, ASSERT_ERROR, assertNode);
    ASSERTION assertTokenCanFinishDTR   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_DTRANS, ASSERT_ERROR, assertNode); 
    ASSERTION assertTokenCanFinishLOA   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_LOAD, ASSERT_ERROR, assertNode);
    ASSERTION assertTokenCanFinishSTO   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_STORE, ASSERT_ERROR, assertNode);

    // ***** Rules ***** //

    //
    // computeCanRewind and computeExeEmpty exist to simplify the Bluespec
    // schedule by computing the canEmulate and canRewind predicates in a single
    // piece of logic, each cycle.  They are consumed by the canEmulate and
    // canRewind methods below.  Without them the vectors of all the counter
    // references are unrolled as predicates to rules that call the methods.
    //

    Wire#(Bit#(NUM_CONTEXTS)) contextCanRewind <- mkBypassWire();
    Wire#(Bool) exeStageEmpty <- mkBypassWire();

    (* fire_when_enabled *)
    rule computeCanRewind (True);
        Bit#(NUM_CONTEXTS) can_rew = ?;

        for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
        begin
            can_rew[c] =
                (numInDEC[c].isZero() &&
                 numInEXE[c].isZero() &&
                 numInDTR[c].isZero() &&
                 numInLOA[c].isZero() &&
                 numInSTO[c].isZero() &&
                 numInCOM[c].isZero()) ? 1 : 0;
        end

        contextCanRewind <= can_rew;
    endrule

    (* fire_when_enabled *)
    rule computeExeEmpty (True);
        Bool exe_empty = True;
        for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
        begin
            exe_empty = exe_empty && numInEXE[c].isZero();
        end

        exeStageEmpty <= exe_empty;
    endrule


    // ***** Helper Functions ***** //

    function Bool tokIdxIsAllocated(TOKEN_INDEX tokIdx, FUNCP_ALLOC_RP readPort);
        return alloc.readPorts[pack(readPort)].sub(tokIdx);
    endfunction

    function Bool tokIdxAliasIsAllocated(TOKEN_INDEX tokIdx, FUNCP_ALLOC_RP readPort);
        //
        // We allocate only half the tokens at once in order to enable relative
        // age comparison of tokens.  The alias of a token shares all bits of
        // its index but the high bit.
        //
        let high_bit_num = valueOf(TOKEN_ID_SIZE) - 1;

        let alias_idx = tokIdx;
        alias_idx.token_id[high_bit_num] = alias_idx.token_id[high_bit_num] ^ 1;
    
        return tokIdxIsAllocated(alias_idx, readPort);
    endfunction


    function Bool canAllocate(CONTEXT_ID ctx_id, FUNCP_ALLOC_RP readPort);
        //
        // To allocate, we check two things:
        //
        //   1.  The alias for the next token (the token with the same number
        //       except for the high bit) is not in use.
        //   2.  The high bit of the number of in flight tokens is 0.
        //        
        let next_tok_idx = tokenIndexFromIds(ctx_id, nextFreeTok[ctx_id]);
        let num_in_flight = nextFreeTok[ctx_id] - oldestTok[ctx_id];

        let high_bit_num = valueOf(TOKEN_ID_SIZE) - 1;

        return (num_in_flight[high_bit_num] == 0 &&
                ! tokIdxAliasIsAllocated(next_tok_idx, readPort));
    endfunction


    //
    // checkFaults --
    //     Check all fault bits for a token.
    //
    function Maybe#(FUNCP_FAULT) checkFaults(TOKEN_INDEX t);
    
        if (faultIllegalInstr.sub(t))
            return tagged Valid FAULT_ILLEGAL_INSTR;
        else if (faultITrans.sub(t))
            return tagged Valid FAULT_ITRANS;
        else if (faultITrans2.sub(t))
            return tagged Valid FAULT_ITRANS2;
        else if (faultDTrans.sub(t))
            return tagged Valid FAULT_DTRANS;
        else if (faultDTrans2.sub(t))
            return tagged Valid FAULT_DTRANS2;
        else
            return tagged Invalid;

    endfunction

    //
    // checkDestWritesInFlight --
    //     Helper function for destWritesInFlight method.  Compute whether
    //     the token has reached the execute stage without yet updating all
    //     destinations.
    //
    function Bool checkDestWritesInFlight(TOKEN_INDEX t);
        return startedEXE.sub(t) && ! tokAllDestsValid.sub(t);
    endfunction

    // deallocate

    // When:   Any time.
    // Effect: Reset the allocation bit. Update the oldest-token pointer.

    method Action deallocate(TOKEN_INDEX t);

        let ctx_id = t.context_id;

        // Update the oldest token.
        oldestTok[ctx_id] <= nextTok.sub(t);

        // Update the allocated bit
        alloc.upd(t, False);

    endmethod

    // allocate

    // When:   When the next token to be allocated is not "busy"
    //         IE it's not in an indeterminate state. 
    //         As long as every macro-operation eventually completes forward progress will be made.
    // Effect: Allocate a token and reset the entire set of scoreboard states.

    method ActionValue#(TOKEN_INDEX) allocate(CONTEXT_ID ctx_id);

        let new_tok = tokenIndexFromIds(ctx_id, nextFreeTok[ctx_id]);

        // Assert the the token wasn't already allocated.
        assert_token_is_not_allocated(! tokIdxIsAllocated(new_tok, ALLOC_RP_ALLOC0));

        // Assert that we haven't run out of tokens.
        assertEnoughTokens(canAllocate(ctx_id, ALLOC_RP_ALLOC1));

        // Update the allocated bit
        alloc.upd(new_tok, True);
        numInDEC[new_tok.context_id].up();

        //
        // Reset all the scoreboards.  A few are not set in order to avoid
        // hazards in the getDependencies pipeline where tokens are allocated.
        // They will be initialized before the token is passed to the timing
        // model.
        //
        finishedDEC.upd(new_tok, True);   // Set speculatively to true to avoid
                                          // getDepencies pipeline hazard
        // tokIsLoad.upd(new_tok, False);       *** Set in getDependencies
        // tokIsStore.upd(new_tok, False);      *** Set in getDependencies
        startedEXE.reset(new_tok);
        finishedEXE.reset(new_tok);
        startedDTR.reset(new_tok);
        finishedDTR.reset(new_tok);
        startedLOA.reset(new_tok);
        finishedLOA.reset(new_tok);
        startedSTO.reset(new_tok);
        finishedSTO.reset(new_tok);
        startedCOM.reset(new_tok);

        tokStoreDataValid.reset(new_tok);
        tokStoreSquashed.reset(new_tok);
        tokAllDestsValid.reset(new_tok);
        // tokIsEmulated.upd(new_tok, False);   *** Set in getDependencies

        faultIllegalInstr.reset(new_tok);
        faultITrans.reset(new_tok);
        faultITrans2.reset(new_tok);
        faultDTrans.reset(new_tok);
        faultDTrans2.reset(new_tok);

        // Update the free pointer.
        let next_token_idx = new_tok + 1;
        nextTok.upd(new_tok, next_token_idx.token_id);
        nextFreeTok[ctx_id] <= next_token_idx.token_id;

        return new_tok;

    endmethod


    // allocateStore

    // When:   Commit of a store instruction.
    // Effect: Allocate a store token so the main token can be released.

    method ActionValue#(STORE_TOKEN_INDEX) allocateStore(TOKEN_INDEX t);
        let ctx_id = t.context_id;

        // Token must be a store and must have locally commited the store
        assertIllegalStoreToken(tokIsStore.sub(t) && finishedSTO.sub(t));

        //
        // This code does not check that the token is not busy.  The store
        // buffer will check.
        //
        let new_tok = storeTokenIndexFromIds(ctx_id, nextFreeStoreTok[ctx_id]);

        // Update the free pointer.
        nextFreeStoreTok[ctx_id] <= nextFreeStoreTok[ctx_id] + 1;

        return new_tok;
    endmethod


    // decFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action decFinish(TOKEN_INDEX t);

        // finishedDEC was already set, speculatively, to avoid a pipeline
        // hazard in getDependencies.
        // finishedDEC.upd(t, True);

        numInDEC[t.context_id].down();

    endmethod

    // exeStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action exeStart(TOKEN_INDEX t);

        startedEXE.upd(t, True);
        numInEXE[t.context_id].up();

    endmethod

    // exeFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action exeFinish(TOKEN_INDEX t);

        assertTokenCanFinishEXE(startedEXE.sub(t));

        finishedEXE.upd(t, True);
        numInEXE[t.context_id].down();

    endmethod

    // dTransStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action dTransStart(TOKEN_INDEX t);

        startedDTR.upd(t, True);
        numInDTR[t.context_id].up();

    endmethod

    // dTransFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action dTransFinish(TOKEN_INDEX t);

        assertTokenCanFinishDTR(startedDTR.sub(t));

        finishedDTR.upd(t, True);
        numInDTR[t.context_id].down();

    endmethod

    // loadStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action loadStart(TOKEN_INDEX t);

        startedLOA.upd(t, True);
        numInLOA[t.context_id].up();

    endmethod

    // loadFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action loadFinish(TOKEN_INDEX t);

        assertTokenCanFinishLOA(startedLOA.sub(t));

        finishedLOA.upd(t, True);
        numInLOA[t.context_id].down();

    endmethod

    // storeStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action storeStart(TOKEN_INDEX t);

        startedSTO.upd(t, True);
        numInSTO[t.context_id].up();

    endmethod

    // storeFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action storeFinish(TOKEN_INDEX t);

        assertTokenCanFinishSTO(startedSTO.sub(t));

        finishedSTO.upd(t, True);
        numInSTO[t.context_id].down();

    endmethod

    // commitStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action commitStart(TOKEN_INDEX t);

        assertPoisonInstr( ! isValid(checkFaults(t)) );

        startedCOM.upd(t, True);
        numInCOM[t.context_id].up();

    endmethod

    method Action commitFinish(TOKEN_INDEX t);

        // Record that the token has finished commit.
        numInCOM[t.context_id].down();

    endmethod

    // setMemOpOffset

    // When:   Any time.
    // Effect: Record the fetch offset.

    method Action setMemOpOffset(TOKEN_INDEX t, MEM_OFFSET offset);
    
        memopOffset.upd(t, offset);
    
    endmethod

    // setLoadType

    // When:   Any time.
    // Effect: Record the store type and mark the token as a store.

    method Action setLoadType(TOKEN_INDEX t, Maybe#(ISA_MEMOP_TYPE) mtype);
    
        tokIsLoad.upd(t, isValid(mtype));
        loadType.upd(t, validValue(mtype));
    
    endmethod

    // setStoreType

    // When:   Any time.
    // Effect: Record the store type and mark the token as a store.

    method Action setStoreType(TOKEN_INDEX t, Maybe#(ISA_MEMOP_TYPE) mtype);
    
        tokIsStore.upd(t, isValid(mtype));
        storeType.upd(t, validValue(mtype));
    
    endmethod


    // setStoreDataValid

    // When:   Any time.
    // Effect: Flag token's store data as valid.  Store data is recorded
    //         separately in BRAM.

    method Action setStoreDataValid(TOKEN_INDEX t);
    
        tokStoreDataValid.upd(t, True);
    
    endmethod

    // setStoreSquashed

    // When:   Any time.
    // Effect: Flag token's store as disabled.  (E.g. Alpha store conditional
    //         failure.)

    method Action setStoreSquashed(TOKEN_INDEX t);
    
        tokStoreSquashed.upd(t, True);
    
    endmethod

    // setAllDestsValid

    // When:   Any time.
    // Effect: Assert that all of the token's destinations (both register and
    //         store data) have been written AND that no further updates are
    //         pending.

    method Action setAllDestsValid(TOKEN_INDEX t);
    
        tokAllDestsValid.upd(t, True);
    
    endmethod

    // setEmulation

    // When:   Any time.
    // Effect: Record whether or not the token should be emulated.

    method Action setEmulation(TOKEN_INDEX t, Bool em);
    
        tokIsEmulated.upd(t, em);
            
    endmethod

    // setFault

    // When:   Any time -- typically during execution
    // Effect: Flag an instruction poisoned

    method Action setFault(TOKEN_INDEX t, FUNCP_FAULT fault_code);
    
        // Only set fault if one hasn't been raised already
        case (fault_code)
            FAULT_ILLEGAL_INSTR:
                faultIllegalInstr.upd(t, True);
            FAULT_ITRANS:
                faultITrans.upd(t, True);
            FAULT_ITRANS2:
                faultITrans2.upd(t, True);
            FAULT_DTRANS:
                faultDTrans.upd(t, True);
            FAULT_DTRANS2:
                faultDTrans2.upd(t, True);
        endcase
            
    endmethod


    // deallocateForRewind
    
    // When:   Any time.
    // Effect: Deactivate a token during rewind.  The oldest token pointer must
    //         be managed during rewind by calling rewindTo() separately.

    method Action deallocateForRewind(TOKEN_INDEX t);

        alloc.upd(t, False);

    endmethod


    // rewindTo
    
    // When:   Any time.
    // Effect: Undo meta-data associated with allocations following a rewind
    //         to token t.  The actual alloc vector must be updated separately
    //         by calling deallocateForRewind().

    method Action rewindTo(TOKEN_INDEX t);

        let ctx_id = t.context_id;

        // nextFreeTok does not change because we don't want to reissue those tokens
        // until the next time we wrap around.
      
        // However we can update oldestTok here. Specifically, if the token you rewound
        // to was already committed, then if it was a legal rewind (checked elsewhere) then
        // after the rewind there will be no tokens in flight. In the case we can jump 
        // oldestTok up to nextFreeTok (so num_in_flight will be zero). Thus we can
        // reclaim tokens slightly more aggressively.
        if (!tokIdxIsAllocated(t, ALLOC_RP_REWIND))
        begin
        
            // t is not allocated, and there's no one older, so there are no tokens in flight.
            oldestTok[ctx_id] <= nextFreeTok[ctx_id];

        end
        else
        begin
            // t is allocated. Mark what the token committed after t should be.
            nextTok.upd(t, nextFreeTok[ctx_id]);
        end

    endmethod

    // isAllocated

    // When:   Any time.
    // Effect: Accessor method.

    method Bool isAllocated(TOKEN_INDEX t, FUNCP_ALLOC_RP readPort);

      return tokIdxIsAllocated(t, readPort);

    endmethod

    // isLoad
    
    // When:   Any time.
    // Effect: Accessor method.

    method Bool isLoad(TOKEN_INDEX t);
    
        return tokIsLoad.sub(t);
    
    endmethod

    // isStore
    
    // When:   Any time.
    // Effect: Accessor method.

    method Bool isStore(TOKEN_INDEX t);

        return tokIsStore.sub(t);

    endmethod

    // emulateInstruction
    
    // When:   Any time.
    // Effect: Accessor method.

    method Bool emulateInstruction(TOKEN_INDEX t);

        return tokIsEmulated.sub(t);

    endmethod

    // getFault
    
    // When:   Any time.
    // Effect: Accessor method.

    method Maybe#(FUNCP_FAULT) getFault(TOKEN_INDEX t);

        return checkFaults(t);

    endmethod

    // getMemOpOffset
    
    // When:   Any time.
    // Effect: Accessor method.

    method MEM_OFFSET getMemOpOffset(TOKEN_INDEX t);
    
        return memopOffset.sub(t);
    
    endmethod

    // getLoadType
    
    // When:   Any time.
    // Effect: Accessor method.

    method ISA_MEMOP_TYPE getLoadType(TOKEN_INDEX t);
    
        return loadType.sub(t);
    
    endmethod

    // getStoreType
    
    // When:   Any time.
    // Effect: Accessor method

    method ISA_MEMOP_TYPE getStoreType(TOKEN_INDEX t);
    
        return storeType.sub(t);
    
    endmethod

    // isStoreDataValid
    
    // When:   Any time.
    // Effect: Accessor method

    method Bool isStoreDataValid(TOKEN_INDEX t);
    
        return tokStoreDataValid.sub(t);
    
    endmethod

    // isStoreSquashed
    
    // When:   Any time.
    // Effect: Accessor method

    method Bool isStoreSquashed(TOKEN_INDEX t);
    
        return tokStoreSquashed.sub(t);
    
    endmethod

    // allDestsValid
    
    // When:   Any time.
    // Effect: Accessor method

    method Bool allDestsValid(TOKEN_INDEX t);
    
        return tokAllDestsValid.sub(t);
    
    endmethod

    // destWritesInFlight
    
    // When:   Any time.
    // Effect: Compute whether token destination writes are in flight

    method Bool destWritesInFlight(TOKEN_INDEX t);

        return checkDestWritesInFlight(t);
    
    endmethod

    // youngest

    // When:   Any time.
    // Effect: Accessor method.

    method TOKEN_INDEX youngest(CONTEXT_ID ctx_id);

        return tokenIndexFromIds(ctx_id, nextFreeTok[ctx_id] - 1);

    endmethod

    // oldest

    // When:   Any time.
    // Effect: Accessor method.

    method TOKEN_INDEX oldest(CONTEXT_ID ctx_id);

        return tokenIndexFromIds(ctx_id, oldestTok[ctx_id]);

    endmethod

    // canEmulate

    // When:   Any time.
    // Effect: Accessor method. Returns true if no instructions are in any pipeline except EXE.

    method Bool canEmulate(TOKEN_INDEX t);
        return exeStageEmpty && (contextCanRewind[t.context_id] == 1);
    endmethod

    // canRewind

    // When:   Any time.
    // Effect: Accessor method. Returns true if no instructions are in any pipeline.

    method Bool canRewind(TOKEN_INDEX t);
        return contextCanRewind[t.context_id] == 1;
    endmethod

    method Bool canStartExe(TOKEN_INDEX t);
        return finishedDEC.sub(t);
    endmethod

    method Bool canStartDTrans(TOKEN_INDEX t);
        return finishedEXE.sub(t);
    endmethod

    method Bool canStartLoad(TOKEN_INDEX t);
        return finishedDTR.sub(t);
    endmethod

    method Bool canStartStore(TOKEN_INDEX t);
        return finishedDTR.sub(t);
    endmethod

    method Bool canStartCommit(TOKEN_INDEX t);
        let bad_mem_op = faultDTrans.sub(t) || faultDTrans2.sub(t) ||
                         tokStoreSquashed.sub(t);

        if (checkDestWritesInFlight(t))
        begin
            // Must wait for all destination writes to complete before
            // deallocating the token.
            return False;
        end
        else if (tokIsLoad.sub(t))
        begin
            // Load
            return finishedLOA.sub(t) || bad_mem_op;
        end
        else if (tokIsStore.sub(t))
        begin
            // Store
            return finishedSTO.sub(t) || bad_mem_op;
        end
        else
        begin
            // Normal op
            return finishedEXE.sub(t);
        end
    endmethod

endmodule
