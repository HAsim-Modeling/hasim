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

// A scoreboard to track information of the status of in-flight instructions.

// Note: We use a One-Hot encoding to improve throughput by reducing rule conflicts.

// Note: We allocate only half the tokens at once.
//       This allows the user to check the relative age between two arbitrary tokens.

// Library imports

import Vector::*;

// Project imports

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
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


// TOKEN_SCOREBOARD

// Because the whole system is made of reg files of Bools, we use
// this typdef as a convenience.

typedef LUTRAM#(TOKEN_INDEX, Bool) TOKEN_SCOREBOARD;

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
  method Action setLoadType(TOKEN_INDEX t,  ISA_MEMOP_TYPE mt);
  method Action setStoreType(TOKEN_INDEX t, ISA_MEMOP_TYPE mt);
  
  // Set whether or not the instruction should be emulated in software.
  method Action setEmulation(TOKEN_INDEX t, Bool em);
  
  // Faults
  method Action setFault(TOKEN_INDEX t, FUNCP_FAULT fault_code);
  
  // Rollback the allocations younger than t.
  method Action rewindTo(TOKEN_INDEX t);
  
  // Test if it is possible for a token to start a given state.
  // These return false if the previous stage is still unfinished.
  method Bool canStartExe(TOKEN_INDEX t);
  method Bool canStartDTrans(TOKEN_INDEX t);
  method Bool canStartLoad(TOKEN_INDEX t);
  method Bool canStartStore(TOKEN_INDEX t);
  method Bool canStartCommit(TOKEN_INDEX t);
  
  // Accessor methods.
  method Bool isAllocated(TOKEN_INDEX t);
  method Bool isLoad(TOKEN_INDEX t);
  method Bool isStore(TOKEN_INDEX t);
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

    // Rewind operates on sets of bits within a context, so organize the
    // alloc vector by context ID.
    Vector#(NUM_CONTEXTS, Reg#(Vector#(NUM_TOKENS_PER_CONTEXT, Bool))) alloc = newVector();
    for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
    begin
        alloc[c] <- mkReg(replicate(False));
    end

    // The actual scoreboards.
    TOKEN_SCOREBOARD finishedDEC   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD tokIsLoad     <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD tokIsStore    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD startedEXE    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD finishedEXE   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD startedDTR    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD finishedDTR   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD startedLOA    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD finishedLOA   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD startedSTO    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD finishedSTO   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD startedCOM    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD tokIsEmulated <- mkLiveTokenLUTRAMU();

    LUTRAM#(TOKEN_INDEX, MEM_OFFSET)          memopOffset <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, ISA_MEMOP_TYPE)      loadType    <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, ISA_MEMOP_TYPE)      storeType   <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, TOKEN_ID)            nextTok     <- mkLiveTokenLUTRAMU();
    
    // Fault is stored as separate arrays for each fault type to avoid
    // causing cross-dependence between functional partition rules that
    // raise faults.
    TOKEN_SCOREBOARD faultIllegalInstr  <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD faultITrans        <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD faultITrans2       <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD faultDTrans        <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD faultDTrans2       <- mkLiveTokenLUTRAMU();

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
        Bit#(NUM_CONTEXTS) can_rew;

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

    function Bool tokIdxIsAllocated(TOKEN_INDEX tokIdx);
        return alloc[tokIdx.context_id][tokIdx.token_id];
    endfunction

    function Bool tokIdxAliasIsAllocated(TOKEN_INDEX tokIdx);
        //
        // We allocate only half the tokens at once in order to enable relative
        // age comparison of tokens.  The alias of a token shares all bits of
        // its index but the high bit.
        //
        let high_bit_num = valueOf(TOKEN_ID_SIZE) - 1;

        let alias_idx = tokIdx;
        alias_idx.token_id[high_bit_num] = alias_idx.token_id[high_bit_num] ^ 1;
    
        return tokIdxIsAllocated(alias_idx);
    endfunction


    function Bool canAllocate(CONTEXT_ID ctx_id);
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

        return (num_in_flight[high_bit_num] == 0 && ! tokIdxAliasIsAllocated(next_tok_idx));
    endfunction


    // isBusy

    // A token is said to be "busy" if it has started a macro-operation but not finished it yet.
    
    function Bool isBusy(TOKEN_INDEX t);

        // Has this token started a macro operation but not finished it?
        let dec_busy =     !finishedDEC.sub(t);
        let exe_busy =     startedEXE.sub(t) && !finishedEXE.sub(t);
        let dtr_busy =     startedDTR.sub(t) && !finishedDTR.sub(t);
        let load_busy =    startedLOA.sub(t) && !finishedLOA.sub(t);
        let store_busy =   startedSTO.sub(t) && !finishedSTO.sub(t);
        // It's not done committing if it's still allocated.  alloc tested later.
        let commit_busy = startedCOM.sub(t);

        // If it is in any macro operation it is busy.
        return tokIdxIsAllocated(t) && (dec_busy || exe_busy || dtr_busy || load_busy || store_busy || commit_busy);

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

    // deallocate

    // When:   Any time.
    // Effect: Reset the allocation bit. Update the oldest-token pointer.

    method Action deallocate(TOKEN_INDEX t);

        let ctx_id = t.context_id;

        // Update the oldest token.
        oldestTok[ctx_id] <= nextTok.sub(t);

        // Update the allocated bit
        alloc[t.context_id][t.token_id] <= False;

    endmethod

    // allocate

    // When:   When the next token to be allocated is not "busy"
    //         IE it's not in an indeterminate state. 
    //         As long as every macro-operation eventually completes forward progress will be made.
    // Effect: Allocate a token and reset the entire set of scoreboard states.

    method ActionValue#(TOKEN_INDEX) allocate(CONTEXT_ID ctx_id);

        let new_tok = tokenIndexFromIds(ctx_id, nextFreeTok[ctx_id]);

        // Assert the the token wasn't already allocated.
        assert_token_is_not_allocated(! tokIdxIsAllocated(new_tok));

        // Assert that we haven't run out of tokens.
        assertEnoughTokens(canAllocate(ctx_id));

        // Update the allocated bit
        alloc[new_tok.context_id][new_tok.token_id] <= True;
        numInDEC[new_tok.context_id].up();

        // Reset all the scoreboards.
        finishedDEC.upd(new_tok, False);
        tokIsLoad.upd(new_tok, False);
        tokIsStore.upd(new_tok, False);
        startedEXE.upd(new_tok, False);
        finishedEXE.upd(new_tok, False);
        startedDTR.upd(new_tok, False);
        finishedDTR.upd(new_tok, False);
        startedLOA.upd(new_tok, False);
        finishedLOA.upd(new_tok, False);
        startedSTO.upd(new_tok, False);
        finishedSTO.upd(new_tok, False);
        startedCOM.upd(new_tok, False);

        tokIsEmulated.upd(new_tok, False);

        faultIllegalInstr.upd(new_tok, False);
        faultITrans.upd(new_tok, False);
        faultITrans2.upd(new_tok, False);
        faultDTrans.upd(new_tok, False);
        faultDTrans2.upd(new_tok, False);

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

        // FIXME --
        // For some reason adding the following test adds a scheduling conflict
        // that I can't figure out.  Everything works fine without it.
        //assertTokenCanFinishDEC(startedDEC.sub(t));

        finishedDEC.upd(t, True);
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

    method Action setLoadType(TOKEN_INDEX t, ISA_MEMOP_TYPE mtype);
    
        tokIsLoad.upd(t, True);
        
        loadType.upd(t, mtype);
    
    endmethod

    // setStoreType

    // When:   Any time.
    // Effect: Record the store type and mark the token as a store.

    method Action setStoreType(TOKEN_INDEX t, ISA_MEMOP_TYPE mtype);
    
        tokIsStore.upd(t, True);
        
        storeType.upd(t, mtype);
    
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

    // rewindTo
    
    // When:   Any time.
    // Effect: Undo all allocations younger than parameter t.

    method Action rewindTo(TOKEN_INDEX t);

        let ctx_id = t.context_id;

        // nextFreeTok does not change because we don't want to reissue those tokens
        // until the next time we wrap around.
      
        // However we can update oldestTok here. Specifically, if the token you rewound
        // to was already committed, then if it was a legal rewind (checked elsewhere) then
        // after the rewind there will be no tokens in flight. In the case we can jump 
        // oldestTok up to nextFreeTok (so num_in_flight will be zero). Thus we can
        // reclaim tokens slightly more aggressively.
        if (!tokIdxIsAllocated(t))
        begin
        
            // t is not allocated, and there's no one older, so there are no tokens in flight.
            oldestTok[ctx_id] <= nextFreeTok[ctx_id];

        end
        else
        begin
            // t is allocated. Mark what the token committed after t should be.
            nextTok.upd(t, nextFreeTok[ctx_id]);
        end

        //
        // Update the alloc vector.
        //

        Vector#(NUM_TOKENS_PER_CONTEXT, Bool) as = alloc[ctx_id];

        for (Integer x = 0; x < valueof(NUM_TOKENS_PER_CONTEXT); x = x + 1)
        begin
            TOKEN_INDEX x_tok = tokenIndexFromIds(ctx_id, fromInteger(x));
            as[x] = tokenIsOlderOrEq(x_tok.token_id, t.token_id) ? as[x] : False;
        end

        alloc[ctx_id] <= as;

    endmethod

    // isAllocated

    // When:   Any time.
    // Effect: Accessor method.

    method Bool isAllocated(TOKEN_INDEX t);

      return tokIdxIsAllocated(t);

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
        let bad_mem_op = faultDTrans.sub(t) || faultDTrans2.sub(t);
        let good_load = finishedLOA.sub(t) || bad_mem_op;
        let good_store = finishedSTO.sub(t) || bad_mem_op;
        return  tokIsLoad.sub(t) ? good_load : 
                tokIsStore.sub(t) ? good_store :
                finishedEXE.sub(t);
    endmethod

endmodule
