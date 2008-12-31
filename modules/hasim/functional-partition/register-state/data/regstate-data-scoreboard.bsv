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
`include "asim/provides/funcp_memory.bsh"
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
  
  // These methods track the internal status of which macro-operation a token is in.
  method Action iTransStart(TOKEN_INDEX t);
  method Action iTransFinish(TOKEN_INDEX t);
  method Action fetStart(TOKEN_INDEX t);
  method Action fetFinish(TOKEN_INDEX t);
  method Action decStart(TOKEN_INDEX t);
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
  
  // Set the offsets after we align the address.
  method Action setFetchOffset(TOKEN_INDEX t, MEM_OFFSET o);
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
  
  // Accessor methods.
  method Bool isAllocated(TOKEN_INDEX t);
  method Bool isLoad(TOKEN_INDEX t);
  method Bool isStore(TOKEN_INDEX t);
  method Bool emulateInstruction(TOKEN_INDEX t);
  method Maybe#(FUNCP_FAULT) getFault(TOKEN_INDEX t);
  method MEM_OFFSET getFetchOffset(TOKEN_INDEX t);
  method MEM_OFFSET getMemOpOffset(TOKEN_INDEX t);
  method ISA_MEMOP_TYPE getLoadType(TOKEN_INDEX t);
  method ISA_MEMOP_TYPE getStoreType(TOKEN_INDEX t);
  method TOKEN_INDEX youngest(CONTEXT_ID ctx_id);
  method TOKEN_INDEX oldest(CONTEXT_ID ctx_id);

  // youngestDecoded can be useful when rewinding.  There are times when it
  // may point to a token that hasn't yet been decoded, but it will never
  // point to a token older than the youngest decoded.
  method TOKEN_INDEX youngestDecoded(CONTEXT_ID ctx_id);
  // Safe is the same as youngestDecoded() but the response must be no older
  // than the input argument.
  method TOKEN_INDEX youngestDecoded_Safe(TOKEN_INDEX t);

  method Bool canEmulate();
  method Bool canRewind();
  
endinterface

// mkFUNCP_Scoreboard

module [Connected_Module] mkFUNCP_Scoreboard 
    // interface:
        (FUNCP_SCOREBOARD);

    // ***** Local State ***** //

    // We keep the status bits in a register for fast pull-downs.
    // NOTE:  This vector is over all tokens, not just live tokens
    Reg#(Vector#(NUM_TOKENS, Bool)) alloc <- mkReg(replicate(False));

    // The actual scoreboards.
    TOKEN_SCOREBOARD itr_start    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD itr_finish   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD fet_start    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD fet_finish   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD dec_start    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD dec_finish   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD is_load      <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD is_store     <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD exe_start    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD exe_finish   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD dtr_start    <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD dtr_finish   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD load_start   <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD load_finish  <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD store_start  <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD store_finish <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD commit_start <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD emulation    <- mkLiveTokenLUTRAMU();

    LUTRAM#(TOKEN_INDEX, MEM_OFFSET)          fetch_offset <- mkLiveTokenLUTRAM(0);
    LUTRAM#(TOKEN_INDEX, MEM_OFFSET)          memop_offset <- mkLiveTokenLUTRAM(0);
    LUTRAM#(TOKEN_INDEX, ISA_MEMOP_TYPE)      load_type    <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, ISA_MEMOP_TYPE)      store_type   <- mkLiveTokenLUTRAMU();
    LUTRAM#(TOKEN_INDEX, TOKEN_ID)            next_tok     <- mkLiveTokenLUTRAMU();
    
    // Fault is stored as separate arrays for each fault type to avoid
    // causing cross-dependence between functional partition rules that
    // raise faults.
    TOKEN_SCOREBOARD fault_illegal_instr <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD fault_itrans        <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD fault_itrans2       <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD fault_dtrans        <- mkLiveTokenLUTRAMU();
    TOKEN_SCOREBOARD fault_dtrans2       <- mkLiveTokenLUTRAMU();

    // A pointer to the next token to be allocated.
    Reg#(Vector#(NUM_CONTEXTS, TOKEN_ID)) next_free_tok <- mkReg(replicate(0));

    // A pointer to the oldest active token.
    Reg#(Vector#(NUM_CONTEXTS, TOKEN_ID)) oldest_tok <- mkReg(replicate(0));
    
    // A pointer to the youngest decoded token.
    Reg#(Vector#(NUM_CONTEXTS, TOKEN_ID)) youngestDecodedTok <- mkReg(replicate(0));
    
    // A register tracking how many tokens are active in pipelines.
    COUNTER_Z#(TOKEN_INDEX_SIZE) num_in_itr <- mkLCounter_Z(0);
    COUNTER_Z#(TOKEN_INDEX_SIZE) num_in_fet <- mkLCounter_Z(0);
    COUNTER_Z#(TOKEN_INDEX_SIZE) num_in_dec <- mkLCounter_Z(0);
    COUNTER_Z#(TOKEN_INDEX_SIZE) num_in_exe <- mkLCounter_Z(0);
    COUNTER_Z#(TOKEN_INDEX_SIZE) num_in_dtr <- mkLCounter_Z(0);
    COUNTER_Z#(TOKEN_INDEX_SIZE) num_in_load <- mkLCounter_Z(0);
    COUNTER_Z#(TOKEN_INDEX_SIZE) num_in_store <- mkLCounter_Z(0);
    COUNTER_Z#(TOKEN_INDEX_SIZE) num_in_commit <- mkLCounter_Z(0);
    
    

    // ***** Assertion Checkers ***** //

    // Use multiple assertion nodes because we have so many assertions.
    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_REGSTATE_SCOREBOARD__BASE);
    ASSERTION_NODE assertNodeStart <- mkAssertionNode(`ASSERTIONS_REGSTATE_SCOREBOARD_START__BASE);
    ASSERTION_NODE assertNodeFinish <- mkAssertionNode(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH__BASE);

    // Do we have enough tokens to do everything the timing model wants us to?
    ASSERTION assert_enough_tokens <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_OUT_OF_TOKENS, ASSERT_ERROR, assertNode);

    // Don't allocate a token which is already allocated.
    ASSERTION assert_token_is_not_allocated <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_REALLOCATE, ASSERT_ERROR, assertNode);

    // Don't de-allocate a token which isn't allocated.
    // Assertion assert_token_is_allocated <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_DEALLOCATE, ASSERT_ERROR, assertNode);

    // Are we completing tokens in order?
    // Assertion assert_completing_tokens_in_order <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_COMPLETION, ASSERT_WARNING, assertNode);

    // Poisoned instruction
    ASSERTION assert_poison_instr           <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_COMMIT_POISON_INSTR, ASSERT_ERROR, assertNode);

    // The following assertions make sure things happen at the right time.
    ASSERTION assert_token_can_finish_itr   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_ITRANS, ASSERT_ERROR, assertNodeFinish); 
    ASSERTION assert_token_can_start_fet    <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_START_FETCH, ASSERT_ERROR, assertNodeStart);
    ASSERTION assert_token_can_finish_fet   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_FETCH, ASSERT_ERROR, assertNodeFinish);
    ASSERTION assert_token_can_start_dec    <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_START_DECODE, ASSERT_ERROR, assertNodeStart);
    ASSERTION assert_token_can_finish_dec   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_DECODE, ASSERT_ERROR, assertNodeFinish);
    ASSERTION assert_token_can_start_exe    <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_START_EXECUTE, ASSERT_ERROR, assertNodeStart);
    ASSERTION assert_token_can_finish_exe   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_EXECUTE, ASSERT_ERROR, assertNodeFinish);
    ASSERTION assert_token_can_start_dtr    <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_START_DTRANS, ASSERT_ERROR, assertNodeStart); 
    ASSERTION assert_token_can_finish_dtr   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_DTRANS, ASSERT_ERROR, assertNodeFinish); 
    ASSERTION assert_token_can_start_load   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_START_LOAD, ASSERT_ERROR, assertNodeStart);
    ASSERTION assert_token_can_finish_load  <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_LOAD, ASSERT_ERROR, assertNodeFinish);
    ASSERTION assert_token_can_start_store  <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_START_STORE, ASSERT_ERROR, assertNodeStart);
    ASSERTION assert_token_can_finish_store <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_FINISH_STORE, ASSERT_ERROR, assertNodeFinish);
    ASSERTION assert_token_can_start_commit <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_START_COMMIT, ASSERT_ERROR, assertNodeStart);
    ASSERTION assert_token_has_done_loads   <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_START_COMMIT_WITHOUT_LOAD, ASSERT_ERROR, assertNodeStart);
    ASSERTION assert_token_has_done_stores  <- mkAssertionChecker(`ASSERTIONS_REGSTATE_SCOREBOARD_START_COMMIT_WITHOUT_STORE, ASSERT_ERROR, assertNodeStart);

    // ***** Helper Functions ***** //

    function Bool tokIsAllocated(CONTEXT_ID ctx_id, TOKEN_ID tok_id);
        TOKEN_INDEX idx = tokenIndexFromIds(ctx_id, tok_id);
        return alloc[pack(idx)];
    endfunction


    function Bool canAllocate(CONTEXT_ID ctx_id);

        //
        // We allocate only half the tokens at once in order to enable relative
        // age comparison of tokens.  To allocate, we check two things:
        //
        //   1.  The alias for the next token (the token with the same number
        //       except for the high bit) is not in use.
        //   2.  The high bit of the number of in flight tokens is 0.
        //        
        let high_bit_num = valueOf(TOKEN_ID_SIZE) - 1;

        let next_tok_alias = next_free_tok[ctx_id];
        next_tok_alias[high_bit_num] = next_tok_alias[high_bit_num] ^ 1;

        let num_in_flight = next_free_tok[ctx_id] - oldest_tok[ctx_id];

        return (num_in_flight[high_bit_num] == 0 && ! tokIsAllocated(ctx_id, next_tok_alias));

    endfunction


    // isBusy

    // A token is said to be "busy" if it has started a macro-operation but not finished it yet.
    
    function Bool isBusy(TOKEN_INDEX t);

        // Has this token started a macro operation but not finished it?
        let itr_busy =       itr_start.sub(t) && !itr_finish.sub(t);
        let fet_busy =       fet_start.sub(t) && !fet_finish.sub(t);
        let dec_busy =       dec_start.sub(t) && !dec_finish.sub(t);
        let exe_busy =       exe_start.sub(t) && !exe_finish.sub(t);
        let dtr_busy =       dtr_start.sub(t) && !dtr_finish.sub(t);
        let load_busy =     load_start.sub(t) && !load_finish.sub(t);
        let store_busy =   store_start.sub(t) && !store_finish.sub(t);
        // It's not done committing if it's still allocated.  alloc[t] tested later.
        let commit_busy = commit_start.sub(t);

        // If it is in any macro operation it is busy.
        return alloc[pack(t)] && (itr_busy || fet_busy || dec_busy || exe_busy || dtr_busy || load_busy || store_busy || commit_busy);

    endfunction

    //
    // checkFaults --
    //     Check all fault bits for a token.
    //
    function Maybe#(FUNCP_FAULT) checkFaults(TOKEN_INDEX t);
    
        if (fault_illegal_instr.sub(t))
            return tagged Valid FAULT_ILLEGAL_INSTR;
        else if (fault_itrans.sub(t))
            return tagged Valid FAULT_ITRANS;
        else if (fault_itrans2.sub(t))
            return tagged Valid FAULT_ITRANS2;
        else if (fault_dtrans.sub(t))
            return tagged Valid FAULT_DTRANS;
        else if (fault_dtrans2.sub(t))
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
        oldest_tok[ctx_id] <= next_tok.sub(t);

        if (youngestDecodedTok[ctx_id] == oldest_tok[ctx_id])
            youngestDecodedTok[ctx_id] <= next_tok.sub(t);


        // Update the allocation table.
        alloc <= update(alloc, pack(t), False);
        
        // Record that the token has finished commit.
        num_in_commit.down();

    endmethod

    // allocate

    // When:   When the next token to be allocated is not "busy"
    //         IE it's not in an indeterminate state. 
    //         As long as every macro-operation eventually completes forward progress will be made.
    // Effect: Allocate a token and reset the entire set of scoreboard states.

    method ActionValue#(TOKEN_INDEX) allocate(CONTEXT_ID ctx_id);

        // Assert the the token wasn't already allocated.
        assert_token_is_not_allocated(! tokIsAllocated(ctx_id, next_free_tok[ctx_id]));

        // Assert that we haven't run out of tokens.
        assert_enough_tokens(canAllocate(ctx_id));

        // Update the allocation status.    
        let new_tok = tokenIndexFromIds(ctx_id, next_free_tok[ctx_id]);
        alloc <= update(alloc, pack(new_tok), True);

        // Reset all the scoreboards.
        itr_start.upd(new_tok, False);
        itr_finish.upd(new_tok, False);
        fet_start.upd(new_tok, False);
        fet_finish.upd(new_tok, False);
        dec_start.upd(new_tok, False);
        dec_finish.upd(new_tok, False);
        is_load.upd(new_tok, False);
        is_store.upd(new_tok, False);
        exe_start.upd(new_tok, False);
        exe_finish.upd(new_tok, False);
        dtr_start.upd(new_tok, False);
        dtr_finish.upd(new_tok, False);
        load_start.upd(new_tok, False);
        load_finish.upd(new_tok, False);
        store_start.upd(new_tok, False);
        store_finish.upd(new_tok, False);
        commit_start.upd(new_tok, False);

        emulation.upd(new_tok, False);

        fault_illegal_instr.upd(new_tok, False);
        fault_itrans.upd(new_tok, False);
        fault_itrans2.upd(new_tok, False);
        fault_dtrans.upd(new_tok, False);
        fault_dtrans2.upd(new_tok, False);

        // Update the free pointer.
        let next_token_idx = new_tok + 1;
        next_tok.upd(new_tok, next_token_idx.token_id);
        next_free_tok[ctx_id] <= next_token_idx.token_id;

        return new_tok;

    endmethod

    // iTransStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action iTransStart(TOKEN_INDEX t);

        // We don't need an assert here, because it's okay to begin working on killed tokens.

        itr_start.upd(t, True);
        num_in_itr.up();

    endmethod

    // iTransFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action iTransFinish(TOKEN_INDEX t);

        assert_token_can_finish_itr(itr_start.sub(t));

        itr_finish.upd(t, True);
        num_in_itr.down();

    endmethod

    // fetStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action fetStart(TOKEN_INDEX t);

        assert_token_can_start_fet(itr_finish.sub(t));

        fet_start.upd(t, True);
        num_in_fet.up();

    endmethod

    // fetFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action fetFinish(TOKEN_INDEX t);

        assert_token_can_finish_fet(fet_start.sub(t));

        fet_finish.upd(t, True);
        num_in_fet.down();

    endmethod

    // decStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action decStart(TOKEN_INDEX t);

        let ctx_id = t.context_id;

        assert_token_can_start_dec(fet_finish.sub(t));

        dec_start.upd(t, True);
        num_in_dec.up();

        let youngest_idx = tokenIndexFromIds(ctx_id, youngestDecodedTok[ctx_id]);
        if (tokenIsOlderOrEq(youngest_idx, t) && alloc[pack(t)])
        begin
            youngestDecodedTok[ctx_id] <= t.token_id;
        end

    endmethod

    // decFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action decFinish(TOKEN_INDEX t);

        assert_token_can_finish_dec(dec_start.sub(t));

        dec_finish.upd(t, True);
        num_in_dec.down();

    endmethod

    // exeStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action exeStart(TOKEN_INDEX t);

        assert_token_can_start_exe(dec_finish.sub(t));

        exe_start.upd(t, True);
        num_in_exe.up();

    endmethod

    // exeFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action exeFinish(TOKEN_INDEX t);

        assert_token_can_finish_exe(exe_start.sub(t));

        exe_finish.upd(t, True);
        num_in_exe.down();

    endmethod

    // dTransStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action dTransStart(TOKEN_INDEX t);

        assert_token_can_start_dtr(exe_finish.sub(t));

        dtr_start.upd(t, True);
        num_in_dtr.up();

    endmethod

    // dTransFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action dTransFinish(TOKEN_INDEX t);

        assert_token_can_finish_dtr(dtr_start.sub(t));

        dtr_finish.upd(t, True);
        num_in_dtr.down();

    endmethod

    // loadStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action loadStart(TOKEN_INDEX t);

        assert_token_can_start_load(dtr_finish.sub(t));

        load_start.upd(t, True);
        num_in_load.up();

    endmethod

    // loadFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action loadFinish(TOKEN_INDEX t);

        assert_token_can_finish_load(load_start.sub(t));

        load_finish.upd(t, True);
        num_in_load.down();

    endmethod

    // storeStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action storeStart(TOKEN_INDEX t);

        assert_token_can_start_store(dtr_finish.sub(t));

        store_start.upd(t, True);
        num_in_store.up();

    endmethod

    // storeFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action storeFinish(TOKEN_INDEX t);

        assert_token_can_finish_store(store_start.sub(t));

        store_finish.upd(t, True);
        num_in_store.down();

    endmethod

    // commit_start

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action commitStart(TOKEN_INDEX t);

        if (is_load.sub(t))
            assert_token_has_done_loads(load_finish.sub(t));

        if (is_store.sub(t))
            assert_token_has_done_stores(store_finish.sub(t));

        assert_poison_instr( ! isValid(checkFaults(t)) );

        assert_token_can_start_commit(exe_finish.sub(t));

        commit_start.upd(t, True);
        num_in_commit.up();

    endmethod

    // setFetchOffset

    // When:   Any time.
    // Effect: Record the fetch offset.

    method Action setFetchOffset(TOKEN_INDEX t, MEM_OFFSET offset);
    
        fetch_offset.upd(t, offset);
    
    endmethod

    // setMemOpOffset

    // When:   Any time.
    // Effect: Record the fetch offset.

    method Action setMemOpOffset(TOKEN_INDEX t, MEM_OFFSET offset);
    
        memop_offset.upd(t, offset);
    
    endmethod

    // setLoadType

    // When:   Any time.
    // Effect: Record the store type and mark the token as a store.

    method Action setLoadType(TOKEN_INDEX t, ISA_MEMOP_TYPE mtype);
    
        is_load.upd(t, True);
        
        load_type.upd(t, mtype);
    
    endmethod

    // setStoreType

    // When:   Any time.
    // Effect: Record the store type and mark the token as a store.

    method Action setStoreType(TOKEN_INDEX t, ISA_MEMOP_TYPE mtype);
    
        is_store.upd(t, True);
        
        store_type.upd(t, mtype);
    
    endmethod

    // setEmulation

    // When:   Any time.
    // Effect: Record whether or not the token should be emulated.

    method Action setEmulation(TOKEN_INDEX t, Bool em);
    
        emulation.upd(t, em);
            
    endmethod

    // setFault

    // When:   Any time -- typically during execution
    // Effect: Flag an instruction poisoned

    method Action setFault(TOKEN_INDEX t, FUNCP_FAULT fault_code);
    
        // Only set fault if one hasn't been raised already
        case (fault_code)
            FAULT_ILLEGAL_INSTR:
                fault_illegal_instr.upd(t, True);
            FAULT_ITRANS:
                fault_itrans.upd(t, True);
            FAULT_ITRANS2:
                fault_itrans2.upd(t, True);
            FAULT_DTRANS:
                fault_dtrans.upd(t, True);
            FAULT_DTRANS2:
                fault_dtrans2.upd(t, True);
        endcase
            
    endmethod

    // rewindTo
    
    // When:   Any time.
    // Effect: Undo all allocations younger than parameter t.

    method Action rewindTo(TOKEN_INDEX t);

        // Construct a new vectore of allocation bits.
        Vector#(NUM_TOKENS, Bool) as = newVector();

        for (Integer x = 0; x < valueof(NUM_TOKENS); x = x + 1)
        begin
            as[x] = tokenIsOlderOrEq(fromInteger(x), t) ? alloc[x] : False;
        end

        // next_free_tok does not change because we don't want to reissue those tokens
        // until the next time we wrap around.
      
        // However we can update oldest_tok here. Specifically, if the token you rewound
        // to was already committed, then if it was a legal rewind (checked elsewhere) then
        // after the rewind there will be no tokens in flight. In the case we can jump 
        // oldest_tok up to next_free_tok (so num_in_flight will be zero). Thus we can
        // reclaim tokens slightly more aggressively.
    
        let ctx_id = t.context_id;
        if (!alloc[pack(t)])
        begin
            oldest_tok[ctx_id] <= next_free_tok[ctx_id];
            youngestDecodedTok[ctx_id] <= next_free_tok[ctx_id];
        end
        else
        begin
            next_tok.upd(t, next_free_tok[ctx_id]);
            youngestDecodedTok[ctx_id] <= t.token_id;
        end

        // Update the vector.
        alloc <= as;

    endmethod

    // isAllocated

    // When:   Any time.
    // Effect: Accessor method.

    method Bool isAllocated(TOKEN_INDEX t);

      return alloc[pack(t)];

    endmethod

    // isLoad
    
    // When:   Any time.
    // Effect: Accessor method.

    method Bool isLoad(TOKEN_INDEX t);
    
        return is_load.sub(t);
    
    endmethod

    // isStore
    
    // When:   Any time.
    // Effect: Accessor method.

    method Bool isStore(TOKEN_INDEX t);

        return is_store.sub(t);

    endmethod

    // emulateInstruction
    
    // When:   Any time.
    // Effect: Accessor method.

    method Bool emulateInstruction(TOKEN_INDEX t);

        return emulation.sub(t);

    endmethod

    // getFault
    
    // When:   Any time.
    // Effect: Accessor method.

    method Maybe#(FUNCP_FAULT) getFault(TOKEN_INDEX t);

        return checkFaults(t);

    endmethod

    // getFetchOffset
    
    // When:   Any time.
    // Effect: Accessor method.

    method MEM_OFFSET getFetchOffset(TOKEN_INDEX t);
    
        return fetch_offset.sub(t);
    
    endmethod

    // getMemOpOffset
    
    // When:   Any time.
    // Effect: Accessor method.

    method MEM_OFFSET getMemOpOffset(TOKEN_INDEX t);
    
        return memop_offset.sub(t);
    
    endmethod

    // getLoadType
    
    // When:   Any time.
    // Effect: Accessor method.

    method ISA_MEMOP_TYPE getLoadType(TOKEN_INDEX t);
    
        return load_type.sub(t);
    
    endmethod

    // getStoreType
    
    // When:   Any time.
    // Effect: Accessor method

    method ISA_MEMOP_TYPE getStoreType(TOKEN_INDEX t);
    
        return store_type.sub(t);
    
    endmethod

    // youngest

    // When:   Any time.
    // Effect: Accessor method.

    method TOKEN_INDEX youngest(CONTEXT_ID ctx_id);

        return tokenIndexFromIds(ctx_id, next_free_tok[ctx_id] - 1);

    endmethod

    // oldest

    // When:   Any time.
    // Effect: Accessor method.

    method TOKEN_INDEX oldest(CONTEXT_ID ctx_id);

        return tokenIndexFromIds(ctx_id, oldest_tok[ctx_id]);

    endmethod

    // youngestDecoded

    // When:   Any time.
    // Effect: Accessor method.

    method TOKEN_INDEX youngestDecoded(CONTEXT_ID ctx_id);

        return tokenIndexFromIds(ctx_id, youngestDecodedTok[ctx_id]);

    endmethod

    // youngestDecoded_Safe

    // When:   Any time.
    // Effect: Similar to youngestDecoded but returned value may be no older than t.

    method TOKEN_INDEX youngestDecoded_Safe(TOKEN_INDEX t);

        let ctx_id = t.context_id;
        let youngest_idx = tokenIndexFromIds(ctx_id, youngestDecodedTok[ctx_id]);
        return tokenIsOlderOrEq(t, youngest_idx) ? youngest_idx : t;

    endmethod

    // canEmulate

    // When:   Any time.
    // Effect: Accessor method. Returns true if no instructions are in any pipeline except EXE.

    method Bool canEmulate();

        return num_in_itr.isZero() && num_in_fet.isZero() && num_in_dec.isZero() && num_in_dtr.isZero() && num_in_load.isZero() && num_in_store.isZero() && num_in_commit.isZero();

    endmethod

    // canRewind

    // When:   Any time.
    // Effect: Accessor method. Returns true if no instructions are in any pipeline.

    method Bool canRewind();

        return num_in_itr.isZero() && num_in_fet.isZero() && num_in_dec.isZero() && num_in_exe.isZero() && num_in_dtr.isZero() && num_in_load.isZero() && num_in_store.isZero() && num_in_commit.isZero();

    endmethod

endmodule
