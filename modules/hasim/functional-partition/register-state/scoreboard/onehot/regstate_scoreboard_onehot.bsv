
// regstate_scoreboard_onehot

// A scoreboard to track information of the status of in-flight instructions.

// Note: We use a One-Hot encoding to improve throughput by reducing rule conflicts.

// Note: We allocate only half the tokens at once.
//       This allows the user to check the relative age between two arbitrary tokens.

// Library imports

import Vector::*;
import RegFile::*;

// Project imports

`include "hasim_common.bsh"
`include "soft_connections.bsh"
`include "hasim_isa.bsh"

// RRR includes
`include "asim/rrr/service_ids.bsh"

// Dictionary includes
`include "asim/dict/ASSERTIONS_SCOREBOARD.bsh"

// TOKEN_SCOREBOARD

// Because the whole system is made of reg files of Bools, we use
// this typdef as a convenience.

typedef RegFile#(TOKEN_INDEX, Bool) TOKEN_SCOREBOARD;

// FUCNCP_SCOREBOARD

// The interface to our scoreboard.

interface FUCNCP_SCOREBOARD;

  // Allocate the next available token.
  method ActionValue#(TOKEN_INDEX) allocate();
  // Finish a token and free it for reuse.
  method Action deallocate(TOKEN_INDEX t);
  
  // These methods track the internal status of which macro-operation a token is in.
  method Action fetStart(TOKEN_INDEX t);
  method Action fetFinish(TOKEN_INDEX t);
  method Action decStart(TOKEN_INDEX t);
  method Action decFinish(TOKEN_INDEX t);
  method Action exeStart(TOKEN_INDEX t);
  method Action exeFinish(TOKEN_INDEX t);
  method Action loadStart(TOKEN_INDEX t);
  method Action loadFinish(TOKEN_INDEX t);
  method Action storeStart(TOKEN_INDEX t);
  method Action storeFinish(TOKEN_INDEX t);
  method Action commitStart(TOKEN_INDEX t);
  
  // Set the memory type that we use for accessing memory.
  method Action setLoadType(TOKEN_INDEX t,  ISA_MEMOP_TYPE mt);
  method Action setStoreType(TOKEN_INDEX t, ISA_MEMOP_TYPE mt);
  
  // Set whether or not the instruction should be emulated in software.
  method Action setEmulation(TOKEN_INDEX t, Bool em);
  
  // Rollback the allocations younger than t.
  method Action rewindTo(TOKEN_INDEX t);
  
  // Accessor methods.
  method Bool isAllocated(TOKEN_INDEX t);
  method Bool isLoad(TOKEN_INDEX t);
  method Bool isStore(TOKEN_INDEX t);
  method Bool emulateInstruction(TOKEN_INDEX t);
  method ISA_MEMOP_TYPE getLoadType(TOKEN_INDEX t);
  method ISA_MEMOP_TYPE getStoreType(TOKEN_INDEX t);
  method TOKEN_INDEX youngest();
  method TOKEN_INDEX oldest();
  
endinterface

// mkFUNCP_Scoreboard

module [Connected_Module] mkFUNCP_Scoreboard (FUCNCP_SCOREBOARD)
    provisos 
            (Bits#(TOKEN_INDEX, idx_SZ)); // The size of the token index.

    // ***** Local State ***** //

    // We keep the status bits in a register for fast pull-downs.
    Reg#(Vector#(TExp#(idx_SZ), Bool)) alloc      <- mkReg(replicate(False));

    // The actual scoreboards.
    TOKEN_SCOREBOARD fet_start    <- mkRegFileFull();
    TOKEN_SCOREBOARD fet_finish   <- mkRegFileFull();
    TOKEN_SCOREBOARD dec_start    <- mkRegFileFull();
    TOKEN_SCOREBOARD dec_finish   <- mkRegFileFull();
    TOKEN_SCOREBOARD is_load      <- mkRegFileFull();
    TOKEN_SCOREBOARD is_store     <- mkRegFileFull();
    TOKEN_SCOREBOARD exe_start    <- mkRegFileFull();
    TOKEN_SCOREBOARD exe_finish   <- mkRegFileFull();
    TOKEN_SCOREBOARD load_start   <- mkRegFileFull();
    TOKEN_SCOREBOARD load_finish  <- mkRegFileFull();
    TOKEN_SCOREBOARD store_start  <- mkRegFileFull();
    TOKEN_SCOREBOARD store_finish <- mkRegFileFull();
    TOKEN_SCOREBOARD commit_start <- mkRegFileFull();

    Reg#(Vector#(TExp#(idx_SZ), ISA_MEMOP_TYPE)) load_type  <- mkRegU();
    Reg#(Vector#(TExp#(idx_SZ), ISA_MEMOP_TYPE)) store_type <- mkRegU();
    TOKEN_SCOREBOARD emulation <- mkRegFileFull();

    // A pointer to the next token to be allocated.
    Reg#(TOKEN_INDEX) next_free_tok <- mkReg(0);

    // A pointer to the oldest active token.
    Reg#(TOKEN_INDEX) oldest_tok <- mkReg(0);

    // ***** Assertion Checkers ***** //

    // Do we have enough tokens to do everything the timing model wants us to?
    Assertion assert_enough_tokens <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_OUT_OF_TOKENS, ASSERT_ERROR);

    // Don't allocate a token which is already allocated.
    Assertion assert_token_is_not_allocated <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_REALLOCATE, ASSERT_ERROR);

    // Don't de-allocate a token which isn't allocated.
    // Assertion assert_token_is_allocated <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_DEALLOCATE, ASSERT_ERROR);

    // Are we completing tokens in order?
    // Assertion assert_completing_tokens_in_order <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_COMPLETION, ASSERT_WARNING);

    // The following assertions make sure things happen at the right time.
    Assertion assert_token_can_finish_fet   <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_FETCH_FINISH, ASSERT_ERROR);
    Assertion assert_token_can_start_dec    <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_DECODE_START, ASSERT_ERROR);
    Assertion assert_token_can_finish_dec   <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_DECODE_FINISH, ASSERT_ERROR);
    Assertion assert_token_can_start_exe    <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_EXECUTE_START, ASSERT_ERROR);
    Assertion assert_token_can_finish_exe   <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_EXECUTE_FINISH, ASSERT_ERROR);
    Assertion assert_token_can_start_load   <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_LOAD_START, ASSERT_ERROR);
    Assertion assert_token_can_finish_load  <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_LOAD_FINISH, ASSERT_ERROR);
    Assertion assert_token_can_start_store  <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_STORE_START, ASSERT_ERROR);
    Assertion assert_token_can_finish_store <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_STORE_FINISH, ASSERT_ERROR);
    Assertion assert_token_can_start_commit <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_COMMIT_START, ASSERT_ERROR);
    Assertion assert_token_has_done_loads   <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_COMMIT_WITHOUT_LOAD, ASSERT_ERROR);
    Assertion assert_token_has_done_stores  <- mkAssertionChecker(`ASSERTIONS_SCOREBOARD_ILLEGAL_COMMIT_WITHOUT_STORE, ASSERT_ERROR);

    // ***** Helper Functions ***** //

    // The youngest token is the last one allocated.
    TOKEN_INDEX youngest_tok = next_free_tok - 1;

    // The number of in-flight tokens.
    TOKEN_INDEX num_in_flight =  next_free_tok - oldest_tok;

    // Note: we allocate only half the tokens at once. See above.
    // We can allocate only if the MSB is zero.
    Bool can_allocate = num_in_flight[valueOf(idx_SZ) - 1] == 0;

    // isBusy

    // A token is said to be "busy" if it has started a macro-operation but not finished it yet.
    
    function Bool isBusy(TOKEN_INDEX t);

        // Has this token started a macro operation but not finished it?
        let fet_busy =       fet_start.sub(t) && !fet_finish.sub(t);
        let dec_busy =       dec_start.sub(t) && !dec_finish.sub(t);
        let exe_busy =       exe_start.sub(t) && !exe_finish.sub(t);
        let load_busy =     load_start.sub(t) && !load_finish.sub(t);
        let store_busy =   store_start.sub(t) && !store_finish.sub(t);
        let commit_busy = commit_start.sub(t) && alloc[t]; // It's not done committing if it's still allocated.

        // If it is in any macro operation it is busy.
        return fet_busy || dec_busy || exe_busy || load_busy || store_busy || commit_busy;

    endfunction

    // deallocate

    // When:   Any time.
    // Effect: Reset the allocation bit. Update the oldest-token pointer.

    method Action deallocate(TOKEN_INDEX t);

        // Assert that the token is actually allocated.
        //assert_token_is_allocated(alloc[t]);

        // Assert that the token is the next to be freed.
        //assert_completing_tokens_in_order(t == oldest_tok);

        // Update the oldest token.
        oldest_tok <= t + 1;

        // Update the allocation table.
        alloc <=  update(alloc, t, False);

    endmethod

    // allocate

    // When:   When the next token to be allocated is not "busy"
    //         IE it's not in an indeterminate state. 
    //         As long as every macro-operation eventually completes forward progress will be made.
    // Effect: Allocate a token and reset the entire set of scoreboard states.

    method ActionValue#(TOKEN_INDEX) allocate() if (!isBusy(next_free_tok));

        // Assert the the token wasn't already allocated.
        assert_token_is_not_allocated(!alloc[next_free_tok]);

        // Assert that we haven't ran out of tokens.
        assert_enough_tokens(can_allocate);

        // Update the allocation status.    
        alloc <= update(alloc, next_free_tok, True);

        // Reset all the scoreboards.
        fet_start.upd(next_free_tok, False);
        fet_finish.upd(next_free_tok, False);
        dec_start.upd(next_free_tok, False);
        dec_finish.upd(next_free_tok, False);
        is_load.upd(next_free_tok, False);
        is_store.upd(next_free_tok, False);
        exe_start.upd(next_free_tok, False);
        exe_finish.upd(next_free_tok, False);
        load_start.upd(next_free_tok, False);
        load_finish.upd(next_free_tok, False);
        store_start.upd(next_free_tok, False);
        store_finish.upd(next_free_tok, False);
        commit_start.upd(next_free_tok, False);

        emulation.upd(next_free_tok, False);

        // Update the free pointer.
        next_free_tok <= next_free_tok + 1;

        return next_free_tok;

    endmethod

    // fetStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action fetStart(TOKEN_INDEX t);

        // We don't need an assert here, because it's okay to fetch killed tokens.

        fet_start.upd(t, True);

    endmethod

    // fetFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action fetFinish(TOKEN_INDEX t);

        assert_token_can_finish_fet(fet_start.sub(t));

        fet_finish.upd(t, True);

    endmethod

    // decStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action decStart(TOKEN_INDEX t);

        assert_token_can_start_dec(fet_finish.sub(t));

        dec_start.upd(t, True);

    endmethod

    // decFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action decFinish(TOKEN_INDEX t);

        assert_token_can_finish_dec(dec_start.sub(t));

        dec_finish.upd(t, True);

    endmethod

    // exeStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action exeStart(TOKEN_INDEX t);

        assert_token_can_start_exe(dec_finish.sub(t));

        exe_start.upd(t, True);

    endmethod

    // exeFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action exeFinish(TOKEN_INDEX t);

        assert_token_can_finish_exe(exe_start.sub(t));

        exe_finish.upd(t, True);

    endmethod

    // loadStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action loadStart(TOKEN_INDEX t);

        assert_token_can_start_load(exe_finish.sub(t));

        load_start.upd(t, True);

    endmethod

    // loadFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action loadFinish(TOKEN_INDEX t);

        assert_token_can_finish_load(load_start.sub(t));

        load_finish.upd(t, True);

    endmethod

    // storeStart

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action storeStart(TOKEN_INDEX t);

        assert_token_can_start_store(exe_finish.sub(t));

        store_start.upd(t, True);

    endmethod

    // storeFinish

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action storeFinish(TOKEN_INDEX t);

        assert_token_can_finish_store(store_start.sub(t));

        store_finish.upd(t, True);

    endmethod

    // commit_start

    // When:   Any time.
    // Effect: Update the scoreboard.

    method Action commitStart(TOKEN_INDEX t);

        if (is_load.sub(t))
            assert_token_has_done_loads(load_finish.sub(t));

        if (is_store.sub(t))
            assert_token_has_done_stores(store_finish.sub(t));

        assert_token_can_start_commit(exe_finish.sub(t));

        commit_start.upd(t, True);

    endmethod

    // setLoadType

    // When:   Any time.
    // Effect: Record the store type and mark the token as a store.

    method Action setLoadType(TOKEN_INDEX t, ISA_MEMOP_TYPE mtype);
    
        is_load.upd(t, True);
        
        load_type[t] <= mtype;
    
    endmethod

    // setStoreType

    // When:   Any time.
    // Effect: Record the store type and mark the token as a store.

    method Action setStoreType(TOKEN_INDEX t, ISA_MEMOP_TYPE mtype);
    
        is_store.upd(t, True);
        
        store_type[t] <= mtype;
    
    endmethod

    // setEmulation

    // When:   Any time.
    // Effect: Record whether or not the token should be emulated.

    method Action setEmulation(TOKEN_INDEX t, Bool em);
    
        emulation.upd(t, em);
            
    endmethod

    // rewindTo
    
    // When:   Any time.
    // Effect: Undo all allocations younger than parameter t.

    method Action rewindTo(TOKEN_INDEX t);

      // Construct a new vectore of allocation bits.
      Vector#(TExp#(idx_SZ), Bool) as = newVector();

      for (Integer x = 0; x < valueof(TExp#(idx_SZ)); x = x + 1)
      begin
        TOKEN_INDEX cur = fromInteger(x);
        as[x] = (youngest_tok > t) ?
                   // No overflow case.
                   ((cur > t) && (cur <= youngest_tok) ? False : alloc[x]) :
                   // Overflow case.
                   ((cur > t) || (cur <= youngest_tok) ? False : alloc[x]);
      end

      // Update the vector.
      alloc <= as;

    endmethod

    // isAllocated

    // When:   Any time.
    // Effect: Accessor method.

    method Bool isAllocated(TOKEN_INDEX t);

      return alloc[t];

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

    // getLoadType
    
    // When:   Any time.
    // Effect: Accessor method.

    method ISA_MEMOP_TYPE getLoadType(TOKEN_INDEX t);
    
        return load_type[t];
    
    endmethod

    // getStoreType
    
    // When:   Any time.
    // Effect: Accessor method

    method ISA_MEMOP_TYPE getStoreType(TOKEN_INDEX t);
    
        return store_type[t];
    
    endmethod

    // youngest

    // When:   Any time.
    // Effect: Accessor method.

    method TOKEN_INDEX youngest();

        return youngest_tok;

    endmethod

    // oldest

    // When:   Any time.
    // Effect: Accessor method.

    method TOKEN_INDEX oldest();

        return oldest_tok;

    endmethod

endmodule
