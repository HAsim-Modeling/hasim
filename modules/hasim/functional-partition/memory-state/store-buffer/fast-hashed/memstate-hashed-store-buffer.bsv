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

// Library imports.

import FIFO::*;
import FIFOF::*;
import Vector::*;


`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_common.bsh"
`include "asim/provides/funcp_memory.bsh"
`include "asim/provides/soft_connections.bsh"

`include "asim/dict/ASSERTIONS_FUNCP_MEMSTATE_SBUFFER.bsh"


// ===================================================================
//
// PUBLIC DATA STRUCTURES
//
// ===================================================================


//
// Response types
//

typedef struct
{
    MEM_ADDRESS addr;
    Maybe#(MEM_VALUE) mvalue;
}
MEMSTATE_SBUFFER_RSP_LOOKUP
    deriving (Eq, Bits);

typedef struct
{
    Bool hasMore;
    MEM_ADDRESS addr;
    MEM_VALUE value;
}
MEMSTATE_SBUFFER_RSP_COMMIT
    deriving (Eq, Bits);


interface MEMSTATE_SBUFFER;

    //
    // LOAD:  Look up and return a value from the store buffer.
    //
    method Action lookupReq(TOKEN_INDEX tokIdx, MEM_ADDRESS addr);
    method ActionValue#(MEMSTATE_SBUFFER_RSP_LOOKUP) lookupResp();
    
    //
    // STORE:  Add a value to the store buffer.
    //
    method Action insertReq(TOKEN_INDEX tokIdx, MEM_ADDRESS addr, MEM_VALUE value);
    
    //
    // COMMIT:  Remove an entry from the store buffer, returning the value.
    //          The caller will forward the value down the memory hierarchy.
    //
    method Action commitReq(TOKEN_INDEX tokIdx);
    method ActionValue#(MEMSTATE_SBUFFER_RSP_COMMIT) commitResp();

    //
    // REWIND
    //
    method Action rewindReq(TOKEN_INDEX rewind_to, TOKEN_INDEX rewind_from);

endinterface: MEMSTATE_SBUFFER


// ===================================================================
//
// PRIVATE DATA STRUCTURES
//
// ===================================================================

// Store buffer index (pointer) into the store buffer.
typedef Bit#(`MEMSTATE_SBUFFER_INDEX_BITS) MEMSTATE_SBUFFER_INDEX;

//
// Each token may have more than one store associated with it.  For
// example, unaligned stores may trigger two aligned stores.  The type declaration
// of MEMSTATE_SBUFFER_TOK_STORE_CNT adds one when setting its size since it is
// a counter, not an index.
//
`define MEMSTATE_SBUFFER_STORES_PER_TOKEN 2
typedef Bit#(TLog#(TAdd#(`MEMSTATE_SBUFFER_STORES_PER_TOKEN, 1))) MEMSTATE_SBUFFER_TOK_STORE_CNT;

typedef struct
{
    MEMSTATE_SBUFFER_TOK_STORE_CNT nStores;
    Vector#(`MEMSTATE_SBUFFER_STORES_PER_TOKEN, MEMSTATE_SBUFFER_INDEX) storeNodePtr;
}
MEMSTATE_SBUFFER_TOKEN
    deriving (Eq, Bits);

//
// Primary store buffer data structure.  Entries are on linked lists where
// every entry on the list shares an address hash value.
//
typedef struct
{
    Maybe#(MEMSTATE_SBUFFER_INDEX) prev;
    Maybe#(MEMSTATE_SBUFFER_INDEX) next;

    TOKEN_INDEX tokIdx;
    MEM_ADDRESS addr;
    MEM_VALUE value;
}
MEMSTATE_SBUFFER_NODE
    deriving (Eq, Bits);


//
// Addresses are hashed into buckets
//
typedef Bit#(`MEMSTATE_SBUFFER_ADDR_HASH_BITS) MEMSTATE_SBUFFER_ADDR_HASH_IDX;


//
// States in the store buffer FSM.
//
typedef enum
{
    SBUFFER_STATE_INIT,             // Initialization
    SBUFFER_STATE_READY,            // Ready for command
    SBUFFER_STATE_ALLOC,            // Token allocation
    SBUFFER_STATE_LOOKUP,           // Lookup (load) result ready
    SBUFFER_STATE_LOOKUP_SEARCH,    // Searching for address in buffer
    SBUFFER_STATE_INSERT,           // Add new entry to buffer
    SBUFFER_STATE_COMMIT,           // Commit store (return result & remove entry)
    SBUFFER_STATE_REWIND_TOKENS,    // Stepping through token range & remove from buffer
    SBUFFER_STATE_REWIND,           // Removing stores for a single token
    SBUFFER_STATE_REMOVE_UPD_PREV,  // Removing token: update prev in addr hash list
    SBUFFER_STATE_REMOVE_UPD_NEXT   // Removing token: update next in addr hash list
}
SBUFFER_STATE
    deriving (Bits, Eq);


module [HASIM_MODULE] mkFUNCP_StoreBuffer#(DEBUG_FILE debugLog)
    // interface:
        (MEMSTATE_SBUFFER);

    // ***** Soft Connections *****

    Connection_Receive#(TOKEN_INDEX) allocate <- mkConnection_Receive("storeBufferAllocate");


    // ***** Assertion Checkers *****

    ASSERTION_NODE assertNode        <- mkAssertionNode(`ASSERTIONS_FUNCP_MEMSTATE_SBUFFER__BASE);
    ASSERTION assertNotBusy          <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_SBUFFER_BUSY_TOKEN, ASSERT_ERROR, assertNode);
    ASSERTION assertTooManyStores    <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_SBUFFER_TOO_MANY_TOKEN_STORES, ASSERT_ERROR, assertNode);
    ASSERTION assertFreeListNotEmpty <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_SBUFFER_FREELIST_EMPTY, ASSERT_ERROR, assertNode);
    ASSERTION assertNoStores         <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_SBUFFER_NO_STORES, ASSERT_ERROR, assertNode);

    // ***** Internal state *****

    // Store-buffer data for a token
    let tokInit = MEMSTATE_SBUFFER_TOKEN { nStores: 0, storeNodePtr: ? };
    BRAM#(TOKEN_INDEX, MEMSTATE_SBUFFER_TOKEN) tokData <- mkLiveTokenBRAMInitialized(tokInit);

    // Storage for store buffer entries
    BRAM#(MEMSTATE_SBUFFER_INDEX, MEMSTATE_SBUFFER_NODE) sBuffer <- mkBRAM();

    // Map from address to stores using a hash table.  This is in LUTs instead
    // of BRAM because it can be small enough that it is a waste of BRAM.  The
    // lookup path is also a cycle shorter with the address hash head in LUTs.
    LUTRAM#(MEMSTATE_SBUFFER_ADDR_HASH_IDX, Maybe#(MEMSTATE_SBUFFER_INDEX)) addrHash <- mkLUTRAM(tagged Invalid);

    // Store buffer node free list head
    Reg#(MEMSTATE_SBUFFER_INDEX) freeListHead <- mkReg(0);

    // Current FSM state
    Reg#(SBUFFER_STATE) state <- mkReg(SBUFFER_STATE_INIT);
    
    // Store insert pipeline
    FIFO#(Tuple2#(MEMSTATE_SBUFFER_INDEX, MEMSTATE_SBUFFER_INDEX)) insertPrev <- mkFIFO();

    // Number of stores in the store buffer.  There can be at most two stores
    // for every token.  Since the number of tokens in flight can be at most
    // half the number of token indices available, a counter with the same number
    // of bits as the token index is large enough for all possible stores in
    // flight.
    COUNTER#(TOKEN_INDEX_SIZE) nStoresInBuffer <- mkLCounter(0);

    //
    // ***** Remove token state *****
    //
    
    // Youngest store hint is used to help rewind find a starting point for
    // looping through all tokens.  Often the youngest tokens are not stores and
    // can be skipped.  If the hint is valid then it correctly points to the
    // youngest store.  If the hint is invalid there may still be a store in
    // the buffer and a full token search is required.
    Reg#(Maybe#(TOKEN_INDEX)) youngestStoreHint <- mkReg(tagged Invalid);

    // Index of store within a token.  Use in remove_token_stores rule.
    Reg#(MEMSTATE_SBUFFER_TOK_STORE_CNT) removeTokenStoreIdx <- mkRegU();

    // Remove token pipeline
    //    next state
    FIFO#(SBUFFER_STATE) removeToken <- mkFIFO();

    // Remove store pipeline
    //             next state    store buffer idx of ref
    FIFO#(Tuple2#(SBUFFER_STATE, MEMSTATE_SBUFFER_INDEX)) removeStore <- mkFIFO1();

    // Hash chain update pipeline during token removal
    FIFO#(Tuple3#(SBUFFER_STATE, MEMSTATE_SBUFFER_INDEX, Maybe#(MEMSTATE_SBUFFER_INDEX))) removeUpdateHashChain <- mkFIFO();


    // Commit response pipeline
    FIFOF#(MEMSTATE_SBUFFER_RSP_COMMIT) commitRespPipe <- mkFIFOF();

    // Current request details
    Reg#(TOKEN_INDEX) reqToken <- mkRegU();
    Reg#(MEM_ADDRESS) reqAddr  <- mkRegU();
    Reg#(MEM_VALUE)   reqValue <- mkRegU();
    Reg#(MEMSTATE_SBUFFER_ADDR_HASH_IDX) reqAddrHash <- mkRegU();

    // Response details
    Reg#(Maybe#(MEM_VALUE)) respValue <- mkRegU();
    Reg#(TOKEN_INDEX)       respClosestReqToken <- mkRegU();

    // Rewind request state
    Reg#(TOKEN_INDEX) rewindTo  <- mkRegU();
    Reg#(TOKEN_INDEX) rewindCur <- mkRegU();

    // Index used to initialize the SB node free list
    Reg#(MEMSTATE_SBUFFER_INDEX) initIdx <- mkReg(0);

    //
    // Map address to store buffer address hash
    //
    function MEMSTATE_SBUFFER_ADDR_HASH_IDX sbAddrHash(MEM_ADDRESS addr);
        return truncate(hashTo8(addr));
    endfunction


    //
    // init --
    //     Initialize the free list.
    //
    rule init (state == SBUFFER_STATE_INIT);
        if (initIdx != maxBound)
        begin
            // next points to idx + 1
            sBuffer.write(initIdx,
                          MEMSTATE_SBUFFER_NODE { prev: tagged Invalid, next: tagged Valid (initIdx + 1), tokIdx: ?, addr: ?, value: ? });
        end
        else
        begin
            // Last entry on free list (next is Invalid)
            sBuffer.write(initIdx,
                          MEMSTATE_SBUFFER_NODE { prev: tagged Invalid, next: tagged Invalid, tokIdx: ?, addr: ?, value: ? });
            state <= SBUFFER_STATE_READY;
        end


        initIdx <= initIdx + 1;
    endrule


    //
    // alloc --
    //     Inialization for a new token.
    //
    rule alloc (state == SBUFFER_STATE_READY);
        let tok_idx = allocate.receive();
        allocate.deq();

        debugLog.record($format("SB ALLOC tok=%0d", tok_idx));

        // We could do the allocation in a single cycle if we skipped the check
        // that the last instance of the token was cleaned up.  Instead, we
        // read the current state and confirm that it holds no stores.
        tokData.readReq(tok_idx);

        state <= SBUFFER_STATE_ALLOC;
    endrule

    rule alloc_done (state == SBUFFER_STATE_ALLOC);
        let sb_tok <- tokData.readRsp();

        // Make sure the last instance of the token ID was cleaned up properly.
        // If it was there is no more work to do.
        assertNotBusy(sb_tok.nStores == 0);

        state <= SBUFFER_STATE_READY;
    endrule


    //
    // lookup_search_node --
    //     Iterate over nodes in the store buffer, looking for a match to
    //     the request.  On match return the value.  On mismatch loop by
    //     requesting the next node in the hash chain.
    //
    rule lookup_search_node (state == SBUFFER_STATE_LOOKUP_SEARCH);
        let node <- sBuffer.readRsp();
        
        if (node.addr == reqAddr)
        begin
            //
            // Two search modes are implemented here.  When MEMSTATE_SBUFFER_OOO_MEM
            // is not zero the full address hash chain is always searched and
            // the match closest in age is returned.  In this case the store buffer
            // assists out-of-order execution by matching memory state.
            // This mode would hide reordering bugs on microarchitectures that
            // are supposed to handle memory aliasing by keeping references to
            // the same address in order.
            //
            // To support ordered implementations, when MEMSTATE_SBUFFER_OOO_MEM is
            // 0 the first match in the address chain is returned and the token's
            // age is ignored.  Ideally we would raise on assertion when the
            // returned token appears to be the wrong one, but we can't since the
            // path may be speculative.
            //
            if (`MEMSTATE_SBUFFER_OOO_MEM == 0)
            begin
                // Ordered memory mode.  Take first hit, ignore age.
                debugLog.record($format("    SB Addr Match: tok=%0d, node_token=%0d, node_addr=0x%x, value=0x%x, ending search", reqToken, node.tokIdx, node.addr, node.value));

                respValue <= tagged Valid node.value;
                state <= SBUFFER_STATE_LOOKUP;
            end
            else
            begin
                // Out-of-order mode
                debugLog.record($format("    SB Addr Match: tok=%0d, node_token=%0d, node_addr=0x%x, value=0x%x", reqToken, node.tokIdx, node.addr, node.value));

                // Use this hit as current best match if the store is before
                // the load and either this is the first address match or this
                // store was executed later than the previous match.
                if (tokenIsOlderOrEq(node.tokIdx, reqToken) &&
                    (tokenIsOlderOrEq(respClosestReqToken, node.tokIdx) || ! isValid(respValue)))
                begin
                    debugLog.record($format("      SB Current Best: tok=%0d, node_token=%0d", reqToken, node.tokIdx));

                    respValue <= tagged Valid node.value;
                    respClosestReqToken <= node.tokIdx;
                end

                // Is there more to search?
                if (node.next matches tagged Valid .n_idx)
                begin
                    // Yes -- search next entry
                    sBuffer.readReq(n_idx);
                end
                else
                begin
                    // No -- done
                    state <= SBUFFER_STATE_LOOKUP;
                end
            end
        end
        else
        begin
            //
            // Mismatch.  Is there another entry in the chain?
            //
            if (node.next matches tagged Valid .n_idx)
            begin
                // Yes -- search next entry
                debugLog.record($format("    SB mismatch: tok=%0d, node_token=%0d, node_addr=0x%x, node_next=%0d", reqToken, node.tokIdx, node.addr, n_idx));

                sBuffer.readReq(n_idx);
            end
            else
            begin
                // No -- give up
                debugLog.record($format("    SB mismatch: tok=%0d, node_token=%0d, node_addr=0x%x, end of chain", reqToken, node.tokIdx, node.addr));

                state <= SBUFFER_STATE_LOOKUP;
            end
        end
    endrule

    
    //
    // insert_store --
    //     Add a store to the store buffer.
    //
    rule insert_store (state == SBUFFER_STATE_INSERT);
        // Receive the old node and address hash head values
        let old_node <- sBuffer.readRsp();
        let old_tok_data <- tokData.readRsp();

        let old_addr_hash_head = addrHash.sub(reqAddrHash);

        // Update youngest store hint.  The hint may not be set if there were
        // already stores in the buffer and the current hint is invalid.  In
        // that case we have no way of knowing the age of the new store relative
        // to others in the buffer.
        if (nStoresInBuffer.value() == 0)
        begin
            youngestStoreHint <= tagged Valid reqToken;
            debugLog.record($format("    SB Store tok=%0d is youngest", reqToken));
        end
        else if (youngestStoreHint matches tagged Valid .cur_youngest &&&
                 tokenIsOlderOrEq(cur_youngest, reqToken))
        begin
            youngestStoreHint <= tagged Valid reqToken;
            debugLog.record($format("    SB Store tok=%0d is youngest (replaces %0d)", reqToken, cur_youngest));
        end

        // New node is taken from head of free list
        let node_idx = freeListHead;
        nStoresInBuffer.up();
        
        //
        // Update node with new store details
        //
        MEMSTATE_SBUFFER_NODE new_node;
        new_node.prev = tagged Invalid;
        new_node.next = old_addr_hash_head;
        new_node.tokIdx = reqToken;
        new_node.addr = reqAddr;
        new_node.value = reqValue;
        sBuffer.write(node_idx, new_node);

        //
        // Update store info for the token.  Tokens may have more than one store.
        //
        debugLog.record($format("    SB Store #%0d for tok=%0d, node_idx=%0d", old_tok_data.nStores, reqToken, node_idx));
        assertTooManyStores(old_tok_data.nStores != `MEMSTATE_SBUFFER_STORES_PER_TOKEN);
        let tok_data = old_tok_data;
        // Pointer to new node
        tok_data.storeNodePtr[tok_data.nStores] = node_idx;
        tok_data.nStores = tok_data.nStores + 1;
        tokData.write(reqToken, tok_data);

        //
        // Update the free list head.  It was stored in the node's next pointer.
        //
        if (old_node.next matches tagged Valid .old_next)
        begin
            freeListHead <= old_next;
        end
        else
        begin
            debugLog.record($format("  <<< Free list is empty! Aborting. >>>"));
            assertFreeListNotEmpty(False);
        end
        
        //
        // Update address hash list, putting new node at the head
        //
        addrHash.upd(reqAddrHash, tagged Valid node_idx);

        if (old_addr_hash_head matches tagged Valid .hash_next)
        begin
            // Need another cycle to set prev pointer in existing hash chain
            sBuffer.readReq(hash_next);
            insertPrev.enq(tuple2(node_idx, hash_next));

            debugLog.record($format("    SB Pushing address hash=%0d, node=%0d, next=%0d", reqAddrHash, node_idx, hash_next));
        end
        else
        begin
            // Done
            state <= SBUFFER_STATE_READY;
        end
    endrule

    //
    // insert_store_prev --
    //     Final step of adding a store:  set the prev pointer in the address
    //     hash chain from the old head of the chain to the node just added.
    //
    rule insert_store_prev (True);
        // Receive the old node and address hash head values
        let next_node <- sBuffer.readRsp();
        match {.new_node_idx, .next_node_idx} = insertPrev.first();
        insertPrev.deq();
        
        next_node.prev = tagged Valid new_node_idx;
        sBuffer.write(next_node_idx, next_node);

        debugLog.record($format("    SB Set prev pointer for node=%0d, prev=%0d", next_node_idx, new_node_idx));

        state <= SBUFFER_STATE_READY;
    endrule


    //
    // remove_token_store --
    //     Remove all stores associated with a token from the store buffer.
    //     Removal is due either to commit or rewind.  The rule keeps
    //     processing the token until all stores are handled.
    //
    rule remove_token_stores ((state == SBUFFER_STATE_COMMIT) || (state == SBUFFER_STATE_REWIND));
        let tok_data <- tokData.readRsp();
        let next_state = removeToken.first();

        // Token must have stores to commit!
        if (state == SBUFFER_STATE_COMMIT)
        begin
            assertNoStores(tok_data.nStores != 0);
        end

        if (tok_data.nStores != 0)
        begin
            debugLog.record($format("  SB remove: tok=%0d, store id=%0d", reqToken, removeTokenStoreIdx));

            // Request the data for the store
            let node_idx = tok_data.storeNodePtr[removeTokenStoreIdx];
            sBuffer.readReq(node_idx);

            if (removeTokenStoreIdx + 1 == tok_data.nStores)
            begin
                // Done with all stores for the token.  Clear it.
                let tokInit = MEMSTATE_SBUFFER_TOKEN { nStores: 0, storeNodePtr: ? };
                tokData.write(reqToken, tokInit);

                // Done processing this token
                removeToken.deq();
            end
            else
            begin
                // More stores for this token.  Another read request will cause
                // this rule to run again.
                tokData.readReq(reqToken);
                removeTokenStoreIdx <= removeTokenStoreIdx + 1;

                // Iterate in this state while there are more stores for this
                // token.
                next_state = state;
            end
            
            // Pass control details to remove_one_store rule.
            removeStore.enq(tuple2(next_state, node_idx));

            // Update global state
            nStoresInBuffer.down();
            if (youngestStoreHint matches tagged Valid .cur_youngest &&&
                cur_youngest == reqToken)
            begin
                youngestStoreHint <= tagged Invalid;
            end
        end
        else
        begin
            //
            // Doing rewind and token has no stores.  Nothing to do.
            //
            debugLog.record($format("  SB remove: no stores, tok=%0d", reqToken));
            removeToken.deq();
            state <= next_state;
        end
    endrule


    //
    // remove_one_store --
    //     Fed by remove_token_stores.  Removes a store buffer node from
    //     its address hash chain.  For commits, also forwards store data to
    //     commit response method.  The commit logic can then start writing
    //     the value to memory while code here mucks with linked lists.
    //
    rule remove_one_store ((state == SBUFFER_STATE_COMMIT) || (state == SBUFFER_STATE_REWIND));
        let node <- sBuffer.readRsp();
        match { .next_state, .node_idx } = removeStore.first();
        removeStore.deq();

        debugLog.record($format("    SB remove addr=0x%x, value=0x%x, next_state=%0d", node.addr, node.value, next_state));

        // Respond with store info for commits
        if (state == SBUFFER_STATE_COMMIT)
            commitRespPipe.enq(MEMSTATE_SBUFFER_RSP_COMMIT { hasMore: (next_state == SBUFFER_STATE_COMMIT), addr: node.addr, value: node.value });
        
        //
        // Put the node back on the free list
        //
        sBuffer.write(node_idx, MEMSTATE_SBUFFER_NODE { prev: tagged Invalid, next: tagged Valid freeListHead, tokIdx: ?, addr: ?, value: ? });
        freeListHead <= node_idx;

        //
        // Now the fun of dropping a node from the address hash linked list...
        //
        
        // Prev pointer valid?
        if (node.prev matches tagged Valid .prev_node_idx)
        begin
            // Yes:  request previous node so its next pointer can be updated
            removeUpdateHashChain.enq(tuple3(next_state, prev_node_idx, node.next));
            sBuffer.readReq(prev_node_idx);
            state <= SBUFFER_STATE_REMOVE_UPD_PREV;
        end
        else
        begin
            // No:  head of list.  Write sbAddrHash directly.
            let addr_hash = sbAddrHash(node.addr);
            addrHash.upd(addr_hash, node.next);

            // Next pointer valid?
            if (node.next matches tagged Valid .next_node_idx)
            begin
                // Yes:  request next node so its prev pointer can be updated
                removeUpdateHashChain.enq(tuple3(next_state, next_node_idx, tagged Invalid));
                sBuffer.readReq(next_node_idx);
                debugLog.record($format("      SB new addr_hash head: hash=%0d, next=%0d", addr_hash, next_node_idx));
                state <= SBUFFER_STATE_REMOVE_UPD_NEXT;
            end
            else
            begin
                // No:  nothing to do.
                debugLog.record($format("      SB addr_hash now empty: hash=%0d", addr_hash));
                state <= next_state;
            end
        end
    endrule

    //
    // remove_store_udpate_prev --
    //     Updates the node before the node being removed from an address
    //     hash chain.
    //
    rule remove_store_update_prev (state == SBUFFER_STATE_REMOVE_UPD_PREV);
        let prev_node <- sBuffer.readRsp();
        match { .next_state, .prev_node_idx, .next_node } = removeUpdateHashChain.first();
        removeUpdateHashChain.deq();
        
        let old_next_idx = validValue(prev_node.next);

        // Update the pointer
        prev_node.next = next_node;
        sBuffer.write(prev_node_idx, prev_node);
        
        // Does the new next node need its prev pointer fixed?
        if (next_node matches tagged Valid .next_node_idx)
        begin
            // Yes:  prepare for remove_store_update_next rule
            debugLog.record($format("      SB update prev: node=%0d, old_next=%0d, new_next=%0d", prev_node_idx, old_next_idx, next_node_idx));

            removeUpdateHashChain.enq(tuple3(next_state, next_node_idx, tagged Valid prev_node_idx));
            sBuffer.readReq(next_node_idx);
            state <= SBUFFER_STATE_REMOVE_UPD_NEXT;
        end
        else
        begin
            // No:  done
            debugLog.record($format("      SB update prev: node=%0d, old_next=%0d, new_next=NULL", prev_node_idx, old_next_idx));

            state <= next_state;
        end
    endrule


    //
    // remove_store_update_next --
    //     Final stage of address hash chain update.  Fix prev pointer of next node.
    //
    rule remove_store_update_next (state == SBUFFER_STATE_REMOVE_UPD_NEXT);
        let next_node <- sBuffer.readRsp();
        match { .next_state, .next_node_idx, .prev_node } = removeUpdateHashChain.first();
        removeUpdateHashChain.deq();
        
        let old_prev_idx = validValue(next_node.prev);

        next_node.prev = prev_node;
        sBuffer.write(next_node_idx, next_node);

        if (prev_node matches tagged Valid .prev_node_idx)
            debugLog.record($format("      SB update next: node=%0d, old_prev=%0d, new_prev=%0d", next_node_idx, old_prev_idx, prev_node_idx));
        else
            debugLog.record($format("      SB update next: node=%0d, old_prev=%0d, new_prev=NULL", next_node_idx, old_prev_idx));

        state <= next_state;
    endrule


    //
    // rewind --
    //     Remove all tokens from rewindCur down to rewindTo.  Rewind of a single
    //     token is almost identical in behavior to commit.  The only difference
    //     is commit streams out the store values as it removes entries from
    //     the store buffer.  Rewind shares the commit rules (remove_....)
    //
    rule rewind (state == SBUFFER_STATE_REWIND_TOKENS);
        // Read token's store pointers.  Will be consumed by remove_token_stores.
        tokData.readReq(rewindCur);

        // After token is removed state should return to REWIND for processing
        // the next token until the rewind point is reached.
        let next_rewind = rewindCur - 1;
        removeToken.enq(next_rewind != rewindTo ? SBUFFER_STATE_REWIND_TOKENS : SBUFFER_STATE_READY);

        reqToken <= rewindCur;
        removeTokenStoreIdx <= 0;

        rewindCur <= next_rewind;
        state <= SBUFFER_STATE_REWIND;
    endrule


    //
    // ***** Public methods *****
    //

    //
    // LOAD:  Look up and return a value from the store buffer.
    //     Load is only allowed to fire when the commit response
    //     pipe is empty.  This avoids having to manage an explicit
    //     lock for loads vs. commits in the memory state manager.
    //
    method Action lookupReq(TOKEN_INDEX tokIdx, MEM_ADDRESS addr) if ((state == SBUFFER_STATE_READY) && ! commitRespPipe.notEmpty());
        // Read the head of the address hash chain and start walking it
        let hash = sbAddrHash(addr);
        if (addrHash.sub(hash) matches tagged Valid .node_idx)
        begin
            debugLog.record($format("  SB Lookup: tok=%0d, addr=0x%x, hash=%0d, head node=%0d", tokIdx, addr, hash, node_idx));

            // Read the first node in the hash chain
            sBuffer.readReq(node_idx);
            state <= SBUFFER_STATE_LOOKUP_SEARCH;
        end
        else
        begin
            debugLog.record($format("  SB Lookup Miss (empty hash head): tok=%0d, addr=0x%x, hash=%0d", tokIdx, addr, hash));
            
            // Respond with miss
            state <= SBUFFER_STATE_LOOKUP;
        end

        // Initialize global request state
        reqAddr <= addr;
        reqToken <= tokIdx;
        reqAddrHash <= hash;
    
        // Initialize response
        respValue <= tagged Invalid;
    endmethod: lookupReq

    method ActionValue#(MEMSTATE_SBUFFER_RSP_LOOKUP) lookupResp() if (state == SBUFFER_STATE_LOOKUP);
        state <= SBUFFER_STATE_READY;

        return MEMSTATE_SBUFFER_RSP_LOOKUP { addr: reqAddr, mvalue: respValue };
    endmethod: lookupResp

    
    //
    // STORE:  Add a value to the store buffer.
    //     Store is only allowed to fire when the allocate pipe is empty.  This
    //     prevents a false positive of allocate detecting a token with stores
    //     when the token was empty at the time allocate was called.
    //
    method Action insertReq(TOKEN_INDEX tokIdx, MEM_ADDRESS addr, MEM_VALUE value) if ((state == SBUFFER_STATE_READY) && ! allocate.notEmpty());
        // New value will be stored to the free list head
        let node_idx = freeListHead;
    
        // Address hash
        let addr_hash = sbAddrHash(addr);

        debugLog.record($format("  SB Insert: tok=%0d, addr=0x%x, hash=%0d, value=0x%x", tokIdx, addr, addr_hash, value));

        // Start by reading the first node on the free list.  We need the node's
        // next pointer to update the free list head.
        sBuffer.readReq(freeListHead);
    
        // Read existing store info for the token
        tokData.readReq(tokIdx);

        reqToken <= tokIdx;
        reqAddr <= addr;
        reqValue <= value;
        reqAddrHash <= addr_hash;
        state <= SBUFFER_STATE_INSERT;
    endmethod: insertReq

    
    //
    // COMMIT:  Remove an entry from the store buffer, returning the value.
    //          The caller will forward the value down the memory hierarchy.
    //
    method Action commitReq(TOKEN_INDEX tokIdx) if (state == SBUFFER_STATE_READY);
        // Read existing store info for the token
        tokData.readReq(tokIdx);
    
        // After token is removed state should return to READY
        removeToken.enq(SBUFFER_STATE_READY);

        reqToken <= tokIdx;
        removeTokenStoreIdx <= 0;
        state <= SBUFFER_STATE_COMMIT;
    endmethod: commitReq


    method ActionValue#(MEMSTATE_SBUFFER_RSP_COMMIT) commitResp();
        let rsp = commitRespPipe.first();
        commitRespPipe.deq();

        return rsp;
    endmethod: commitResp


    //
    // REWIND
    //
    method Action rewindReq(TOKEN_INDEX rewind_to, TOKEN_INDEX rewind_from) if (state == SBUFFER_STATE_READY);
        if (nStoresInBuffer.value() == 0)
        begin
            debugLog.record($format("  SB Rewind: Store buffer is already empty"));
        end
        else if (youngestStoreHint matches tagged Valid .cur_youngest &&&
                 tokenIsOlderOrEq(cur_youngest, rewind_to))
        begin
            // Youngest token in store buffer is older than rewind target.
            debugLog.record($format("  SB Rewind: Nothing to do (rewind to %0d, current youngest is %0d)", rewind_to, cur_youngest));
        end
        else
        begin
            state <= SBUFFER_STATE_REWIND_TOKENS;
            rewindTo  <= rewind_to;

            // Pick the first token in the rewind range that has a store.
            if (youngestStoreHint matches tagged Valid .cur_youngest &&&
                tokenIsOlderOrEq(cur_youngest, rewind_from))
            begin
                rewindCur <= cur_youngest;
                debugLog.record($format("  SB Rewind: Starting rewind at current youngest (%0d)", cur_youngest));
            end
            else
            begin
                rewindCur <= rewind_from;
            end
        end
    endmethod: rewindReq

endmodule
