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
import SpecialFIFOs::*;
import Vector::*;
import FShow::*;

`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_common.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"

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
    TOKEN_INDEX tokIdx;
    Bool hasMore;
    MEM_ADDRESS addr;
    MEM_VALUE value;
}
MEMSTATE_SBUFFER_RSP_WRITEBACK
    deriving (Eq, Bits);


interface MEMSTATE_SBUFFER;

    //
    // LOAD:  Look up and return a value from the store buffer.  lookupReq
    //        returns False if there is no chance the address is in the
    //        store buffer.  In that case, do not wait for lookupResp.
    //        If lookupReq returns True then the caller was wait for
    //        lookupResp, though the result may still be that the address
    //        is not in the buffer.
    //
    method ActionValue#(Bool) lookupReq(TOKEN_INDEX tokIdx, MEM_ADDRESS addr);
    method ActionValue#(MEMSTATE_SBUFFER_RSP_LOOKUP) lookupResp();
    
    //
    // STORE:  Add a value to the store buffer.
    //
    method Action insertReq(TOKEN_INDEX tokIdx, MEM_ADDRESS addr, MEM_VALUE value);
    
    //
    // COMMIT: Move store from TOKEN_INDEX space to STORE_TOKEN_INDEX space.
    //         The store remains in the store buffer until WRITE BACK.
    method Action commitReq(TOKEN_INDEX tokIdx, STORE_TOKEN_INDEX storeTokIdx);

    //
    // WRITE BACK:
    //        Remove an entry from the store buffer, returning the value.
    //        The caller will forward the value down the memory hierarchy.
    //
    method Action writeBackReq(STORE_TOKEN_INDEX storeTokIdx);
    method ActionValue#(MEMSTATE_SBUFFER_RSP_WRITEBACK) writeBackResp();

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

// Store buffer index (pointer) into the store buffer.  Allocating as many
// nodes as store IDs seems safe.
typedef Bit#(STORE_TOKEN_INDEX_SIZE) MEMSTATE_SBUFFER_INDEX;

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
// Primary store buffer data structure, broken into two types.  The metadata
// type holds the linked list pointers.  The data type holds the address and
// value.  The data type is stored separately because it is read less often
// and is large.  Entries on linked lists share an address hash value.
//
typedef struct
{
    Maybe#(MEMSTATE_SBUFFER_INDEX) prev;
    Maybe#(MEMSTATE_SBUFFER_INDEX) next;
}
MEMSTATE_SBUFFER_METADATA_NODE
    deriving (Eq, Bits);

typedef struct
{
    TOKEN_INDEX tokIdx;
    MEM_ADDRESS addr;
    MEM_VALUE value;
    Bool committed;
}
MEMSTATE_SBUFFER_DATA_NODE
    deriving (Eq, Bits);


//
// Addresses are hashed into buckets.  Allocate 4 entries per context.
// The entries are shared by all contexts, so 4 per context is merely
// a sizing heuristic.
//
typedef Bit#(TAdd#(2, CONTEXT_ID_SIZE)) MEMSTATE_SBUFFER_ADDR_HASH_IDX;


//
// States in the store buffer FSM.
//
typedef enum
{
    SBUFFER_STATE_INIT,             // Initialization
    SBUFFER_STATE_READY,            // Ready for command
    SBUFFER_STATE_LOOKUP,           // Lookup (load) result ready
    SBUFFER_STATE_LOOKUP_SEARCH,    // Searching for address in buffer
    SBUFFER_STATE_INSERT,           // Add new entry to buffer
    SBUFFER_STATE_COMMIT,           // Convert TOKEN to STORE_TOKEN
    SBUFFER_STATE_WRITEBACK,        // Write back store (return result & remove entry)
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
    ASSERTION assertStoreTokNotBusy  <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_SBUFFER_BUSY_STORE_TOKEN, ASSERT_ERROR, assertNode);
    ASSERTION assertTooManyStores    <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_SBUFFER_TOO_MANY_TOKEN_STORES, ASSERT_ERROR, assertNode);
    ASSERTION assertFreeListNotEmpty <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_SBUFFER_FREELIST_EMPTY, ASSERT_ERROR, assertNode);
    ASSERTION assertNoStores         <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_SBUFFER_NO_STORES, ASSERT_ERROR, assertNode);

    // ***** Internal state *****

    // Store-buffer data for a token
    let tokInit = MEMSTATE_SBUFFER_TOKEN { nStores: 0, storeNodePtr: ? };
    BRAM_MULTI_READ#(4, TOKEN_INDEX, MEMSTATE_SBUFFER_TOKEN) tokData <- mkLiveTokenBRAMMultiReadInitialized(False, tokInit);
    let tokDataPort_ALLOC = 0;
    let tokDataPort_INSERT = 1;
    let tokDataPort_COMMIT = 2;
    let tokDataPort_REMOVE = 3;

    // Store-buffer data for a store token
    BRAM_MULTI_READ#(2, STORE_TOKEN_INDEX, MEMSTATE_SBUFFER_TOKEN) storeTokData <- mkBRAMPseudoMultiReadInitialized(tokInit);
    let storeTokDataPort_COMMIT = 0;
    let storeTokDataPort_WRITEBACK = 1;

    // Storage for store buffer entries
    BRAM#(MEMSTATE_SBUFFER_INDEX, MEMSTATE_SBUFFER_METADATA_NODE) sBufferMeta <- mkBRAM();
    BRAM#(MEMSTATE_SBUFFER_INDEX, MEMSTATE_SBUFFER_DATA_NODE) sBufferData <- mkBRAM();

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

    // Commit pipeline
    FIFO#(Tuple2#(TOKEN_INDEX, STORE_TOKEN_INDEX)) commitQ <- mkFIFO();

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
    Vector#(NUM_CONTEXTS, Reg#(Maybe#(TOKEN_INDEX))) youngestStoreHint = newVector();
    for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
    begin
        youngestStoreHint[c] <- mkReg(tagged Invalid);
    end

    // Index of store within a token.  Use in multiple rules.
    Reg#(MEMSTATE_SBUFFER_TOK_STORE_CNT) operTokenStoreIdx <- mkRegU();

    // Remove store pipeline
    //             next state    store buffer idx of ref
    FIFO#(Tuple2#(SBUFFER_STATE, MEMSTATE_SBUFFER_INDEX)) removeStore <- mkFIFO1();

    // Hash chain update pipeline during token removal
    FIFO#(Tuple3#(SBUFFER_STATE, MEMSTATE_SBUFFER_INDEX, Maybe#(MEMSTATE_SBUFFER_INDEX))) removeUpdateHashChain <- mkFIFO();


    // Write back response pipeline
    FIFOF#(MEMSTATE_SBUFFER_RSP_WRITEBACK) writeBackRespPipe <- mkFIFOF();

    // Current request details
    Reg#(TOKEN_INDEX) reqToken <- mkRegU();
    Reg#(MEM_ADDRESS) reqAddr  <- mkRegU();
    Reg#(MEM_VALUE)   reqValue <- mkRegU();
    Reg#(MEMSTATE_SBUFFER_ADDR_HASH_IDX) reqAddrHash <- mkRegU();

    // Response details
    Reg#(Maybe#(MEM_VALUE)) respValue <- mkRegU();
    Reg#(TOKEN_INDEX)       respClosestReqToken <- mkRegU();

    //
    // Map address to store buffer address hash
    //
    function MEMSTATE_SBUFFER_ADDR_HASH_IDX sbAddrHash(MEM_ADDRESS addr);
        return truncate(hashBits(addr));
    endfunction


    //
    // init --
    //     Initialize the free list.
    //
    Reg#(MEMSTATE_SBUFFER_INDEX) initIdx <- mkReg(0);

    rule init (state == SBUFFER_STATE_INIT);
        if (initIdx != maxBound)
        begin
            // next points to idx + 1
            sBufferMeta.write(initIdx,
                              MEMSTATE_SBUFFER_METADATA_NODE { prev: tagged Invalid, next: tagged Valid (initIdx + 1) });
        end
        else
        begin
            // Last entry on free list (next is Invalid)
            sBufferMeta.write(initIdx,
                              MEMSTATE_SBUFFER_METADATA_NODE { prev: tagged Invalid, next: tagged Invalid });
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

        debugLog.record($format("SB ALLOC ") + fshow(tok_idx));

        // We could do the allocation in a single cycle if we skipped the check
        // that the last instance of the token was cleaned up.  Instead, we
        // read the current state and confirm that it holds no stores.
        tokData.readPorts[tokDataPort_ALLOC].readReq(tok_idx);
    endrule

    rule allocDone (True);
        let sb_tok <- tokData.readPorts[tokDataPort_ALLOC].readRsp();

        // Make sure the last instance of the token ID was cleaned up properly.
        // If it was there is no more work to do.
        //
        // An error here could also be caused by an alias problem if a store
        // token's lifetime extends so long that it is alive at the same time
        // as its live-token alias.
        //
        if (sb_tok.nStores != 0) 
        begin
            debugLog.record($format("ERROR: ALLOC ASSERTION FAILURE: nStores: %0d", sb_tok.nStores));
        end

        assertNotBusy(sb_tok.nStores == 0);
    endrule


    //
    // startLookupReq --
    //     The lookupReq method determined that a hash chain exists for the
    //     address.  Start a walk of the chain too see whether there is a match.
    //
    FIFOF#(Tuple3#(TOKEN_INDEX,
                   MEM_ADDRESS,
                   MEMSTATE_SBUFFER_ADDR_HASH_IDX)) lookupQ <- mkBypassFIFOF();

    (* conservative_implicit_conditions *)
    rule startLookupReq (state == SBUFFER_STATE_READY);
        match {.tok_idx, .addr, .hash} = lookupQ.first();
        lookupQ.deq();

        // Read the head of the address hash chain again in case it
        // has changed since lookupReq().
        if (addrHash.sub(hash) matches tagged Valid .node_idx)
        begin
            // Read the first node in the hash chain
            debugLog.record($format("  SB Lookup: ") + fshow(tok_idx) + $format(", addr=0x%x, hash=%0d, head node=%0d", addr, hash, node_idx));

            sBufferMeta.readReq(node_idx);
            sBufferData.readReq(node_idx);
            state <= SBUFFER_STATE_LOOKUP_SEARCH;
        end
        else
        begin
            // Hash chain now empty.  Return nothing.
            debugLog.record($format("  SB hash now empty: ") + fshow(tok_idx) + $format(", addr=0x%x, hash=%0d", addr, hash));
            state <= SBUFFER_STATE_LOOKUP;
        end

        // Initialize global request state
        reqAddr <= addr;
        reqToken <= tok_idx;
    
        // Initialize response
        respValue <= tagged Invalid;
    endrule


    //
    // lookupSearchNode --
    //     Iterate over nodes in the store buffer, looking for a match to
    //     the request.  On match return the value.  On mismatch loop by
    //     requesting the next node in the hash chain.
    //
    (* conservative_implicit_conditions *)
    rule lookupSearchNode (state == SBUFFER_STATE_LOOKUP_SEARCH);
        let node_meta <- sBufferMeta.readRsp();
        let node_data <- sBufferData.readRsp();
        
        if ((node_data.addr == reqAddr) &&
            (node_data.tokIdx.context_id == reqToken.context_id))
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
                debugLog.record($format("    SB Addr Match: ") + fshow(reqToken) + $format(", node_token=") + fshow(node_data.tokIdx) + $format(", node_addr=0x%x, value=0x%x, ending search", node_data.addr, node_data.value));

                respValue <= tagged Valid node_data.value;
                state <= SBUFFER_STATE_LOOKUP;
            end
            else
            begin
                // Out-of-order mode
                debugLog.record($format("    SB Addr Match: ") + fshow(reqToken) + $format(", node_token=") + fshow(node_data.tokIdx) + $format(", node_addr=0x%x, value=0x%x", node_data.addr, node_data.value));

                if (node_data.committed)
                begin
                    // This node has been committed.  Stores are pushed to the head
                    // of the list, so this store must be the youngest committed
                    // store to the address.  Either return the best speculative
                    // result or this committed value.
                    if (! isValid(respValue))
                    begin
                        debugLog.record($format("      SB Match Committed: ") + fshow(reqToken));
                        respValue <= tagged Valid node_data.value;
                    end

                    state <= SBUFFER_STATE_LOOKUP;
                end
                else
                begin
                    // Use this hit as current best match if the store is before
                    // the load and either this is the first address match or this
                    // store was executed later than the previous match.
                    if (tokenIsOlderOrEq(node_data.tokIdx.token_id, reqToken.token_id) &&
                       (tokenIsOlderOrEq(respClosestReqToken.token_id, node_data.tokIdx.token_id) || ! isValid(respValue)))
                    begin
                        debugLog.record($format("      SB Current Best: ") + fshow(reqToken) + $format(", node_token=") + fshow(node_data.tokIdx));

                        respValue <= tagged Valid node_data.value;
                        respClosestReqToken <= node_data.tokIdx;
                    end

                    // Is there more to search?
                    if (node_meta.next matches tagged Valid .n_idx)
                    begin
                        // Yes -- search next entry
                        sBufferMeta.readReq(n_idx);
                        sBufferData.readReq(n_idx);
                    end
                    else
                    begin
                        // No -- done
                        state <= SBUFFER_STATE_LOOKUP;
                    end
                end
            end
        end
        else
        begin
            //
            // Mismatch.  Is there another entry in the chain?
            //
            if (node_meta.next matches tagged Valid .n_idx)
            begin
                // Yes -- search next entry
                debugLog.record($format("    SB mismatch: ") + fshow(reqToken) + $format(", node_token=") + fshow(node_data.tokIdx) + $format(", node_addr=0x%x, node_next=%0d", node_data.addr, n_idx));

                sBufferMeta.readReq(n_idx);
                sBufferData.readReq(n_idx);
            end
            else
            begin
                // No -- give up
                debugLog.record($format("    SB mismatch: ") + fshow(reqToken) + $format(", node_token=") + fshow(node_data.tokIdx) + $format(", node_addr=0x%x, end of chain", node_data.addr));

                state <= SBUFFER_STATE_LOOKUP;
            end
        end
    endrule

    
    //
    // insertStore --
    //     Add a store to the store buffer.
    //
    (* conservative_implicit_conditions *)
    rule insertStore (state == SBUFFER_STATE_INSERT);
        // Receive the old node and address hash head values
        let old_node <- sBufferMeta.readRsp();
        let old_tok_data <- tokData.readPorts[tokDataPort_INSERT].readRsp();

        let old_addr_hash_head = addrHash.sub(reqAddrHash);

        // Update youngest store hint.  The hint may not be set if there were
        // already stores in the buffer and the current hint is invalid.  In
        // that case we have no way of knowing the age of the new store relative
        // to others in the buffer.
        let ctx_id = reqToken.context_id;
        if (nStoresInBuffer.value() == 0)
        begin
            youngestStoreHint[ctx_id] <= tagged Valid reqToken;
            debugLog.record($format("    SB Store ") + fshow(reqToken) + $format(" is youngest"));
        end
        else if (youngestStoreHint[ctx_id] matches tagged Valid .cur_youngest &&&
                 tokenIsOlderOrEq(cur_youngest.token_id, reqToken.token_id))
        begin
            youngestStoreHint[ctx_id] <= tagged Valid reqToken;
            debugLog.record($format("    SB Store ") + fshow(reqToken) + $format(" is youngest -- replaces ") + fshow(cur_youngest));
        end

        // New node is taken from head of free list
        let node_idx = freeListHead;
        nStoresInBuffer.up();
        
        //
        // Update node with new store details
        //
        MEMSTATE_SBUFFER_METADATA_NODE new_meta_node;
        new_meta_node.prev = tagged Invalid;
        new_meta_node.next = old_addr_hash_head;
        sBufferMeta.write(node_idx, new_meta_node);

        MEMSTATE_SBUFFER_DATA_NODE new_data_node;
        new_data_node.tokIdx = reqToken;
        new_data_node.addr = reqAddr;
        new_data_node.value = reqValue;
        new_data_node.committed = False;
        sBufferData.write(node_idx, new_data_node);

        //
        // Update store info for the token.  Tokens may have more than one store.
        //
        debugLog.record($format("    SB Store #%0d for ", old_tok_data.nStores) + fshow(reqToken) + $format(", node_idx=%0d", node_idx));
        assertTooManyStores(old_tok_data.nStores != `MEMSTATE_SBUFFER_STORES_PER_TOKEN);
        let tok_data = old_tok_data;
        // Pointer to new node
        tok_data.storeNodePtr[tok_data.nStores] = node_idx;
        tok_data.nStores = tok_data.nStores + 1;
        debugLog.record($format("    SB INSERT WRITE TOK DATA. IDX: %0d, NUM STORES: %0d", reqToken, tok_data.nStores));
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
            sBufferMeta.readReq(hash_next);
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
    // insertStorePrev --
    //     Final step of adding a store:  set the prev pointer in the address
    //     hash chain from the old head of the chain to the node just added.
    //
    (* descending_urgency = "insertStorePrev, insertStore" *)
    rule insertStorePrev (state == SBUFFER_STATE_INSERT);
        // Receive the old node and address hash head values
        let next_node <- sBufferMeta.readRsp();
        match {.new_node_idx, .next_node_idx} = insertPrev.first();
        insertPrev.deq();
        
        next_node.prev = tagged Valid new_node_idx;
        sBufferMeta.write(next_node_idx, next_node);

        debugLog.record($format("    SB Set prev pointer for node=%0d, prev=%0d", next_node_idx, new_node_idx));

        state <= SBUFFER_STATE_READY;
    endrule


    //
    // moveToStoreToken --
    //     Token is being committed but not yet written back to memory.  The
    //     details are now associated with a store token instead of a main
    //     token.  This allows us to keep the main token space smaller and
    //     pack long-lived stores densely.
    //
    FIFO#(Tuple2#(Bool, MEMSTATE_SBUFFER_INDEX)) markCommittedQ <- mkFIFO();

    rule moveToStoreToken (state == SBUFFER_STATE_COMMIT);
        match {.tok_idx, .store_tok_idx} = commitQ.first();

        let tok_data = tokData.readPorts[tokDataPort_COMMIT].peek();
        let old_store_tok = storeTokData.readPorts[storeTokDataPort_COMMIT].peek();

        // Has previous use of the store token been written back?
        assertStoreTokNotBusy(old_store_tok.nStores == 0);

        debugLog.record($format("  SB COMMIT ") + fshow(tok_idx) + $format(" is now ") + fshow(store_tok_idx) + $format(", store id=%0d", operTokenStoreIdx));

        // Request the data for the store
        let node_idx = tok_data.storeNodePtr[operTokenStoreIdx];
        sBufferData.readReq(node_idx);

        Bool done;
        if (operTokenStoreIdx + 1 == tok_data.nStores)
        begin
            // Done with all stores for the token.
            done = True;
            commitQ.deq();

            // Pop the read data
            let dummy0 <- tokData.readPorts[tokDataPort_COMMIT].readRsp();
            let dummy1 <- storeTokData.readPorts[storeTokDataPort_COMMIT].readRsp();

            // Move the record to the store token's data
            tokData.write(tok_idx, tokInit);
            storeTokData.write(store_tok_idx, tok_data);
        end
        else
        begin
            done = False;

            // More stores for this token.  This rule will run again.
            operTokenStoreIdx <= operTokenStoreIdx + 1;
        end
            
        // Pass control details to markStoreCommitted rule.
        markCommittedQ.enq(tuple2(done, node_idx));
    endrule


    //
    // markStoreCommitted --
    //     Receive requests from moveToStoreToken to mark individual store
    //     buffer entries committed.  Once committed the original TOKEN_INDEX
    //     is no longer used to address the token.
    //
    rule markStoreCommitted (state == SBUFFER_STATE_COMMIT);
        match {.done, .node_idx} = markCommittedQ.first();
        markCommittedQ.deq();

        let node <- sBufferData.readRsp();
        node.committed = True;
        sBufferData.write(node_idx, node);

        if (done)
        begin
            state <= SBUFFER_STATE_READY;
        end
    endrule


    //
    // rewindStores --
    //     Remove all stores associated with a token from the store buffer
    //     due to a rewind.  The rule keeps processing the token until all
    //     stores are handled.
    //
    FIFO#(Tuple2#(TOKEN_INDEX, SBUFFER_STATE)) rewindQ <- mkFIFO();

    (* descending_urgency = "alloc, rewindStores" *)
    (* conservative_implicit_conditions *)
    rule rewindStores (state == SBUFFER_STATE_REWIND);
        let tok_data = tokData.readPorts[tokDataPort_REMOVE].peek();
        match {.tok_idx, .next_state} = rewindQ.first();

        if (tok_data.nStores != 0)
        begin
            debugLog.record($format("  SB rewind: ") + fshow(tok_idx) + $format(", store id=%0d", operTokenStoreIdx));

            // Request the data for the store
            let node_idx = tok_data.storeNodePtr[operTokenStoreIdx];
            sBufferMeta.readReq(node_idx);
            sBufferData.readReq(node_idx);

            if (operTokenStoreIdx + 1 == tok_data.nStores)
            begin
                // Done with all stores for the token.  Clear it.
                debugLog.record($format("    SB REMOVE WRITE TOK DATA. IDX: %0d, NUM STORES: %0d", tok_idx, tokInit.nStores));
                tokData.write(tok_idx, tokInit);

                // Pop the read result
                let dummy <- tokData.readPorts[tokDataPort_REMOVE].readRsp();

                // Done processing this token
                rewindQ.deq();
            end
            else
            begin
                // More stores for this token.  This rule will run again.
                operTokenStoreIdx <= operTokenStoreIdx + 1;

                // Iterate in this state while there are more stores for this
                // token.
                next_state = state;
            end
            
            // Pass control details to removeOneStore rule.
            removeStore.enq(tuple2(next_state, node_idx));

            // Update global state
            nStoresInBuffer.down();
            let ctx_id = tok_idx.context_id;
            if (youngestStoreHint[ctx_id] matches tagged Valid .cur_youngest &&&
                cur_youngest == tok_idx)
            begin
                youngestStoreHint[ctx_id] <= tagged Invalid;
            end
        end
        else
        begin
            //
            // Doing rewind and token has no stores.  Nothing to do.
            //
            debugLog.record($format("  SB rewind: no stores, ") + fshow(tok_idx));
            let dummy <- tokData.readPorts[tokDataPort_REMOVE].readRsp();
            rewindQ.deq();
            state <= next_state;
        end
    endrule


    //
    // writeBackStores --
    //     Remove all stores associated with a store token from the store buffer
    //     due to a write back.  The rule keeps processing the token until all
    //     stores are handled.
    //
    FIFO#(STORE_TOKEN_INDEX) writeBackQ <- mkFIFO();

    (* conservative_implicit_conditions *)
    rule writeBackStores (state == SBUFFER_STATE_WRITEBACK);
        let store_tok_idx = writeBackQ.first();

        let tok_data = storeTokData.readPorts[storeTokDataPort_WRITEBACK].peek();
        let next_state = SBUFFER_STATE_READY;

        // Token must have stores to write back!
        assertNoStores(tok_data.nStores != 0);

        debugLog.record($format("  SB write back: ") + fshow(store_tok_idx) + $format(", store id=%0d", operTokenStoreIdx));

        // Request the data for the store
        let node_idx = tok_data.storeNodePtr[operTokenStoreIdx];
        sBufferMeta.readReq(node_idx);
        sBufferData.readReq(node_idx);

        if (operTokenStoreIdx + 1 == tok_data.nStores)
        begin
            // Done with all stores for the token.  Pop the read response
            // and clear the token.
            debugLog.record($format("    SB REMOVE WRITE TOK DATA. IDX: %0d, NUM STORES: %0d", store_tok_idx, tokInit.nStores));

            let dummy <- storeTokData.readPorts[storeTokDataPort_WRITEBACK].readRsp();
            writeBackQ.deq();

            storeTokData.write(store_tok_idx, tokInit);
        end
        else
        begin
            // More stores for this token.
            operTokenStoreIdx <= operTokenStoreIdx + 1;

            // Iterate in this state while there are more stores for this
            // token.
            next_state = state;
        end
            
        // Pass control details to removeOneStore rule.
        removeStore.enq(tuple2(next_state, node_idx));

        // Update global state
        nStoresInBuffer.down();
    endrule


    //
    // removeOneStore --
    //     Fed by remove_token_stores.  Removes a store buffer node from
    //     its address hash chain.  For write backs, also forwards store data to
    //     write back response method.  The write back logic can then start writing
    //     the value to memory while code here mucks with linked lists.
    //
    (* descending_urgency = "removeOneStore, writeBackStores, rewindStores" *)
    (* conservative_implicit_conditions *)
    rule removeOneStore ((state == SBUFFER_STATE_WRITEBACK) || (state == SBUFFER_STATE_REWIND));
        let node_meta <- sBufferMeta.readRsp();
        let node_data <- sBufferData.readRsp();
        match { .next_state, .node_idx } = removeStore.first();
        removeStore.deq();

        debugLog.record($format("    SB remove addr=0x%x, value=0x%x, next_state=%0d", node_data.addr, node_data.value, next_state));

        // Respond with store info for write backs
        if (state == SBUFFER_STATE_WRITEBACK)
            writeBackRespPipe.enq(MEMSTATE_SBUFFER_RSP_WRITEBACK { tokIdx: node_data.tokIdx, hasMore: (next_state == SBUFFER_STATE_WRITEBACK), addr: node_data.addr, value: node_data.value });
        
        //
        // Put the node back on the free list
        //
        sBufferMeta.write(node_idx, MEMSTATE_SBUFFER_METADATA_NODE { prev: tagged Invalid, next: tagged Valid freeListHead });
        freeListHead <= node_idx;

        //
        // Now the fun of dropping a node from the address hash linked list...
        //
        
        // Prev pointer valid?
        if (node_meta.prev matches tagged Valid .prev_node_idx)
        begin
            // Yes:  request previous node so its next pointer can be updated
            removeUpdateHashChain.enq(tuple3(next_state, prev_node_idx, node_meta.next));
            sBufferMeta.readReq(prev_node_idx);
            state <= SBUFFER_STATE_REMOVE_UPD_PREV;
        end
        else
        begin
            // No:  head of list.  Write sbAddrHash directly.
            let addr_hash = sbAddrHash(node_data.addr);
            addrHash.upd(addr_hash, node_meta.next);

            // Next pointer valid?
            if (node_meta.next matches tagged Valid .next_node_idx)
            begin
                // Yes:  request next node so its prev pointer can be updated
                removeUpdateHashChain.enq(tuple3(next_state, next_node_idx, tagged Invalid));
                sBufferMeta.readReq(next_node_idx);
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
    // removeStoreUdpatePrev --
    //     Updates the node before the node being removed from an address
    //     hash chain.
    //
    rule removeStoreUpdatePrev (state == SBUFFER_STATE_REMOVE_UPD_PREV);
        let prev_node <- sBufferMeta.readRsp();
        match { .next_state, .prev_node_idx, .next_node } = removeUpdateHashChain.first();
        removeUpdateHashChain.deq();
        
        let old_next_idx = validValue(prev_node.next);

        // Update the pointer
        prev_node.next = next_node;
        sBufferMeta.write(prev_node_idx, prev_node);
        
        // Does the new next node need its prev pointer fixed?
        if (next_node matches tagged Valid .next_node_idx)
        begin
            // Yes:  prepare for removeStoreUpdateNext rule
            debugLog.record($format("      SB update prev: node=%0d, old_next=%0d, new_next=%0d", prev_node_idx, old_next_idx, next_node_idx));

            removeUpdateHashChain.enq(tuple3(next_state, next_node_idx, tagged Valid prev_node_idx));
            sBufferMeta.readReq(next_node_idx);
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
    // removeStoreUpdateNext --
    //     Final stage of address hash chain update.  Fix prev pointer of next node.
    //
    rule removeStoreUpdateNext (state == SBUFFER_STATE_REMOVE_UPD_NEXT);
        let next_node <- sBufferMeta.readRsp();
        match { .next_state, .next_node_idx, .prev_node } = removeUpdateHashChain.first();
        removeUpdateHashChain.deq();
        
        let old_prev_idx = validValue(next_node.prev);

        next_node.prev = prev_node;
        sBufferMeta.write(next_node_idx, next_node);

        if (prev_node matches tagged Valid .prev_node_idx)
            debugLog.record($format("      SB update next: node=%0d, old_prev=%0d, new_prev=%0d", next_node_idx, old_prev_idx, prev_node_idx));
        else
            debugLog.record($format("      SB update next: node=%0d, old_prev=%0d, new_prev=NULL", next_node_idx, old_prev_idx));

        state <= next_state;
    endrule


    //
    // rewind --
    //     Remove all tokens from rewindCur down to rewindTo.  Rewind of a single
    //     token is almost identical in behavior to write back.  The only difference
    //     is write back streams out the store values as it removes entries from
    //     the store buffer.  Rewind shares the write back rules (remove_....)
    //
    Reg#(TOKEN_INDEX) rewindTo  <- mkRegU();
    Reg#(TOKEN_INDEX) rewindCur <- mkRegU();

    (* descending_urgency = "alloc, rewind" *)
    rule rewind (state == SBUFFER_STATE_REWIND_TOKENS);
        // Read token's store pointers.  Will be consumed by remove_token_stores.
        tokData.readPorts[tokDataPort_REMOVE].readReq(rewindCur);

        // After token is removed state should return to REWIND for processing
        // the next token until the rewind point is reached.
        let next_rewind = rewindCur - 1;
        rewindQ.enq(tuple2(rewindCur,
                           next_rewind != rewindTo ? SBUFFER_STATE_REWIND_TOKENS :
                                                     SBUFFER_STATE_READY));

        operTokenStoreIdx <= 0;

        rewindCur <= next_rewind;
        state <= SBUFFER_STATE_REWIND;
    endrule


    //
    // ***** Public methods *****
    //

    //
    // LOAD:
    //   Search the store buffer for a match.  This method starts with a quick
    //   check of the address hash table.  If the hash bucket is empty then there
    //   is no match in the table and the method responds "False".
    //
    //   The predicate on this method is relaxed, requiring only that no update
    //   to the head of the hash chains be in flight and that all write back
    //   requests are complete.  This optimizes the lookup case where hash
    //   buckets are empty (the typical case.)  If the hash bucket is not empty
    //   the request is forwarded to startLookupReq, which waits until the
    //   store buffer is ready for a new request.
    //
    method ActionValue#(Bool) lookupReq(TOKEN_INDEX tokIdx, MEM_ADDRESS addr) if ((state != SBUFFER_STATE_INIT) &&
                                                                                  (state != SBUFFER_STATE_INSERT) &&
                                                                                  ! writeBackRespPipe.notEmpty());
        Bool resp;

        // Read the head of the address hash chain
        let hash = sbAddrHash(addr);
        if (addrHash.sub(hash) matches tagged Valid .node_idx)
        begin
            // Hash table isn't empty, so we must walk the chain.  Forward the
            // request to startLookupReq, which has a stronger predicate than
            // this method.
            debugLog.record($format("  SB hash not empty: ") + fshow(tokIdx) + $format(", addr=0x%x, hash=%0d, head node=%0d", addr, hash, node_idx));

            lookupQ.enq(tuple3(tokIdx, addr, hash));
            resp = True;
        end
        else
        begin
            // Hash bucket is empty.  Respond with a "quick" miss.
            debugLog.record($format("  SB Lookup Miss (empty hash head): ") + fshow(tokIdx) + $format(", addr=0x%x, hash=%0d", addr, hash));
            
            resp = False;
        end

        return resp;
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
    method Action insertReq(TOKEN_INDEX tokIdx, MEM_ADDRESS addr, MEM_VALUE value) if ((state == SBUFFER_STATE_READY) &&
                                                                                       ! allocate.notEmpty() &&
                                                                                       ! lookupQ.notEmpty());
        // New value will be stored to the free list head
        let node_idx = freeListHead;
    
        // Address hash
        let addr_hash = sbAddrHash(addr);

        debugLog.record($format("  SB Insert: ") + fshow(tokIdx) + $format(", addr=0x%x, hash=%0d, value=0x%x", addr, addr_hash, value));

        // Start by reading the first node on the free list.  We need the node's
        // next pointer to update the free list head.
        sBufferMeta.readReq(freeListHead);

        // Read existing store info for the token
        tokData.readPorts[tokDataPort_INSERT].readReq(tokIdx);

        reqToken <= tokIdx;
        reqAddr <= addr;
        reqValue <= value;
        reqAddrHash <= addr_hash;
        state <= SBUFFER_STATE_INSERT;
    endmethod: insertReq


    //
    // COMMIT: Move store from TOKEN_INDEX space to STORE_TOKEN_INDEX space.
    //         The store remains in the store buffer until WRITE BACK.
    method Action commitReq(TOKEN_INDEX tokIdx, STORE_TOKEN_INDEX storeTokIdx) if ((state == SBUFFER_STATE_READY) &&
                                                                                   ! allocate.notEmpty() &&
                                                                                   ! lookupQ.notEmpty());
        // Read the current mapping.  The reads will be consumed by
        // moveToStoreToken.
        tokData.readPorts[tokDataPort_COMMIT].readReq(tokIdx);
        storeTokData.readPorts[storeTokDataPort_COMMIT].readReq(storeTokIdx);

        operTokenStoreIdx <= 0;
        commitQ.enq(tuple2(tokIdx, storeTokIdx));
        state <= SBUFFER_STATE_COMMIT;
    endmethod: commitReq


    //
    // WRITE BACK:
    //        Remove an entry from the store buffer, returning the value.
    //        The caller will forward the value down the memory hierarchy.
    //
    method Action writeBackReq(STORE_TOKEN_INDEX storeTokIdx) if (state == SBUFFER_STATE_READY &&
                                                                  ! lookupQ.notEmpty());
        // Read existing store info for the token
        storeTokData.readPorts[storeTokDataPort_WRITEBACK].readReq(storeTokIdx);

        operTokenStoreIdx <= 0;
        writeBackQ.enq(storeTokIdx);
        state <= SBUFFER_STATE_WRITEBACK;
    endmethod: writeBackReq


    method ActionValue#(MEMSTATE_SBUFFER_RSP_WRITEBACK) writeBackResp();
        let rsp = writeBackRespPipe.first();
        writeBackRespPipe.deq();

        return rsp;
    endmethod: writeBackResp


    //
    // REWIND
    //
    method Action rewindReq(TOKEN_INDEX rewind_to, TOKEN_INDEX rewind_from) if (state == SBUFFER_STATE_READY &&
                                                                                ! lookupQ.notEmpty());
        let ctx_id = rewind_to.context_id;

        if (nStoresInBuffer.value() == 0)
        begin
            debugLog.record($format("  SB Rewind: Store buffer is already empty"));
        end
        else if (youngestStoreHint[ctx_id] matches tagged Valid .cur_youngest &&&
                 tokenIsOlderOrEq(cur_youngest.token_id, rewind_to.token_id))
        begin
            // Youngest token in store buffer is older than rewind target.
            debugLog.record($format("  SB Rewind: Nothing to do (rewind to ") + fshow(rewind_to) + $format(", current youngest is ") + fshow(cur_youngest) + $format(")"));
        end
        else if (rewind_to == rewind_from)
        begin

            // They asked for an empty rewind.
            debugLog.record($format("  SB Rewind: Nothing to do (rewind to ") + fshow(rewind_to) + $format(" is equal to rewind from ") + fshow(rewind_from) + $format(")"));
        
        end
        else
        begin
            state <= SBUFFER_STATE_REWIND_TOKENS;
            rewindTo  <= rewind_to;

            // Pick the first token in the rewind range that has a store.
            if (youngestStoreHint[ctx_id] matches tagged Valid .cur_youngest &&&
                tokenIsOlderOrEq(cur_youngest.token_id, rewind_from.token_id))
            begin
                rewindCur <= cur_youngest;
                debugLog.record($format("  SB Rewind: Starting rewind at current youngest ") + fshow(cur_youngest));
            end
            else
            begin
                rewindCur <= rewind_from;
            end
        end
    endmethod: rewindReq

endmodule
