
// memstate_storebuffer_bucket_hash

// A store buffer which uses a bucket-hash scheme.
// This version sequentializes all operations.
// This acts as a giant lock on all the lists.
// Making this locking finer-grained could increase parallelism.

// Library imports

import Vector::*;
import FIFOF::*;

// Project foundation imports

`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/dict/ASSERTIONS_STOREBUFFER.bsh"

// ***** Typedefs ***** //

// MEMSTATE_SBUFFER_REQ

// The type of a request to the store buffer.

typedef union tagged
{
    struct {TOKEN tok; MEM_VALUE value; MEM_ADDRESS addr;}    SBUFFER_REQ_INSERT;
    struct {TOKEN tok; MEM_ADDRESS addr;}                     SBUFFER_REQ_LOOKUP;
    TOKEN                                                  SBUFFER_REQ_COMMIT;
    struct {TOKEN_INDEX rewind; TOKEN_INDEX youngest;}     SBUFFER_REQ_REWIND;
}
    MEMSTATE_SBUFFER_REQ 
        deriving
            (Eq, Bits);

// MEMSTATE_SBUFFER_RSP

// The type of a response from the store buffer.

typedef union tagged
{
    struct {TOKEN tok; MEM_ADDRESS addr; Maybe#(MEM_VALUE) mresult;}      SBUFFER_RSP_LOOKUP;
    struct {TOKEN tok; MEM_ADDRESS addr; Bool hasMore; MEM_VALUE value;}  SBUFFER_RSP_COMMIT;
}
    MEMSTATE_SBUFFER_RSP
        deriving 
            (Eq, Bits);


// SB_BUCKET

// The number of buckets we hash into.

typedef Bit#(`SBUFFER_HASH_BITS) SB_BUCKET;

// SECONDARY_INDEX

// Index into the backing RAM.

typedef `SBUFFER_NUM_SECONDARY_SLOTS NUM_SECONDARY_SLOTS;
typedef TLog#(NUM_SECONDARY_SLOTS) SECONDARY_INDEX_SIZE;
typedef Bit#(SECONDARY_INDEX_SIZE) SECONDARY_INDEX;

// SB_INDEX

// A pointer to either the main RAM or the secondary RAM.

typedef union tagged 
{
    void SB_NULL;
    TOKEN SB_MAIN;
    Tuple2#(TOKEN, SECONDARY_INDEX) SB_SECONDARY;
}
    SB_INDEX
        deriving (Eq, Bits);

// getIndexToken

// To be called only on a known non-null index.

function TOKEN getIndexToken(SB_INDEX idx);

    case (idx) matches
        tagged SB_NULL: return ?;
        tagged SB_MAIN .tok: return tok;
        tagged SB_SECONDARY {.tok, .idx}: return tok;
    endcase

endfunction

// SB_MAIN_NODE

// The values about each token we store in the main RAM.

typedef struct
{
    CONTEXT_MASK     ctxtMask;
    MEM_ADDRESS      addr;
    MEM_VALUE        value;
}
    SB_MAIN_NODE
        deriving (Bits, Eq);

function SB_MAIN_NODE initMainNode(CONTEXT_MASK c, MEM_ADDRESS a, MEM_VALUE v);

    return SB_MAIN_NODE {ctxtMask: c, addr: a, value: v};

endfunction

// SB_SECONDARY NODE

// The value about each token we store in the secondary RAM.

typedef struct
{
    TOKEN_INDEX      tok_index;
    CONTEXT_MASK     ctxtMask;
    MEM_ADDRESS      addr;
    MEM_VALUE        value;
}
    SB_SECONDARY_NODE
        deriving (Bits, Eq);

function SB_SECONDARY_NODE initSecondaryNode(TOKEN_INDEX idx, CONTEXT_MASK c, MEM_ADDRESS a, MEM_VALUE v);

    return SB_SECONDARY_NODE {tok_index: idx, ctxtMask: c, addr: a, value: v};

endfunction

function SB_MAIN_NODE toMainNode(SB_SECONDARY_NODE node);

    return SB_MAIN_NODE {ctxtMask: node.ctxtMask, addr: node.addr, value: node.value};

endfunction

// State for intermediate stages.

// INSERT_STATE

typedef union tagged
{
    void      INSERT_NORMAL;
    SB_INDEX  INSERT_SEARCH_MAIN;
    SB_INDEX  INSERT_SEARCH_SECONDARY;
    SB_BUCKET INSERT_ALLOC_SECONDARY;
}
    INSERT_STATE
        deriving (Eq, Bits);

// LOOKUP_STATE

typedef union tagged
{
    void  LOOKUP_NORMAL;
    TOKEN LOOKUP_SEARCH_MAIN;
    TOKEN LOOKUP_SEARCH_SECONDARY;
}
    LOOKUP_STATE
        deriving (Eq, Bits);

// REWIND_STATE

typedef union tagged
{
    void            REWIND_NORMAL;
    SECONDARY_INDEX REWIND_SEARCH_SECONDARY;
    
}
    REWIND_STATE
        deriving (Eq, Bits);

// COMMIT2_STATE

typedef union tagged
{
    void            COMMIT2_NORMAL;
    SECONDARY_INDEX COMMIT2_STATE;
}
    COMMIT2_STATE
        deriving (Eq, Bits);

// CLEANUP_INFO

// State for dealing with removing dead elements from the lists.

typedef struct
{
    Bool      inGap;          // Are we in a gap in the list?
    SB_INDEX  lastValidIndex; // What was the last valid index we've seen. (SB_NULL = none.)
    SB_BUCKET bucket;         // What bucket are we cleaning?
}
    CLEANUP_INFO
        deriving (Eq, Bits);

// mkFUNCP_StoreBuffer

// A version of the store-buffer using bucket hashes which supports multiple contexts.

module [HASIM_MODULE] mkFUNCP_StoreBuffer
    // interface:
        ();

    // ***** Local State ***** //

    // Do tokens have one or two stores?
    Reg#(Vector#(NUM_TOKENS, Bool)) hasSecondStore  <- mkReg(Vector::replicate(False));

    // We use these RAMs as a bucket-hashed linked-list of nodes.
    Reg#(Vector#(NUM_TOKENS, Bool))  mainListValids <- mkReg(Vector::replicate(False));
    BRAM#(TOKEN_INDEX, SB_MAIN_NODE) mainListValues <- mkBRAM();
    BRAM#(TOKEN_INDEX, SB_INDEX)     mainListNexts  <- mkBRAMInitialized(tagged Invalid);

    // We use a smaller backing RAM for any stores with a second address.
    Reg#(Vector#(NUM_SECONDARY_SLOTS, Bool))  secondaryListValids <- mkVector(Vector::replicate(False));
    BRAM#(SECONDARY_INDEX, SB_SECONDARY_NODE) secondaryListValues <- mkBRAM();
    BRAM#(SECONDARY_INDEX, SB_INDEX)          secondaryListNexts  <- mkBRAMInitialized(tagged Invalid);
    
    // Track where we should insert into the secondary list.
    Counter#(SECONDARY_INDEX_SIZE) secondaryListInsertIndex <- mkCounter(0);
    
    // Is the secondary list full?
    Counter#(SECONDARY_INDEX_SIZE) numInSecondaryList <- mkCounter(0);
    let secondaryListFull = numInSecondaryList.value() == `SBUFFER_NUM_SECONDARY_SLOTS;
    let secondaryListAlmostFull = numInSecondaryList.value() == (`SBUFFER_NUM_SECONDARY_SLOTS - 1);
    
    // The bucket hash divides things into N lists or "buckets". 
    // This reg file stores the heads of these lists. Invalid indicates empty list.
    LUTRAM#(MEM_ADDRESS_HASH, SB_INDEX) bucketHeads <- mkLUTRAMInitialized(tagged SB_NULL);

    // Track which contexts should search a certain bucket. We don't need to search
    // a certain bucket if none of the nodes are visible to the target context.
    // This is a Blum Filter in that it can result in false positives. However we
    // reset the context mask when the given bucket is empty.
    LUTRAM#(MEM_ADDRESS_HASH, CONTEXT_MASK) bucketContexts <- mkLUTRAMInitialized(initEmptyContextMask());

    // Intermediate state for list operations.
    Reg#(INSERT_STATE)  insertState  <- mkReg(INSERT_NORMAL);
    Reg#(LOOKUP_STATE)  lookupState  <- mkReg(LOOKUP_NORMAL);
    Reg#(COMMIT2_STATE) commit2State <- mkReg(COMMIT2_NORMAL);
    Reg#(REWIND_STATE)  rewindState  <- mkReg(REWIND_NORMAL);
    FIFOF#(TOKEN)       commitQ      <- mkFIFOF1();
    

    // We can lookup if we're not changing the state of the list.
    let canLookup = insertState == INSERT_NORMAL && rewindState == REWIND_NORMAL && !commitQ.notEmpty();

    // Intermediate state for cleaning up the lists as we walk them.
    Reg#(CLEAN_UP_INFO) insertCleanupInfo <- mkRegU();
    Reg#(CLEAN_UP_INFO) lookupleanupInfo <- mkRegU();

    // Debug log for simualtion.
    DEBUG_FILE debugLog <- mkDebugFile(`SBUFFER_LOGFILE_NAME);

    // ***** Soft Connections ***** //

    // Link to the mem state.
    Connection_Server#(TOKEN_INDEX, VOID)  linkRegstate <- mkConnection_Server("storeBufferAllocate");
    Connection_Server#(MEMSTATE_SBUFFER_REQ, MEMSTATE_SBUFFER_RSP)  linkMemState <- mkConnection_Server("mem_storebuf");

    // ***** Assertions ***** //
    
    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_STOREBUFFER__BASE);
    ASSERTION assertSecondaryStorageHasRoom  <- mkAssertionChecker(`ASSERTIONS_STOREBUFFER_NOT_FULL, ASSERT_ERROR, assertNode);
    ASSERTION assertTokToCommitHasStore      <- mkAssertionChecker(`ASSERTIONS_STOREBUFFER_LEGAL_COMMIT, ASSERT_ERROR, assertNode);
    ASSERTION assertTokToCommitListNotEmpty  <- mkAssertionChecker(`ASSERTIONS_STOREBUFFER_COMMIT_NOT_EMPTY, ASSERT_ERROR, assertNode);
    ASSERTION assertSecondaryStoreFound      <- mkAssertionChecker(`ASSERTIONS_STOREBUFFER_SECOND_STORE_FOUND, ASSERT_ERROR, assertNode);

    // ***** Helper Functions ***** //

    // hash

    // The hash is simple truncation for now. 
    // (Last three bits are ignored since the memory is doubleword-aligned.)

    function SB_BUCKET bucketHash(MEM_ADDRESS a) = truncate(a>>3);
    
    // beginInsert
    
    // Start the insertion of an element into a list.

    function Action beginInsert(SB_BUCKET bucket, SB_INDEX new_idx);
    action

        // Get the old head of that bucket.
        SB_INDEX oldHead = bucketHeads.sub(bucket);
        
        // We know that the new index is not null, so we can get the token
        let new_tok = getIndexToken(new_idx);

        if (oldHead matches tagged SB_MAIN .head_tok &&& isOlder(new_tok, head_tok))
        begin

            // The new guy is not the new head. We gotta walk the list.
            debugLog.record($format("insert: TOKEN %0d must search list #%0d (main).", tok.index, bucket));

            // Retrieve the next guy.
            mainListNexts.readReq(head_tok);
            
            // Begin a cleanup of this list
            beginListCleanup(bucket, oldHead, insertCleanupInfo);

            // Stall the pipeline while we search.
            insertState <= tagged INSERT_SEARCH_MAIN new_idx;

        end
        else if (oldHead matches tagged SB_LIST_SECOND {.head_tok, .head_idx} &&& isOlder(new_tok, head_tok))
        begin
            
            // The new guy is not the new head, we gotta find the next guy in the secondary store.
            debugLog.record($format("TOKEN %0d: insert: Must search list #%0d (secondary).", tok.index, bucket));

            // Retrieve the next guy.
            secondaryListNexts.readReq(head_idx);
            
            // Begin a cleanup of this list
            beginListCleanup(bucket, oldHead, insertCleanupInfo);

            // Stall the pipeline as we search.
            insertState <= tagged INSERT_SEARCH_SECONDARY new_idx;
        
        end
        else
        begin
    
            // The new guy is the new head.
            debugLog.record($format("Token %0d: insert: Putting at head of list #%0d.", tok.index, bucket));

            // Update the bucket heads to point at the new guy.
            bucketHeads.upd(bucket, new_idx);

            // The new node points at the old head (if any).
            case (new_idx) matches
                tagged SB_MAIN .new_tok: 
                begin

                    // It's in the main list. 
                    mainListNexts.write(new_tok.index, oldHead);

                end
                tagged SB_SECONDARY {.new_tok, .new_idx2}: 
                begin

                    // It's in the backing store.
                    secondaryListNexts.write(new_idx2, oldHead);

                end
            endcase

            // Unstall the pipeline.
            insertState <= tagged INSERT_NORMAL;

            // Pop the request. End of macro-operation (path 1).
            debugLog.record($format("TOKEN %0d: insert: Finished (path 1).", new_tok.index));
            linkMemState.deq();
        end

    endaction
    endfunction
    
    
    // continueInsert
    
    // Continue searching for a place to put a new store.

    function continueInsert(SB_INDEX new_idx, SB_INDEX next_idx);
    action

        // We know that the new index is not null, so we can get the token
        let new_tok = getIndexToken(new_idx);

        // Does the new guy go here?
        if (next_idx matches tagged SB_MAIN .next_tok &&& isOlder(new_tok, next_tok))
        begin

            // We gotta keep looking for someone older. Do this rule again on the next element in the list.
            debugLog.record($format("TOKEN %0d: insert: Could not put before TOKEN %0d (main).", new_tok.index, next_tok.index));

            // Request the next node to examine.
            mainListNexts.readReq(next_tok);

            // Cleanup the current list node.
            cleanupListNode(next_idx, insertCleanupInfo);

            // Keep searching and take the response from the main list.
            state <= tagged INSERT_SEARCH_MAIN new_idx;

        end
        else if (next_idx matches tagged SB_SECONDARY {.next_tok, .next_idx} &&& isOlder(new_tok, next_tok))
        begin

            // The new guy does not go here. Look in the secondary store for the next node.
            debugLog.record($format("TOKEN %0d: insert: Could not put before TOKEN %0d (secondary).", new_tok.index, next_tok.index));

            // Request the next node to examine.
            secondaryListNexts.readReq(next_idx);

            // Cleanup the current list node.
            cleanupListNode(next_idx, insertCleanupInfo);

            // Keep searching and take the response from the secondary list.
            state <= tagged INSERT_SEARCH_SECONDARY new_idx;

        end
        else
        begin

            // Well it was older than the last guy, so put it here.
            debugLog.record($format("TOKEN %0d: insert: putting before TOKEN %0d.", new_tok.index, next_tok.index));

            // Point the last valid node we've seen at us.
            cleanupListNode(new_idx, insertCleanupInfo);

            // Finish up everything next cycle.
            state <= tagged INSERT_FINISH tuple2(new_idx, next_idx);

        end

    endaction
    endfunction

    // beginListCleanup
    
    // Begin cleaning up a certain list. This basically means recording some info. 

    function Action beginListCleanup(SB_BUCKET b, SB_INDEX oldHead, Reg#(CLEANUP_INFO) scoreboard);
    action

        case (oldHead) matches
            tagged SB_NULL:
            begin
                // The list is empty... this shouldn't happen so this is a don't-care case.
                scoreboard <= CLEANUP_INFO {inGap: True, lastValidIndex: SB_NULL, bucket: b};
            end
            tagged SB_MAIN .tok:
            begin
                if (mainListValids[tok.index])
                begin
                    // The head is Valid, so we are not in a gap.
                    scoreboard <= CLEANUP_INFO {inGap: False, lastValidIndex: oldHead, bucket: b};
                end
                else
                begin
                    // The head is invalid, so we can remove it from the list.
                    scoreboard <= CLEANUP_INFO {inGap: True, lastValidIndex: SB_NULL, bucket: b};
                end
            end
            tagged SB_SECONDARY {.tok, .idx}:
            begin
                if (secondaryListValids[idx])
                begin
                    // The head is valid, so we are not in a gap.
                    scoreboard <= CLEANUP_INFO {inGap: False, lastValidIndex: oldHead, bucket: b};
                end
                else
                begin
                    // The head is invalid, so we can remove it from the list.
                    scoreboard <= CLEANUP_INFO {inGap: True, lastValidIndex: SB_NULL, bucket: b};
                end
            end
        endcase
    
   
    endaction
    endfunction


    // cleanupListNode
    
    // Clean a list up as follows:
    // * If you were in a gap and you see a valid node, remove the gap.
    // * Otherwise just do some book-keeping.
   
    function Action cleanupListNode(SB_INDEX next_idx, Reg#(CLEANUP_INFO) scoreboard);
    action

        CLEANUP_INFO new_info = scoreboard;

        // Clean up the current node
        if (scoreboard.inGap)
        begin

            // We haven't seen a valid node for a while.

            if (indexValid(next_idx))
            begin

                // The next guy is valid, so remove the gap node from the list.
                pointAt(scoreboard.lastValidIndex, next_idx, scoreboard);

                // We are no longer in a gap.
                new_info.inGap = False;

                // The last valid token is this guy.
                new_info.lastValidIndex = next_idx;

            end
            else
            begin

                // The next guy is invalid, so the gap widens.
                noAction;

            end

        end
        else
        begin

            // We are not currently in a gap.

            if (indexValid(next_idx))
            begin
                // The next guy is valid, so we are still not in a gap.
                // We do the pointing anway so that we can use this function for
                // inserting new elements into the list.
                pointAt(scoreboard.lastValidIndex, next_idx, scoreboard);
                new_info.lastValidIndex = next_idx;
            end
            else
            begin
                // The next guy is invalid, so now a gap begins.
                new_info.inGap = True;
            end

        end
       
        scoreboard <= new_info;
    
    endaction
    endfunction

    
    // pointAt
    
    // Re-point the given node at the new target.

    function Action pointAt(SB_INDEX prev, SB_INDEX new_next, Reg#(CLEANUP_INFO) scoreboard);
    action
        
        case (prev) matches

            tagged SB_NULL:
            begin
                // We never saw a valid element, so update the head of the list.
                bucketHeads.upd(scoreboard.bucket, new_next);
            end
            tagged SB_MAIN .tok:
            begin
                mainListNexts.write(tok, new_next);
            end
            tagged SB_SECONDARY {.tok, .idx}:
            begin
                secondaryListNexts.write(idx, new_next);
            end

        endcase
        
    endaction
    endfunction

    // lookupSearch
    
    // Search for the youngest token older than the target.

    function Action lookupSearch(TOKEN cur_tok, Bool node_valid, SB_NODE node, SB_INDEX next, MEM_ADDRESS target_addr, MEM_ADDRESS tok);
    action

        // Is the guy we are examining a potential answer?
        
        // Since the list is sorted by age, the first valid store 
        // to the correct address we encounter which is older than 
        // the target is the best match.
        
        // It is if it is (A) Valid, (B) the correct address, (C) older than the parameter
        // AND (D) the candidate is visible to the context.

        if (node_valid && (node.addr == target_addr) && isOlder(cur_tok, tok) &&
            contextMaskContains(node.mask, tok.ctxt))
        begin
        
            // We found it!
        
            // Log it.
            debugLog.record($format("TOKEN %0d: lookupSearch: Found older matching store in TOKEN %0d (V: 0x%h).", tok.index, cur_tok.index, node.value));

            // We're idle again.
            lookupState <= tagged LOOKUP_NORMAL;

            // Pop the request.
            linkMemState.deq();

            // Respond to mem state. End of macro-operation (path 4).
            debugLog.record($format("TOKEN %0d: lookup: Finished (path 4)", tok.index));
            linkMemState.makeResp(tagged SBUFFER_RSP_LOOKUP {mresult:tagged Valid node.value, addr: target_addr, tok:tok});

        end
        else
        begin
            
            // Haven't found it yet.
            debugLog.record($format("TOKEN %0d: lookupSearch: No match on TOKEN %0d.", tok.index, cur_tok.index)); 
            
            // Try to continue the search.

            case (next) matches
                tagged SB_NULL:
                begin
                  
                    //We've reached the tail. It was a false positive.
                    debugLog.record($format("TOKEN %0d: lookupSearch: Reached list tail. (False positive...)", tok.index));
                  
                    // We're idle again.
                    lookupState <= tagged LOOKUP_NORMAL;

                    // Pop the request.
                    linkMemState.deq();

                    // Respond to mem state. End of macro-operation (path 5).
                    debugLog.record($format("TOKEN %0d: lookup: Finished (path 5).", tok.index));
                    linkMemState.makeResp(tagged SBUFFER_RSP_LOOKUP {mresult:tagged Invalid, addr: addr, tok:tok});

                end
                tagged SB_MAIN .next_tok:
                begin
                
                    // There's more list to search.
                    debugLog.record($format("TOKEN %0d: lookupSearch: Continuing on TOKEN %0d (1)...", tok.index, next_tok.index)); 

                    // Retrieve the values from RAM for the next go.
                    mainListValues.readReq(next_tok.index);
                    mainListNexts.readReq(next_tok.index);

                    // The next becomes the candidate.
                    lookupState <= tagged LOOKUP_SEARCH_MAIN next_tok;

                end
                tagged SB_SECONDARY {.next_tok, .next_idx}:
                begin
                
                    // There's more list to search.
                    debugLog.record($format("TOKEN %0d: lookupSearch: Continuing on TOKEN %0d (2)...", tok.index, next_tok.index)); 

                    // Retrieve the values from RAM for the next go.
                    secondaryListValues.readReq(next_idx);
                    secondaryListNexts.readReq(next_idx);

                    // The next becomes the candidate.
                    lookupState <= tagged LOOKUP_SEARCH_SECONDARY tuple2(next_tok, next_idx);

                end

            endcase
                
        end

    endaction
    endfunction
   


    // ***** Rules ***** //

    // insert

    // N+1+K stage macro operation.
    // Where N is the dynamic length of the list in the bucket which the address hashes to.
    // K is zero for the first store associated with a token, and the time needed to allocate
    // a secondary index in the other case.
        
    // When:   When the mem state requests an INSERT.
    // Effect: Insert a new value into a chosen list sorted by token age.
    // Parameters: MEMSTATE_SBUFFER_REQ (SBUFFER_INSERT)
    // Returns:    N/A
   
   
    // insert

    // When:   When the mem state requests an INSERT.
    // Effect: 1) Update the appropriate values RAM with the new data.
    //         2) Choose a list "bucket" to put the element into.
    //         3) If that list happened to be empty, insert it and we're done (though we may have to find a new backing index).
    //         4) Otherwise we begin a sequential search of that bucket.
    //         (If this was the second store for a token, we put it into the backing store.)

    rule insert (insertState == INSERT_NORMAL &&& linkMemState.getReq() matches tagged SBUFFER_REQ_INSERT {value:.v, addr: .a, tok: .new_tok});
        
        // Hash the address to determine the appropriate "bucket".
        SB_BUCKET bucket = bucketHash(a);

        // Log it.
        debugLog.record($format("TOKEN %0d: insert: Begin. (bucket #%0d)", new_tok.index, bucket));

        // Get the current mask of contexts in that bucket.
        CONTEXT_MASK existing_ctxts = bucketContexts.sub(bucket);

        // Turn on this context's bit in the mask.
        bucketContexts.upd(bucket, addContextToMask(existing_ctxts, new_tok.ctxt));

        // At insertion time only the owner context can see the store.
        CONTEXT_MASK new_mask = addContextToMask(initEmptyContextMask(), new_tok.ctxt);

        // Should the new element become the head of this list?
        // It should if it's younger than the current head, or if there is no current head.
        
        // TODO: Corner case: we can also insert at the head if the current head is gone, and the token we're 
        //       inserting is head-1. This is because head-1 takes the place of head in the list.

        // If there was already one store, then put it into the backing store.
        if (!mainListValids[new_tok.index])
        begin
        
            // We can put it into the main list.
            mainListValues.write(new_tok.index, initMainNode(new_mask, a, v));

            // Update the valid bit
            mainListValids <= update(mainListValids, new_tok.index, True);
            
            // Continue with the insert.
            beginInsert(bucket, tagged SB_MAIN new_tok);

        end
        else
        begin
            
            // It's the second store for this token.
            debugLog.record($format("TOKEN %0d: insert: Putting Store #2 into secondary RAM.", new_tok.index));

            // We'd better have room in the secondary storage.
            assertSecondaryStorageHasRoom(!secondaryListFull);

            // Request a new index in the secondary storage.
            secondaryListValues.write(secondaryListInsertIndex.value(), initSecondaryNode(new_tok.index, new_mask, a, v));

            // Update the valid bit.
            secondaryListValids <= update(secondaryListValids, secondaryListInsertIndex.value(), True);

            // Update the fullness info.
            numInSecondaryList.up();

            // Record that this token has a second store.
            hasSecondStore <= update(hasSecondStore, new_tok.index, True);

            // We now must find a new free space in the backing store.
            let candidate_idx = secondaryListInsertIndex.value() + 1;

            if (secondaryListValids[candidate_idx] && !secondaryListAlmostFull)
            begin

                // We gotta stall to find the next free index.
                insertState <= tagged INSERT_ALLOC_SECONDARY bucket;
                
                // Start searching at the next spot.
                secondaryListInsertIndex.incr(2);
            
            end
            else
            begin
                
                // The next spot was free (or the list was full so it doesn't matter).
                secondaryListInsertIndex.up();

                // Continue with the insert.
                beginInsert(bucket, tagged SB_SECONDARY tuple2(new_tok, secondaryListInsertIndex.value()));
            
            end

        end

    endrule

    // insertAllocateSecondary

    // When:   After insert does an insert and cannot immediately find a free spot in the secondary storage.
    // Effect: Sequentially search the backing store to find a free index. 
   
    rule insertAllocateSecondary (insertState matches tagged INSERT_ALLOC_SECONDARY .bucket 
                                  &&& linkMemState.getReq() matches tagged SBUFFER_REQ_INSERT {value:.v, addr: .a, tok: .new_tok});
    

        if (!secondaryListValids[secondaryListInsertIndex.value()])
        begin
            // We've found a free spot. We can continue with the insert.
            beginInsert(bucket, tagged SB_SECONDARY tuple2(new_tok, secondaryListInsertIndex.value()));
        end
        else
        begin
            // Keep looking at the next spot.
            secondaryListInsertIndex.up();
        end

    endrule

    // insertSearchMain

    // When:   After beginInsert has determined that we need to walk a list and the current node is in the main list.
    // Recurs: Until we find a node older than the current node (and it's in the main RAM).
    // Effect: Check if the new node to be inserted should go between the
    //         current node and the next node. If so, we're done.
    //         Otherwise do the rule again on the next position on the list.

    rule insertSearchMain (insertState matches tagged INSERT_SEARCH_MAIN .new_idx
                           &&& linkMemState.getReq() matches tagged SBUFFER_REQ_INSERT {value:.v, addr: .a, tok: .new_tok});

       // Retrieve the next nodes in the lists.
       // (We will only look at one of these, but doing both keeps the logic clean.)
       let next_idx <- mainListNexts.readRsp();

       continueInsert(new_idx, next_idx);

    endrule

    // insertSearchSecondary

    // When:   After beginInsert has determined that we need to walk a list and the current node is in the secondary list.
    // Recurs: Until we find a node older than the current node (and it's in the secondary RAM).
    // Effect: Check if the new node to be inserted should go between the
    //         "candidate" and the next node. If so, we're done.
    //         Otherwise do the rule again on the next position on the list.

    rule insertSearchSecondary (insertState matches tagged INSERT_SEARCH_SECONDARY .new_idx
                                &&& linkMemState.getReq() matches tagged SBUFFER_REQ_INSERT {value:.v, addr: .a, tok: .new_tok});

        // Retrieve the next nodes in the lists.
        // (We will only look at one of these, but doing both keeps the logic clean.)
        let next_idx <- secondaryListNexts.readRsp();

        continueInsert(new_idx, next_idx);
    
    endrule

    // insertFinish
    
    // When:   After insertSearchMain/Secondary has found the appropriate place in the list.
    // Effect: Point the new guy at the current list tail (if any) and pop the request.

    rule insertFinish (state matches tagged INSERT_FINISH {.new_idx, .tail}
                       &&& linkMemState.getReq() matches tagged SBUFFER_REQ_INSERT {value:.v, addr: .a, tok: .new_tok});
        
        // Point the new guy at the tail, which may or may not be valid.
        pointAt(new_idx, tail, insertCleanupInfo);

        // Log it.
        debugLog.record($format("TOKEN %0d: insert: Finished (path 2).", new_tok.index));
        
        // Unstall the pipeline.
        insertState <= tagged INSERT_NORMAL;
        
        // Pop the request. End of macro-operation (path 2).
        linkMemState.deq();

    endrule


    // commit
    
    // 2-stage macro-operation where the second stage may stall.
    // When: After the mem-state requests a commit.
    // Effect: Invalidate the element(s), and return the value(s).
    //         NOTE: We do _not_ remove the elements from the list here, unless they are the head of their buckets.
    //               Instead it is removed during insert/lookup by the "cleanup" operations.
    // Parameters: MEMSTATE_SBUFFER_REQ (SBUFFER_COMMIT)
    // Returns:    MEMSTATE_SBUFFER_RSP (SBUFFER_RSP_COMMIT) with the value which should be sent to memory.

    // commit1

    // When:   When the mem state requests a commit and we are idle.
    // Effect: Retrieve the location from BlockRAM.

    rule commit1 (linkMemState.getReq() matches tagged SBUFFER_REQ_COMMIT .tok);

        // Get the request. Begin macro-operation.
        linkMemState.deq();

        // There had better be at least one store in the buffer for this token,
        // and that store had better be in location 1.
        assertTokToCommitHasStore(hasLocation1(tok.index));

        // Log it.
        debugLog.record($format("TOKEN %0d: commit: Begin.", tok.index));

        // Retrieve the actual values from the list.
        mainListValues.readReq(tok.index);
        mainListNexts.readReq(tok.index);

        // Pass it to the next stage.
        commitQ.enq(tok);

    endrule

       
    // commit2

    // When:   After a commit1's BRAMs responses.
    // Effect: We invalidate the node's data, but do not remove the node from the list
    //         unless it's the head. This is correct, but may slow down lookups.
    //         They will be removed later by the "cleanup" operations.
    //         If there is a second store we stall to take care of it.

    rule commit2 (commit2State matches tagged COMMIT2_NORMAL);

        // Get the data from the previous stage.
        let tok = commitQ.first();
      
        // Get the responses
        let node   <- listValues.readRsp();
        let m_next <- listNexts.readRsp();
        
        // Hash the address to see which list "bucket" the node was in.
        let bucket = bucketHash(node.addr);

        // If that bucket is empty, something is really wrong.
        assertTokToCommitListNotEmpty(isValid(bucketHeads.sub(bucket)));

        // Although indexes are removed from lists lazily, we can do one optimization: 
        // Remove this guy immmediately if they were the head.
        if (getIndexToken(bucketHeads.sub(bucket)) == tok)
        begin

            // The committed node was the head of the list.
            debugLog.record($format("TOKEN %0d: commit2: Resetting head of list #%0n.", tok.index, bucket));


            // Update the heads to point at the next index (if any).
            bucketHeads.upd(bucket, m_next);

            // We can clear the context bits if the list is now empty.
            // Note that this is the only point context bits are reset.
            // Therefore if lists never empty the hash will be less effective.
            if (m_next matches tagged SB_NULL)
            begin
                bucketContexts.upd(bucket, initEmptyContextMask());
                debugLog.record($format("TOKEN %0d: commit2: Resetting context hash of empty list #%0n.", tok.index, bucket));
            end

        end

        // Invalidate the list data. If it was not removed from 
        // the list in the above if-statement we will get it while walking the lists.
        mainListValids <= update(mainListValids, tok.index, False);

        // Now we may have to take care of a second store.
        if (!hasSecondStore[tok.index])
        begin
        
            // Just one store. We're idle again.
            state <= SB_READY;

            // Pop the request.
            commitQ.deq();

            // Respond with the data. End of macro-operation. (path 1)
            debugLog.record($format("TOKEN %0d: commit: Finished (path 1).", tok.index));
            linkMemState.makeResp(tagged SBUFFER_RSP_COMMIT {addr: node.addr, hasMore: False, value: node.value, tok: tok});

        end
        else
        begin

            // Gotta retrieve the second value.
            debugLog.record($format("TOKEN %0d: commit2: Stalling to find second store.", tok.index));

            // Stall the pipeline.
            commit2State <= tagged COMMIT2_ADDITIONAL 0;
            
            // Search for the values starting at the head of the list.
            secondaryListValues.readReq(0);

            // Respond with the data, but keep going.
            linkMemState.makeResp(tagged SBUFFER_RSP_COMMIT {addr: node.addr, hasMore: True, value: node.value, tok: tok});

        end
        
    endrule
    
    rule commit2Additional (commit2State matches tagged COMMIT2_ADDITIONAL .cur);
    
        // Get the value from the previous stage.
        let tok = commitQ.first();
    
        // Get the current index;
        let node <- secondaryListValues.readRsp();
        
        if (node.tok_index == tok.index)
        begin
        
            // We've found it!
            debugLog.record($format("TOKEN %0d: commit2Additional: Found second store at location #%0d.", tok.index, cur));
            
            // Update the valid bits. The node will be removed lazily.
            hasSecondStore <= update(hasSecondStore, tok.index, False);
            secondaryListValids <= update(secondaryListValids, cur, False);
            numInSecondaryList.down();
            
            // This is now the next free index (this is important if the buffer was full).
            secondaryListInsertIndex.setC(cur);

            // We're idle again.
            commit2State <= COMMIT2_NORMAL;

            // Pop the request.
            commitQ.deq();

            // Respond with the data. End of macro-operation. (path 2)
            debugLog.record($format("TOKEN %0d: commit: Finished (path 2).", tok.index));
            linkMemState.makeResp(tagged SBUFFER_RSP_COMMIT {addr: node.addr, hasMore: False, value: node.value, tok: tok});
     
        end
        else
        begin
            
            // Gotta keep looking...
            debugLog.record($format("TOKEN %0d: commit2Additional: Second store is not at location #%0d.", tok.index, cur));

            // Check the next index.
            let new_cur = cur + 1;
            
            // If we looped around and didn't find it, something is really wrong.
            assertSecondaryStoreFound(new_cur != 0);

            // Request the next values.
            secondaryListValues.readReq(new_cur);
            
            // Stay in this state and do it again.
            commit2State <= tagged COMMIT2_ADDITIONAL new_cur;

        end
     
    endrule
    
    // rewind
    
    // 1-stage macro-operation.
    
    // When:   When the mem state requests a commit, and we are idle.
    // Effect: Invalidate the nodes' data, but don't remove them from the list. (See above).
    // Parameters: MEMSTATE_SBUFFER_REQ
    // Return:     N/A

    rule rewind (linkMemState.getReq() matches tagged SBUFFER_REQ_REWIND .rinfo &&& 
                 rewindState matches tagged REWIND_NORMAL);

        // Log it.
        debugLog.record($format("TOKEN %0d: rewind: begin rewind to %0d (Youngest is %0d)", rinfo.rewind, rinfo.youngest));

        // Track all the tokens which should be pulled down.
        Vector#(NUM_TOKENS, Bool) first_stores = newVector();
        Vector#(NUM_TOKENS, Bool) second_stores = newVector();

        // Combinationally walk all the valid bits, pulling some of them down.
        for (Integer x = 0; x < valueof(NUM_TOKENS); x = x + 1)
        begin
            TOKEN_INDEX cur = fromInteger(x);
            match {.new_fst, .new_snd} = (rinfo.youngest > rinfo.rewind) ? 
                // No overflow
                ((cur > rinfo.rewind) && (cur <= rinfo.youngest) ? tuple2(False, False) : tuple2(hasLocation1[x], hasLocation2[x])) :
                // Overflow
                ((cur > rinfo.rewind) || (cur <= rinfo.youngest) ? tuple2(False, False) : tuple2(hasLocation1[x], hasLocation2[x]));
            // Log the ones which actually change.
            if (new_fst != mainListValids[x])
                debugLog.record($format("TOKEN %0d: rewind: Changing TOKEN %0d Store 1 from %0d to %0d", rinfo.rewind, cur, mainListValids[x], new_fst));
            if (new_snd != hasSecondStore[x])
                debugLog.record($format("TOKEN %0d: rewind: Changing TOKEN %0d Store 2 from %0d to %0d", rinfo.rewind, cur, hasSecondStore[x], new_snd));
            // Update the vectors.
            first_stores[x] = new_fst;
            second_stores[x] = new_snd;
          end

        // Assign the valid bits to their new values.
        mainListValids <= first_stores;
        hasSecondStore <= second_stores;
        
        if (second_stores != hasSecondStore)
        begin
        
            // We're altering the secondary buffering, so we have to stall.
            debugLog.record($format("TOKEN %0d: rewind: stalling to walk secondary buffer.", rinfo.rewind));

            // Stall.
            rewindState <= tagged REWIND_SEARCH_SECONDARY 0;

            // Request the first lookup.
            secondaryListValues.readReq(0);
        
        end
        else
        begin
        
            // No stall. Pop the request. End of macro-operation. (path 1).
            debugLog.record($format("TOKEN %0d: rewind: Finished (path 1).", rinfo.rewind));
            linkMemState.deq();

        end
        

    endrule

    rule rewindSearch (linkMemState.getReq() matches tagged SBUFFER_REQ_REWIND .rinfo &&&
                       rewindState matches tagged REWIND_SEARCH_SECONDARY .cur);

        // Get the current data.
        let node <- secondaryListValues.readRsp();
        
        if (secondaryListValids[cur] && !hasSecondStore[node.tok_index])
        begin
        
            // We rewound this node, so now we must invalidate it.
            debugLog.record($format("TOKEN %0d: rewindSearch: Invalidating second store for TOKEN %0d at location #%0d.", rinfo.rewind, node.token.index, cur));

            // Reclaim the node.
            secondaryListValids[cur] <= False;
            numInSecondaryList.down();
        
            let new_cur = cur + 1;
            
            if (new_cur == 0)
            begin
                
                // We're done.
                debugLog.record($format("TOKEN %0d: rewind: Finished (path 2).", rinfo.rewind));
                // Unstall.
                rewindState <= REWIND_NORMAL;
                
                // Pop the request. End of macro-operation (path 2).
                linkMemstate.deq();

            end
            else
            begin

               // Keep looking. 
               debugLog.record($format("TOKEN %0d: rewindSearch: Not invalidating second store for TOKEN %0d at location #%0d.", rinfo.rewind, node.token.index, cur));

               // Do this again on the next spot.
               rewindState <= tagged REWIND_SEARCH_SECONDARY new_cur;
               
               // Retrieve the value for next time.
               secondaryListValues.readReq(new_cur);

            end
        
        end
        else
        begin
        
            // Not a valid node.
            debugLog.record($format("TOKEN %0d: rewindSearch: Skipping invalid node #%0d.", rinfo.rewind, cur));
        
        end
        

    endrule

    // lookup
    
    // N-stage macro-operation.
    // Where N dynamically varies depending on the number and composition of the list.
    // When:   When the mem state requests a lookup.
    // Effect: Retrieve the youngest store older than the param (if any).
    // Parameters: MEMSTATE_SBUFFER_REQ (SBUFFER_REQ_LOOKUP)
    // Returns:    MEMSTATE_SBUFFER_RSP (SBUFFER_RSP_LOOKUP)

    // lookup1
    
    // When:   When the mem state requests a lookup and we are idle.
    // Effect: Determine which list we should search. 
    //         If the list is empty, or there are no stores from this context in the list
    //         then we can't have it, so we're done. Otherwise stall and keep searching.

    rule lookup (canLookup &&& lookupState matches tagged LOOKUP_NORMAL &&& linkMemState.getReq() matches tagged SBUFFER_REQ_LOOKUP {addr:.addr, tok:.tok});

        debugLog.record("TOKEN %0d: lookup: Begin.", tok.index); 

        // Hash the address to see which list "bucket" we should check.
        let bucket = bucketHash(addr);
 
        let list_head = bucketHeads.sub(bucket);

        case (list_head) matches

            tagged SB_NULL: 
            begin

                // The chosen list is empty, so we must not have it.
                debugLog.record($format("TOKEN %0d: lookup: List #%0d is empty so we don't have it.", tok.index, bucket));

                // Pop the request.
                linkMemState.deq();

                // Make the response. End of macro-operation (path 1).
                debugLog.record($format("TOKEN %0d: lookup: Finished (path 1).", tok.index));
                linkMemState.makeResp(tagged SBUFFER_RSP_LOOKUP {addr: addr, mresult: tagged Invalid, tok:tok});

            end
            tagged SB_MAIN .head_tok:
            begin


                // The chosen list is non-empty, so we may have to walk it.

                // We use a filter to see if there could be any stores from this context in the list.
                if (!contextMaskContains(bucketContexts.sub(bucket), token.ctxt))
                begin

                    // There are definitely not stores visible to this context in
                    // this bucket.
                    debugLog.record($format("TOKEN %0d: lookup: Context filter #%0d says we don't have it.", tok.index, bucket));

                    // Pop the request.
                    linkMemState.deq();

                    // Make the response. End of macro-operation (path 2).
                    debugLog.record($format("TOKEN %0d: lookup: Finished (path 2).", tok.index));
                    linkMemState.makeResp(tagged SBUFFER_RSP_LOOKUP {addr: addr, mresult: tagged Invalid, tok:tok});

                end
                else
                begin

                    // There could be stores visible to this context in this bucket.
                    // We must seach it to find out.
                    debugLog.record($format("TOKEN %0d: lookup: Beginning to search list #%0d.", tok.index, bucket)); 
                                        
                    // Retrieve the list info from RAM.
                    mainListValues.readReq(head_tok);
                    mainListNexts.readReq(head_tok);

                    // Clean the list as we go.
                    beginListCleanup(bucket, list_head, lookupCleanupInfo);

                    // Stall the pipeline as we search.
                    lookupState <= tagged LOOKUP_SEARCH_MAIN head_tok;

                end

            end
            tagged SB_SECONDARY {.head_tok, .head_idx}:
            begin
            

                // The chosen list is non-empty, so we may have to walk it.

                // We use a filter to see if there could be any stores from this context in the list.
                if (!contextMaskContains(bucketContexts.sub(bucket), token.ctxt))
                begin

                    // There are definitely not stores visible to this context in
                    // this bucket.
                    debugLog.record($format("TOKEN %0d: lookup: Context filter #%0d says we don't have it (2).", tok.index, bucket));

                    // Pop the request.
                    linkMemState.deq();

                    // Make the response. End of macro-operation (path 3).
                    debugLog.record($format("TOKEN %0d: lookup: Finished (path 3).", tok.index));
                    linkMemState.makeResp(tagged SBUFFER_RSP_LOOKUP {addr: addr, mresult: tagged Invalid, tok:tok});

                end
                else
                begin

                    // There could be stores visible to this context in this bucket.
                    // We must seach it to find out.
                    debugLog.record($format("TOKEN %0d: lookup: Beginning to search list #%0d (2).", tok.index, bucket)); 
                                        
                    // Retrieve the list info from RAM.
                    secondaryListValues.readReq(head_idx);
                    secondaryListNexts.readReq(head_idx);

                    // Clean the list as we go.
                    beginListCleanup(bucket, list_head, lookupCleanupInfo);

                    // Stall the pipeline as we search.
                    lookupState <= tagged LOOKUP_SEARCH_SECONDARY tuple2(head_tok, head_idx);

                end
            end
        endcase

    endrule

    // lookupSearch
    
    // When:   After a lookup has determined we should search the list.
    // Recurs: Until the youngest store to the same address is found 
    //         or the end of the list is reached.
    // Effect: Search the given list to find the youngest store to the same address.
    //         Note that since the list is sorted by age we can stop once we
    //         find the first store.

    rule lookupSearchMain (lookupState matches tagged LOOKUP_SEARCH_MAIN .cur_tok &&& linkMemState.getReq() matches tagged SBUFFER_REQ_LOOKUP {addr:.addr, tok:.tok});

        // Get the data we requested previously.
        let node <- mainListValues.readRsp();
        let next <- mainListNexts.readRsp();

        // Is the current node valid?
        let node_valid = mainListValids[cur_tok.index];

        // Cleanup the list as we go.
        cleanupListNode(tagged SB_MAIN cur_tok, lookupCleanupInfo);

        lookupSearch(cur_tok, node_valid, node, next, addr, tok);

    endrule
    
    rule lookupSearchSecondary (lookupState matches tagged LOOKUP_SEARCH_SECONDARY {.cur_tok, .cur_idx} &&& linkMemState.getReq() matches tagged SBUFFER_REQ_LOOKUP {addr:.addr, tok:.tok});

        // Get the data we requested previously.
        let node <- secondaryListValues.readRsp();
        let next <- secondaryListNexts.readRsp();

        // Is the current node valid?
        let node_valid = secondaryListValids[cur_idx];

        // Cleanup the list as we go.
        cleanupListNode(tagged SB_SECONDARY tuple2(cur_tok, cur_idx), lookupCleanupInfo);
 
        lookupSearch(cur_tok, node_valid, toMainNode(node), next, addr, tok);

    endrule
    
    rule allocEmpty (True);
    
        link_regstate.deq();
    
    endrule
   
endmodule
