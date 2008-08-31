
// memstate_storebuffer_bucket_hash

// A store buffer which uses a bucket-hash scheme.
// This version sequentializes all operations.
// This acts as a giant lock on all the lists.
// Making this locking finer-grained could increase parallelism.

// Library imports

import Vector::*;
import FIFO::*;

// Project foundation imports

`include "fpga_components.bsh"
`include "hasim_common.bsh"
`include "soft_connections.bsh"
`include "funcp_memory.bsh"


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

// SBUFFER_STATE

// The state the store buffer is currently in.

typedef enum
{
    SB_INITIALIZING,
    SB_READY,
    SB_INSERTING,
    SB_FINISHING_INSERT,
    SB_SEARCHING,
    SB_COMMITTING
}
    SBUFFER_STATE
        deriving 
            (Eq, Bits);


// MEM_ADDRESS_HASH

// A convenience.

typedef Bit#(`SBUFFER_HASH_BITS) MEM_ADDRESS_HASH;

// mkFUNCP_StoreBuffer

// A version of the store-buffer using bucket hashes which sequentializes all operations.

module [HASIM_MODULE] mkFUNCP_StoreBuffer
    // interface:
        ();

    // ***** Local State ***** //

    // Valid bits into the RAM.
    Reg#(Vector#(NUM_TOKENS, Bool)) hasFirst  <- mkReg(Vector::replicate(False));
    Reg#(Vector#(NUM_TOKENS, Bool)) hasSecond <- mkReg(Vector::replicate(False));

    // We will use RAM as a bucket-hashed linked-list of nodes.

    // The "next" pointer in the list. IE token 2 is followed by token 10.
    // Invalid indicates end of list.

    BRAM#(TOKEN_INDEX, Maybe#(TOKEN)) firstNexts <- mkBRAMInitialized(tagged Invalid);
    BRAM#(TOKEN_INDEX, Maybe#(TOKEN)) secondNexts <- mkBRAMInitialized(tagged Invalid);

    // The actual values in the list are (Addr, Value) pairs.

    BRAM#(TOKEN_INDEX, Tuple2#(MEM_ADDRESS, MEM_VALUE)) firstStores   <- mkBRAMInitialized(0);
    BRAM#(TOKEN_INDEX, Tuple2#(MEM_ADDRESS, MEM_VALUE)) secondstores  <- mkBRAMInitialized(0);

    // The bucket hash divides things into N lists. This memory stores the heads of
    // these lists. Invalid indicates empty list.

    LUTRAM#(MEM_ADDRESS_HASH, Maybe#(NODE_IDX)) heads <- mkLUTRAMU();

    // Intermediate state for list operations.

    // The place in the list we should examine next.

    Reg#(TOKEN) candidate <- mkRegU();

    // The last valid token we have observed. Used to rearrange the list as we go.

    Reg#(Maybe#(TOKEN)) last_valid_tok <- mkRegU();

    // The current bucket we are looking into.

    Reg#(MEM_ADDRESS_HASH) cur_list <- mkRegU();

    // The best result we have found so far (if any).

    Reg#(Maybe#(Tuple2#(TOKEN, MEM_VALUE))) best_so_far <- mkReg(Invalid);

    // What state is the store buffer in?

    Reg#(SBUFFER_STATE) state <- mkReg(SB_INITIALIZING);


    // State for initialization.

    // Are we initializing?

    Reg#(Bool) initializing <- mkReg(True);

    // What's the current token we've reset?

    Reg#(TOKEN_INDEX) cur <- mkReg(0);

    // Debugging state.

    // Debug log file pointer.
    let debug_log <- mkReg(InvalidFile);

    // FPGA clock cycle.

    Reg#(Bit#(32)) fpga_cc <- mkReg(0);

    // ***** Soft Connections ***** //

    // Link to the mem state.
    Connection_Server#(TOKEN_INDEX, VOID)  link_regstate <- mkConnection_Server("storeBufferAllocate");
    Connection_Server#(MEMSTATE_SBUFFER_REQ, MEMSTATE_SBUFFER_RSP)  link_memstate <- mkConnection_Server("mem_storebuf");

    // ***** Helper Functions ***** //

    // hash

    // The hash is simple truncation for now. 
    // (Last two bits are ignored since the memory is word-aligned.)

    function MEM_ADDRESS_HASH hash(MEM_ADDRESS a) = truncate(a>>2);

    // isBetter

    // This function determines if a new candidate is a "better" match
    // than the existing candidate.
    // A token is better if it is younger and of the same epoch.

    function Bool isBetter(TOKEN t, Maybe#(Tuple2#(TOKEN, MEM_VALUE)) mt);

        case (mt) matches
          tagged Invalid: // Certainly not better.
            return True;
          tagged Valid {.old_t, .v}: // Potentially better.
            return isOlder(old_t.index, t.index) && (t.timep_info.epoch == old_t.timep_info.epoch);
        endcase

    endfunction

    // List Cleanup Functions.
    
    // We only pull down valid bits on list deletion (to speed this operation up).
    // Because of this we can have dead elements sitting in the lists, which is
    // harmless but can slow down lookups. Therefore we remove these dead elements
    // from the list as we go.

    // beginListCleanup

    // A helper function which records that we are entering a mode where we are
    // cleaning up the lists by removing dead elements.

    function Action beginListCleanup(MEM_ADDRESS_HASH list);
    action

        // Record the current bucket.
        cur_list <= list;

        // Reset the state recording we have not yet seen a valid element.
        last_valid_tok <= tagged Invalid;

    endaction
    endfunction

    // cleanupListNode

    // Check if the given node needs to be cleaned up.

    function Action cleanupListNode(TOKEN tok);
    action

        if (tvalids[tok.index]) // We're at a valid node in the list
        begin

            case (last_valid_tok) matches
              tagged Invalid:  // We haven't seen a valid node yet, so this guy is the head of the list.
              begin
              
                  // Update the head to point to this guy.
                  heads.upd(cur_list, tagged Valid tok);
                  // Log it.
                  $fdisplay(debug_log, "[%d]: Clean-up #%0d: setting TOKEN %0d to be head.", fpga_cc, cur_list, tok.index); 
              
              end
              tagged Valid .prv:  // The last valid guy now should point at this guy (it might already, but this is harmless).
              begin

                  // Point the last valid node at this guy.
                  listnodes.write(prv.index, tagged Valid tok);
                  // Log it.
                  $fdisplay(debug_log, "[%d]: Clean-up #%0d: pointing TOKEN %0d at TOKEN %0d.", fpga_cc, cur_list, prv.index, tok.index); 
              end
            endcase

            // This node is the new latest valid node.
            last_valid_tok <= tagged Valid tok;
            // Log it.
            $fdisplay(debug_log, "[%d]: Clean-up #%0d: TOKEN %0d is last-known good node.", fpga_cc, cur_list, candidate.index); 

        end
        else  // This guy was invalid and will be removed when we do find a valid guy (or hit the end of the list).
        begin
          // Log it.
          $fdisplay(debug_log, "[%d]: Clean-up #%0d: Removing invalid TOKEN %0d from list.", fpga_cc, cur_list, candidate.index); 
        end

    endaction
    endfunction

    // finishListCleanup
    
    // We've reached the end, so finish cleanup.

    function Action finishListCleanup(TOKEN listtail);
    action

        if (tvalids[listtail.index]) // The final node is valid.
        begin

            case (last_valid_tok) matches
              tagged Invalid: // The tail is actually the only element in the list!
              begin

                  // Set the tail to be the new head.
                  heads.upd(cur_list, tagged Valid listtail);
                  // Log it.
                  $fdisplay(debug_log, "[%d]: Clean-up #%0d (Finished): setting TOKEN %0d to be head.", fpga_cc, cur_list, listtail.index); 

              end
              tagged Valid .prv: // We did see another element at some point.
              begin

                  // Set the last valid guy to point at the tail.
                  listnodes.write(prv.index, tagged Valid listtail);
                  // Log it.
                  $fdisplay(debug_log, "[%d]: Clean-up #%0d (Finished): pointing TOKEN %0d at TOKEN %0d.", fpga_cc, cur_list, prv.index, listtail.index); 

              end
            endcase

        end
        else // The list tail is invalid.
        begin

            case (last_valid_tok) matches
              tagged Invalid:  // We never saw a valid node, so the list is empty!
              begin
                  // Empty the list.
                  heads.upd(cur_list, tagged Invalid);
                  // Log it.
                  $fdisplay(debug_log, "[%d]: Clean-up #%0d (Finished): Setting to empty.", fpga_cc, cur_list); 
              end
              tagged Valid .t: // The last valid node should actually be the tail.
              begin
                // Update its pointer.
                listnodes.write(t.index, tagged Invalid);
                // Log it.
                $fdisplay(debug_log, "[%d]: Clean-up #%0d (Finished): Setting %0d to be the tail of the list.", fpga_cc, cur_list, t.index); 
              end
            endcase

        end

    endaction
    endfunction

    // ***** Rules ***** //
    
    // currentCC
    
    // When:   All the time.
    // Effect: Update the fpga_cc for debugging purposes.

    rule currentCC (True);

        fpga_cc <= fpga_cc + 1;

    endrule

    // initialize
    
    // When:   Only at the beginning of time.
    // Effect: Reset all lists to empty.
    //         Also open the debug log (first time only).

    rule initialize (state == SB_INITIALIZING);

        // Open the debug log. (After the first time through this will not be invalid.)

        if (debug_log == InvalidFile)
        begin

          let fd <- $fopen("funcp_storebuff.out", "w");

          if (fd == InvalidFile)
          begin
            $display("Error opening FUNCP logfile funcp_storebuff.out");
            $finish(1);
          end

          debug_log <= fd;

        end

        // Foreach x in list RAMs, x := Invalid.

        listnodes.write(cur, tagged Invalid);
        heads.upd(truncate(cur), tagged Invalid);

        let newcur = cur + 1;

        cur <= newcur;

        if (newcur == 0) // Done!
          state <= SB_READY;

    endrule

    // insert

    // N+1 stage macro operation.
    // Where N is the dynamic length of the list.
        
    // When:   When the mem state requests an INSERT.
    // Effect: Insert a new value into a chosen list sorted by token age.
    // Parameters: MEMSTATE_SBUFFER_REQ (SBUFFER_INSERT)
    // Returns:    N/A
   
    // insert1

    // When:   When the mem state requests an INSERT and we are idle.
    // Effect: 1) Update the values RAM with the new data.
    //         2) Choose a list "bucket" to put the element into.
    //         3) If that list happened to be empty, insert it and we're done.
    //         4) Otherwise we 
   

    rule insert1 (state == SB_READY &&& link_memstate.getReq() matches tagged SBUFFER_REQ_INSERT {value:.v, addr: .a, tok: .tok});
        
        // Update the values ram to point this token at this addr/value.

        
        if (hasFirst[tok.index])
        begin
        
            secondStores.write(tok.index, tuple2(a, v));
            hasSecond <= update(hasSecond, tok.index, True);

        end
        else
        begin
        
            firstStores.write(tok.index, tuple2(a, v));
            hasFirst <= update(hasFirst, tok.index, True);

        end

        // Hash the address to determine the appropriate "bucket"
        let h = hash(a);
        
        // Get the old head of that bucket.
        
        let oldHead = heads.sub(h);
        
        // Should the new element become the head of this list?
        
        let insertAtHead = case (oldHead) matches
            tagged Invalid: True; // The list was empty, so yes.
            tagged Valid .t2: return isOlder(t2.index, tok.index); // Sort by age.
          endcase;


        if (insertAtHead) // It's the new head.
        begin

            // Log it.
            $fdisplay(debug_log, "[%d]: Inserting TOKEN %0d at head of list #%0d", fpga_cc, tok.index, h);
            // The new node points at the old head (if any).
            listnodes.write(tok.index, oldHead);
            // Update the heads.
            heads.upd(h, tagged Valid tok);
            // Pop the request. End of macro-operation (path 1).
            link_memstate.deq();

        end
        else  // Not the new head. We gotta walk the list.
        begin

            // Log it.
            $fdisplay(debug_log, "[%d]: TOKEN %0d must search list #%0d", fpga_cc, tok.index, h);
            // We are no longer idle.
            state <= SB_INSERTING;
            // Start retrieving the list.
            listnodes.readReq(validValue(oldHead).index);
            // The position after oldHead is the first position we will examine.
            candidate <= validValue(oldHead);
            // We should also clean up the list as we go.
            beginListCleanup(h);

        end

    endrule

    // insert2Walk

    // When:   After insert1 has determined that we need to walk a list, 
    //         or after insert2Walk when we haven't found the right place yet.
    // Effect: Check if the new node to be inserted should go between the
    //         "candidate" and the next node. If so, we're done.
    //         Otherwise do the rule again on the next position on the list.

    rule insert2Walk (state == SB_INSERTING &&& link_memstate.getReq() matches tagged SBUFFER_REQ_INSERT {value:.v, addr: .a, tok: .tok});

       // Retrieve the next node to be examined. (May be invalid).
       let mnext <- listnodes.readRsp();

       // Is the guy to be inserted younger than the next node in the list?

       let isYoungerThanNext = case (mnext) matches 
                                    tagged Invalid: True; // There is no next node, so yes!
                                    tagged Valid .t2: isOlder(t2.index, tok.index); // Actually check.
                                  endcase;

       if (isYoungerThanNext) // Well it was older than the last guy, so put it here.
       begin

           // Log it.
           $fdisplay(debug_log, "[%d]: Inserting TOKEN %0d after TOKEN %0d", fpga_cc, tok.index, candidate.index);
           // Set the new guy's "next" pointer to point at the next guy.
           listnodes.write(tok.index, mnext);
           // The new token is now valid.
           tvalids <= update(tvalids, tok.index, True);
           // Finish up everything next cycle.
           state <= SB_FINISHING_INSERT;
         
       end
       else // Well, we gotta keep looking. Do this rule again on the next element in the list.
       begin

           // Log it.
           $fdisplay(debug_log, "[%d]: Could not insert TOKEN %0d after TOKEN %0d", fpga_cc, tok.index, candidate.index);
           // Clean up the current node.
           cleanupListNode(candidate);
           // Request the next node to examine.
           listnodes.readReq(validValue(mnext).index);
           // The node we were just examining becomes the candidate.
           candidate <= validValue(mnext);

       end

    endrule

    // insert3
    
    // When:   After insert2Walk has found the appropriate place in the list.
    // Effect: Finish the list cleanup, and pop the request.

    rule insert3 (state == SB_FINISHING_INSERT &&& link_memstate.getReq() matches tagged SBUFFER_REQ_INSERT {value:.v, addr: .a, tok: .tok});

        // Finish the cleanup.
        finishListCleanup(tok);

        // Log it.
        $fdisplay(debug_log, "[%d]: Insert Finished", fpga_cc);
        
        // We're idle again.
        state <= SB_READY;

        // Pop the request. End of macro-operation (path 2).
        link_memstate.deq();

    endrule

    // commit
    
    // 2-stage macro-operation.
    // When: After the mem-state requests a commit.
    // Effect: Invalidate the element, and return the value.
    //         NOTE: We do _not_ remove the element from the list here, unless they are the head of the list.
    //               Instead it is removed by the "cleanup" operations.
    // Parameters: MEMSTATE_SBUFFER_REQ (SBUFFER_COMMIT)
    // Returns:    MEMSTATE_SBUFFER_RSP (SBUFFER_RSP_COMMIT) with the value which should be sent to memory.

    // commit1

    // When:   When the mem state requests a commit and we are idle.
    // Effect: Retrieve the info from RAM.

    rule commit1 (state == SB_READY &&& link_memstate.getReq() matches tagged SBUFFER_REQ_COMMIT .tok);

        // Retrieve the values.
        firstStores.readReq(tok.index);
        listnodes.readReq(tok.index);
        // We are no longer idle.
        state <= SB_COMMITTING;

    endrule

    // commit2

    // When:   After a commit1.
    // Effect: We invalidate the node's data, but do not remove the node from the list
    //         unless it's the head. This is correct, but may slow down lookups.
    //         They will be removed later by the "cleanup" operations.

    rule commit2 (state == SB_COMMITTING &&& link_memstate.getReq() matches tagged SBUFFER_REQ_COMMIT .tok);
      
        // Log it.
        $fdisplay(debug_log, "[%d]: Committing TOKEN %0d", fpga_cc, tok.index);

        // Get the responses.
        match {.a1, .v1} <- firstStores.readRsp();
        let nextNode <- listnodes.readRsp();
        
        // Hash the address to see which list "bucket" the node was in.
        let h = hash(a1);

        case (heads.sub(h)) matches
          tagged Invalid: // This would be very bad, but has never happened in simulation.
          begin
              
              $display("[%d]: ERROR: FUNCP: Store Buffer: Committing TOKEN %0d which was not in a list.", fpga_cc, tok.index);
              $finish(1);

          end
          tagged Valid .t2:
          begin

              if (tok.index == t2.index) // The committed node was the head of the list.
              begin
                  // Update the heads to point at the next node.
                  heads.upd(h, nextNode);
                  // Log it.
                  $fdisplay(debug_log, "[%d]: Resetting head of list!", fpga_cc);
              end

          end
        endcase

        // Invalidate the committed stores.
        hasFirst <= update(hasFirst, tok.index, False);
        if (!hasSecond[tok.index])
        begin
        
            // We're idle again.
            state <= SB_READY;

            // Pop the request.
            link_memstate.deq();

            // Respond with the data. End of macro-operation. (path 1)
            link_memstate.makeResp(tagged SBUFFER_RSP_COMMIT {addr: a1, hasMore: False, value: v1, tok: tok});

        end
        else
        begin
            // Gotta retrieve the second value.
            state <= SB_COMMITTING2;
            secondStores.readReq(tok.index);

            // Respond with the data, but keep going.
            link_memstate.makeResp(tagged SBUFFER_RSP_COMMIT {addr: a1, hasMore: True, value: v1, tok: tok});

        end
        
    endrule
    
    rule commit2Additional (state == SB_COMMITTING2 &&& link_memstate.getReq() matches tagged SBUFFER_REQ_COMMIT .tok);
    
        // We're idle again.
        state <= SB_READY;

        // Pop the request.
        link_memstate.deq();

        // Respond with the data. End of macro-operation. (path 2)
        link_memstate.makeResp(tagged SBUFFER_RSP_COMMIT {addr: second_addr, hasMore: False, value: second_value, tok: tok});
    
    endrule
    
    // rewind
    
    // 1-stage macro-operation.
    
    // When:   When the mem state requests a commit, and we are idle.
    // Effect: Invalidate the nodes' data, but don't remove them from the list. (See above).
    // Parameters: MEMSTATE_SBUFFER_REQ
    // Return:     N/A

    rule rewind (state == SB_READY &&& link_memstate.getReq() matches tagged SBUFFER_REQ_REWIND .rinfo);

        // Log it.
        $fdisplay(debug_log, "[%d]: Rewinding to %0d (Youngest is %0d)", fpga_cc, rinfo.rewind, rinfo.youngest);

        // A vector to track all the tokens which should be pulled down.
        Vector#(NUM_TOKENS, Bool) as = newVector();

        // Walk all the valid bits, pulling some of them down.
        for (Integer x = 0; x < valueof(NUM_TOKENS); x = x + 1)
        begin
            TOKEN_INDEX cur = fromInteger(x);
            let newval = (rinfo.youngest > rinfo.rewind) ? 
                       // No overflow
                       ((cur > rinfo.rewind) && (cur <= rinfo.youngest) ? False : tvalids[x]) :
                       // Overflow
                       ((cur > rinfo.rewind) || (cur <= rinfo.youngest) ? False : tvalids[x]);
            // Log the ones which actually change.
            if (newval != tvalids[x])
                $fdisplay(debug_log, "[%d]: Changing TOKEN %0d from %0d to %0d", fpga_cc, cur, tvalids[x], newval);
            // Update the vector.
            as[x] = newval;
          end

        // Assign the valid bits to their new values.
        tvalids <= as;
        
        // Pop the request. End of macro-operation.
        link_memstate.deq(); 

    endrule

    // lookup
    
    // N-stage macro-operation.
    // Where N dynamically varies depending on the number and composition of the list.
    // When:   When the mem state requests a lookup.
    // Effect: Retrieve the oldest store younger than the param (if any).
    // Parameters: MEMSTATE_SBUFFER_REQ (SBUFFER_REQ_LOOKUP)
    // Returns:    MEMSTATE_SBUFFER_RSP (SBUFFER_RSP_LOOKUP)

    // lookup1
    
    // When:   When the mem state requests a lookup and we are idle.
    // Effect: Determine which list we should search. 
    //         If the list is empty, we can't have it, so we're done.
    //         Otherwise send things on to lookup2Walk.

    rule lookup1 (state == SB_READY &&& link_memstate.getReq() matches tagged SBUFFER_REQ_LOOKUP {addr:.addr, tok:.tok});

        // Hash the address to see which list "bucket" we should check.
        let h = hash(addr);

        case (heads.sub(h)) matches
          tagged Invalid: // The chosen list is empty, so we must not have it.
          begin
              // Log it.
              $fdisplay(debug_log, "[%d]: Looking up %0d: Hash says we don't have it!", fpga_cc, tok.index);
              // Pop the request.
              link_memstate.deq();
              // Make the response. End of macro-operation (path 1).
              link_memstate.makeResp(tagged SBUFFER_RSP_LOOKUP {addr: addr, mresult: tagged Invalid, tok:tok});
          end
          tagged Valid .c: // The choen list is non-empty, so we have to walk it.
          begin
              // Log it.
              $fdisplay(debug_log, "[%d]: Begining lookup of TOKEN %0d in list #%0d", fpga_cc, tok.index, hash(addr)); 
              // Retrieve the list info from RAM.
              listvalues.readReq(c.index);
              listnodes.readReq(c.index);
              // Record the candidate.
              candidate <= c;
              // We haven't found a best match yet.
              best_so_far <= tagged Invalid;
              // We are no longer idle.
              state <= SB_SEARCHING;
              // Begin a cleanup as we go!
              beginListCleanup(h);
          end
        endcase

    endrule

    // lookup2Walk
    
    // When:   After a lookup1 has happened on a non-empty list, OR
    //         after a lookup2Walk has not succesfully found the data.
    // Effect:  

    rule lookup2Walk (state == SB_SEARCHING &&& link_memstate.getReq() matches tagged SBUFFER_REQ_LOOKUP {addr:.addr, tok:.tok});

        // Get the data we requested previously.
        match {.a, .v} <- listvalues.readRsp();
        let mnext_tok <- listnodes.readRsp();

        // Is the guy we are examining the new best potential answer?
        
        // It is if it is (A) Valid, (B) the correct address, (C) older than the parameter
        // AND (D) better than the previous best match.

        let newbest = (tvalids[candidate.index] 
                       && (a == addr) 
                       && isOlder(candidate.index, tok.index) 
                       && isBetter(candidate, best_so_far)) 
                            ? tagged Valid tuple2(candidate, v)  // The new guy is better.
                            : best_so_far;                       // The old guy is better.

        // Log the result of all this.
        if (newbest != best_so_far)
           $fdisplay(debug_log, "[%d]: TOKEN %0d is the new best match.", fpga_cc, candidate.index);

        // Examine the next position in the list.

        case (mnext_tok) matches
          tagged Invalid: //We've reached the tail. Whether or not we've found anything, we're done
          begin
              
              // Marshall up the final result.
              let finalres  = case (newbest) matches
                                tagged Invalid: tagged Invalid;
                                tagged Valid {.tk, .v}: tagged Valid v;
                              endcase;

              // Log it.
              $fwrite(debug_log, "[%d]: Hit the end of list.", fpga_cc); 

              // Log our answer.
              if (isValid(finalres))
                $fwrite(debug_log, "A success!\n");
              else
                $fwrite(debug_log, "A false positive...\n");

              // Finish our cleanup.
              finishListCleanup(candidate);
              
              // We're idle again.
              state <= SB_READY;

              // Pop the request.
              link_memstate.deq();

              // Respond to mem state. End of macro-operation (path 2).
              link_memstate.makeResp(tagged SBUFFER_RSP_LOOKUP {mresult:finalres, addr: addr, tok:tok});

          end
        tagged Valid .next_tok: // More to the list. Keep looking for a better match.
          begin
          
              // This would be really bad. Luckily it's never been observed in the wild.
              if (next_tok.index == candidate.index)
              begin
                $display("ERROR: FUNCP: Store Buffer: Infinite loop in list on TOKEN %0d (MEM_ADDRESS: 0x%h, MEM_VALUE: 0x%h)", candidate.index, a, v);
                $finish(1);
              end

              // Log it.
              $fdisplay(debug_log, "[%d]: Still more list to go.", fpga_cc); 

              // Cleanup the current node.
              cleanupListNode(candidate);

              // Retrieve the values from RAM for the next go.
              listvalues.readReq(next_tok.index);
              listnodes.readReq(next_tok.index);
              
              // The next becomes the candiate.
              candidate <= next_tok;
              
              // Record all the work we did.
              best_so_far <= newbest;
              
          end
        endcase

    endrule
   
    rule allocEmpty (True);
    
        link_regstate.deq();
    
    endrule
   
endmodule
