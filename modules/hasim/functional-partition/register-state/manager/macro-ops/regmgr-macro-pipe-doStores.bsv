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
 
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"
  

// ========================================================================
//
//   Internal data structures
//
// ========================================================================

// STORES_INFO
typedef struct
{
    TOKEN token;
    UP_TO_TWO#(MEM_ADDRESS) memAddrs;
    ISA_MEMOP_TYPE opType;
    MEM_OFFSET offset;
    ISA_VALUE storeValue;
}
STORES_INFO
    deriving (Eq, Bits);

// STATE_STORES2
typedef union tagged
{ 
    void        STORES2_NORMAL;
    STORES_INFO STORES2_RMW_RSP;
    STORES_INFO STORES2_SPAN_REQ;
    STORES_INFO STORES2_SPAN_RSP1;
    Tuple2#(STORES_INFO, MEM_VALUE) STORES2_SPAN_RSP2;
    Tuple2#(STORES_INFO, MEM_VALUE) STORES2_SPAN_END;
}
STATE_STORES2
    deriving (Eq, Bits);

typedef enum
{
    PATH_FINISH,
    PATH_RMW_LOAD,
    PATH_SPAN_LOAD1,
    PATH_SPAN_LOAD2,
    PATH_SPAN_STORE1
}
PATH_STORE deriving (Eq, Bits);


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_DoStores#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_MEMORY_QUEUE linkToMem,
    REGSTATE_PHYSICAL_REGS_WRITE_REG prf,
    BROM#(TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalMemAddrs,
    BRAM#(TOKEN_INDEX, ISA_VALUE) tokStoreValue,
    BRAM#(TOKEN_INDEX, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) tokIsStoreCond)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_doStores.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_DO_STORES, 
                       FUNCP_RSP_DO_STORES) linkDoStores <- mkConnection_Server("funcp_doSpeculativeStores");

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

    FIFO#(FUNCP_REQ_DO_STORES) storesSQ <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, Maybe#(ISA_MEMOP_TYPE))) stores2Q <- mkSizedFIFO(valueOf(MAX_FUNCP_INFLIGHT_MEMREFS));
    FIFO#(Tuple4#(TOKEN,
                  Bool,
                  Maybe#(FUNCP_PHYSICAL_REG_INDEX),
                  Bool)) stores3Q <- mkSizedFIFO(valueOf(MAX_FUNCP_INFLIGHT_MEMREFS));
    FIFO#(PATH_STORE) pathQ <- mkSizedFIFO(valueOf(MAX_FUNCP_INFLIGHT_MEMREFS));

    Reg#(STATE_STORES2) stateStores2 <- mkReg(STORES2_NORMAL);

    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    //
    // Sigh --
    //     For reasons unknown, XST 12 and later fail to initialize stateStores2
    //     correctly.  Putting this rule here fixes it.
    //
    Reg#(Bool) xstFirstTimeBugFix <- mkReg(False);

    rule xstBugFix (! xstFirstTimeBugFix);
        // Xilinx XST bug fails to initialize stateStores2 correctly
        xstFirstTimeBugFix <= True;

        stateStores2 <= tagged STORES2_NORMAL;
    endrule


    // ******* doStores ******* //

    // 2-stage macro operation. Stage 2 can stall in two different ways.

    // When:   When the timing model requests it.
    // Effect: Read the effective address and result, do a store to the memory state.
    // Soft Inputs:  Token
    // Soft Returns: Token


    // doStores1

    // When:   When the timing model starts a doStores().
    // Effect: Broke off from doStores1 to meet faster timing.  Prepare to store.

    rule doStoresS (linkDoStores.getReq().token matches .tok &&&
                    state.readyToBegin(tokContextId(tok)) &&&
                    tokScoreboard.canStartStore(tok.index));

        // Get the input from the timing model. Begin macro-operation.
        let req = linkDoStores.getReq();
        linkDoStores.deq();

        // Confirm timing model propagated poison bit correctly
        assertion.poisonBit(tokIsPoisoned(tok) == isValid(tokScoreboard.getFault(tok.index)));

        // If it's not actually a store, it's an exception.
        let isStore = tokScoreboard.isStore(tok.index);
        assertion.instructionIsActuallyAStore(isStore);
        if (!isStore) begin
            debugLog.record(fshow(tok.index) + $format(": BAD STORE")); 
        end

        storesSQ.enq(req);
    endrule

    // doStores1

    // When:   Follows doStoresS.
    // Effect: Lookup the destination of this token.  The predicate remains
    //         readyToBegin() because the scoreboard isn't yet updated.

    (* conservative_implicit_conditions *)
    rule doStores1 (state.readyToBegin(tokContextId(storesSQ.first().token)));

        let req = storesSQ.first();
        let tok = req.token;

        if (tokScoreboard.emulateInstruction(tok.index) ||
            ! tokScoreboard.isAllocated(tok.index, ALLOC_RP_DOSTORES))
        begin
            // Log it.
            debugLog.record(fshow(tok.index) + $format(": DoStores: Ignoring junk token or emulated instruction."));

            storesSQ.deq();

            // Pass to the next stage.
            stores2Q.enq(tuple2(tok, tagged Invalid));
        end
        else if (! tokScoreboard.isStoreDataValid(tok.index))
        begin
            // Store data isn't valid yet -- still waiting for data path.
            // Leave storesSQ at the head of the FIFO and spin.
            noAction;
        end
        else // Everything's fine.
        begin
            // Log it.
            debugLog.record(fshow(tok.index) + $format(": DoStores: Begin.")); 

            storesSQ.deq();

            // Update the scoreboard.
            tokScoreboard.storeStart(tok.index);

            // Read the store value.
            tokStoreValue.readReq(tok.index);

            // Read the effective address(es).
            tokPhysicalMemAddrs.readReq(tok.index);

            // Get the store type.
            let st_type = tokScoreboard.getStoreType(tok.index);
            tokIsStoreCond.readReq(tok.index);

            // Pass to the next stage.
            stores2Q.enq(tuple2(tok, tagged Valid st_type));
        end

    endrule
    

    // doStores2Bypass

    // When:   Handling a dynamically dead store.
    // Effect: Forward token to next stage and do nothing else.

    rule doStores2Bypass (state.readyToContinue() &&&
                    ! isValid(tpl_2(stores2Q.first())) &&&
                    xstFirstTimeBugFix &&&
                    stateStores2 matches tagged STORES2_NORMAL);

        // Read the parameters from the previous stage.
        match {.tok, .m_st_type} = stores2Q.first();
        let st_type = validValue(m_st_type);

        debugLog.record(fshow(tok.index) + $format(": DoStores2Bypass")); 

        stores2Q.deq(); 
        stores3Q.enq(tuple4(tok, False, tagged Invalid, False));
        pathQ.enq(PATH_FINISH);

    endrule


    // doStores2

    // When:   After we get a response from the address RAM.
    // Effect: Read the physical register file and the effective address. 

    rule doStores2 (state.readyToContinue() &&&
                    isValid(tpl_2(stores2Q.first())) &&&
                    xstFirstTimeBugFix &&&
                    stateStores2 matches tagged STORES2_NORMAL);

        // Read the parameters from the previous stage.
        match {.tok, .m_st_type} = stores2Q.first();
        let st_type = validValue(m_st_type);

        // Get the offset.
        let offset = tokScoreboard.getMemOpOffset(tok.index);

        // Get the store value.
        let store_val <- tokStoreValue.readRsp();

        // Get the physical address(es).
        let p_addrs <- tokPhysicalMemAddrs.readRsp();
        
        case (p_addrs) matches
            tagged ONE .p_addr:
            begin

                // There's only one address, but we may still need to do a read-modify-write.

                if (isaStoreRequiresReadModifyWrite(st_type))
                begin

                    // We're doing read-modify-write. Request a load.
                    let m_req = memStateReqLoad(tok, p_addr, False);
                    linkToMem.makeReq(tagged REQ_LOAD m_req);
                    pathQ.enq(PATH_RMW_LOAD);

                    // Log it.
                    debugLog.record(fshow(tok.index) + $format(": DoStores2: Load Req for Read-Modify-Write. (PA: 0x%h).", p_addr)); 

                    // Stall this stage.
                    let store_info = STORES_INFO
                                     {
                                        token: tok,
                                        memAddrs: p_addrs,
                                        opType: st_type,
                                        offset: offset, 
                                        storeValue: store_val
                                     };
                    stateStores2 <= tagged STORES2_RMW_RSP store_info;

                end
                else
                begin

                    // It's a normal store. We're not stalled.
                    stores2Q.deq(); 

                    // Convert the store.
                    let mem_store_value = isaStoreValueToMemValue(store_val, st_type);
                    debugLog.record(fshow(tok.index) + $format(": DoStores2: ISA Store (V: 0x%h, T: %0d, O: %b) = 0x%h", store_val,  pack(st_type), offset, mem_store_value)); 

                    // Store conditional?
                    let m_store_cond_dst_pr <- tokIsStoreCond.readRsp();
                    let is_store_cond = isValid(m_store_cond_dst_pr);

                    // Make the request to the memory state.
                    let m_req = memStateReqStore(tok, p_addr, mem_store_value, is_store_cond);
                    linkToMem.makeReq(tagged REQ_STORE m_req);
                    pathQ.enq(PATH_FINISH);

                    // Log it.
                    debugLog.record(fshow(tok.index) + $format(": DoStores2: Sending Store to Memory (PA: 0x%h, V: 0x%h).", p_addr, mem_store_value)); 

                    // Wait for response from memory
                    stores3Q.enq(tuple4(tok, True, m_store_cond_dst_pr, True));
                end

            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Two addresses means load two values, then modify them, then write them back.
                // Make the first load now.
                let m_req = memStateReqLoad(tok, p_addr1, False);
                linkToMem.makeReq(tagged REQ_LOAD m_req);
                pathQ.enq(PATH_SPAN_LOAD1);

                // Log it.
                debugLog.record(fshow(tok.index) + $format(": DoStores2: Spanning Store Load Req 1 (PA1: 0x%h, PA2: 0x%h).", p_addr1, p_addr2)); 

                // Stall this stage.
                let store_info = STORES_INFO
                                 {
                                    token: tok,
                                    memAddrs: p_addrs,
                                    opType: st_type,
                                    offset: offset,
                                    storeValue: store_val
                                 };
                stateStores2 <= tagged STORES2_SPAN_REQ store_info;


            end
        endcase
        
        
    endrule

    // doStores2RMW
    
    // When:   After a store has stalled to do a read-modify-write and the load has come back.
    // Effect: Do the "modify-write" portion. Unstall the pipeline.
    
    rule doStores2RMW (state.readyToContinue() &&&
                       xstFirstTimeBugFix &&&
                       stateStores2 matches tagged STORES2_RMW_RSP .store_info &&&
                       pathQ.first() == PATH_RMW_LOAD);
    
        // Get the info from the previous stage.
        match {.tok, .m_st_type} = stores2Q.first();
        let st_type = validValue(m_st_type);
        
        // Get the load from memory.
        MEM_VALUE existing_val = linkToMem.getResp();
        linkToMem.deq();
        pathQ.deq();

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoStores2: Got RMW Load Rsp (V: 0x%h).", existing_val)); 

        // Merge the values.
        let new_mem_val = isaStoreValueToMemValueRMW(existing_val, store_info.storeValue, store_info.offset, store_info.opType);
        debugLog.record(fshow(tok.index) + $format(": doStores2: ISA StoreRMW (EV: 0x%h, V: 0x%h, T: %0d, O: %b) = 0x%h", existing_val, store_info.storeValue,  pack(store_info.opType), store_info.offset, new_mem_val)); 

        // Store conditional?
        let m_store_cond_dst_pr <- tokIsStoreCond.readRsp();
        let is_store_cond = isValid(m_store_cond_dst_pr);

        // Write the store to memory.
        let mem_addr = getFirst(store_info.memAddrs);
        let m_req = memStateReqStore(tok, mem_addr, new_mem_val, is_store_cond);
        linkToMem.makeReq(tagged REQ_STORE m_req);
        pathQ.enq(PATH_FINISH);

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoStores2: Sending RMW Store to Memory (PA: 0x%h, V: 0x%h).", mem_addr, new_mem_val)); 

        // Unstall this stage.
        stateStores2 <= tagged STORES2_NORMAL;
        stores2Q.deq();

        // Wait for response from memory
        stores3Q.enq(tuple4(tok, True, m_store_cond_dst_pr, True));
    
    endrule
    
    // doStores2SpanReq
    
    // When:   After a store has stalled to do a spanning load.
    // Effect: Make the second request, then start to wait for responses.
    
    rule doStores2SpanReq (state.readyToContinue() &&
                           xstFirstTimeBugFix &&&
                           stateStores2 matches tagged STORES2_SPAN_REQ .store_info);
    
        // Get the data from the previous stage.
        match {.tok, .m_st_type} = stores2Q.first();
        let st_type = validValue(m_st_type);
    
        // Make the second load request.
        let p_addr2 = getSecondOfTwo(store_info.memAddrs);
        let m_req = memStateReqLoad(tok, p_addr2, False);
        linkToMem.makeReq(tagged REQ_LOAD m_req);
        pathQ.enq(PATH_SPAN_LOAD2);
        
        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoStores2: Spanning Store Load Req 2 (PA2: 0x%h).", p_addr2));

        // Wait for the first response.
        stateStores2 <= tagged STORES2_SPAN_RSP1 store_info;
    
    endrule

    // doStores2SpanRsp1
    
    // When:   After the first load comes back from a spanning store.
    // Effect: Record the intermediate value, wait for the final response.
    
    rule doStores2SpanRsp1 (state.readyToContinue() &&&
                            xstFirstTimeBugFix &&&
                            stateStores2 matches tagged STORES2_SPAN_RSP1 .store_info &&&
                            pathQ.first() == PATH_SPAN_LOAD1);
    
        // Get the value from the previous stage.
        match {.tok, .m_st_type} = stores2Q.first();
        let st_type = validValue(m_st_type);
    
        // Get the first value from memory.
        MEM_VALUE existing_val1 = linkToMem.getResp();
        linkToMem.deq();
        pathQ.deq();
        
        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoStores2: Spanning Store Load Rsp 1 (V: 0x%h).", existing_val1));
        
        // Wait for the second response.
        stateStores2 <= tagged STORES2_SPAN_RSP2 tuple2(store_info, existing_val1);
    
    endrule

    // doStores2SpanRsp2
    
    // When:   After the second load comes back from a spanning store.
    // Effect: Figure out the values and make the first store request.
    
    rule doStores2SpanRsp2 (state.readyToContinue() &&&
                            xstFirstTimeBugFix &&&
                            stateStores2 matches tagged STORES2_SPAN_RSP2 {.store_info, .existing_val1} &&&
                            pathQ.first() == PATH_SPAN_LOAD2);
    
        // Get the value from the previous stage.
        match {.tok, .m_st_type} = stores2Q.first();
        let st_type = validValue(m_st_type);

        // Get the second value from memory.
        MEM_VALUE existing_val2 = linkToMem.getResp();
        linkToMem.deq();
        pathQ.deq();
        
        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoStores2: Spanning Store Load Rsp 2 (V: 0x%h).", existing_val2));
        
        // Use the ISA-provided conversion function.
        match {.new_val1, .new_val2} = isaStoreValueToSpanningMemValues(existing_val1, existing_val2, store_info.storeValue, store_info.offset, store_info.opType);
        debugLog.record(fshow(tok.index) + $format(": DoStores2: ISA StoreSpan (EV1: 0x%h, EV2, 0x%h, V: 0x%h, T: %0d, O: %b) = 0x%h, 0x%h", existing_val1, existing_val2, store_info.storeValue,  pack(store_info.opType), store_info.offset, new_val1, new_val2));

        // Store conditional?
        let m_store_cond_dst_pr = tokIsStoreCond.peek();
        // The store might be conditional, but this model refuses to honor
        // unaligned conditional stores.  The clean-up from failure is too
        // complicated.
        let is_store_cond = False;

        // Make the first store request.
        MEM_ADDRESS p_addr1 = getFirst(store_info.memAddrs);
        let m_req = memStateReqStore(tok, p_addr1, new_val1, is_store_cond);
        linkToMem.makeReq(tagged REQ_STORE m_req);
        pathQ.enq(PATH_SPAN_STORE1);

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoStores2: Spanning Store Req 1 (PA1: V1: 0x%h).", p_addr1, new_val1));

        // Stall to make the second request.
        stateStores2 <= tagged STORES2_SPAN_END tuple2(store_info, new_val2);
    
    endrule

    // doStores2SpanEnd
    
    // When:   After making the first store request for a spanning store.
    // Effect: Make the second store request. Unstall the pipeline.

    rule doStores2SpanEnd (state.readyToContinue() &&&
                           xstFirstTimeBugFix &&&
                           stateStores2 matches tagged STORES2_SPAN_END { .store_info,
                                                                          .new_val2 } &&&
                           pathQ.first() == PATH_SPAN_STORE1);
    
        let tok = store_info.token;
    
        // First store queued?  Success response has meaning only for conditional
        // stores.
        let success = linkToMem.getRespSuccess();
        linkToMem.deq();
        pathQ.deq();

        // Store conditional?
        let m_store_cond_dst_pr <- tokIsStoreCond.readRsp();
        // See comment in doStores2SpanRsp2.  We do not permit unaligned
        // conditional stores.
        let is_store_cond = False;

        // Make the second store.
        let p_addr2 = getSecondOfTwo(store_info.memAddrs);
        let m_req = memStateReqStore(tok, p_addr2, new_val2, is_store_cond);
        linkToMem.makeReq(tagged REQ_STORE m_req);
        pathQ.enq(PATH_FINISH);
        
        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoStores2: Spanning Store Req 2 (PA2: V2: 0x%h).", p_addr2, new_val2));
        
        // Unstall this stage.
        stores2Q.deq();
        stateStores2 <= tagged STORES2_NORMAL;
        
        // Wait for response from memory
        stores3Q.enq(tuple4(tok, True, m_store_cond_dst_pr, success));

    endrule
    
    //
    // doStores3
    //     Wait for response from memory that store was queued and return
    //     message to timing model.
    //
    // The following urgency is necessary for functional correctness.

    rule doStores3 (state.readyToContinue() &&& pathQ.first() == PATH_FINISH);
        
        match {.tok, .active, .m_store_cond_dst_pr, .got_cond_locks} = stores3Q.first();
        stores3Q.deq();
        pathQ.deq();

        Bool success = active;

        if (active)
        begin
            // Is the instruction a store conditional?
            if (m_store_cond_dst_pr matches tagged Valid .dst_pr)
            begin
                // Did the memory system grant the lock for all stores associated
                // with the token?
                Bool have_lock = (got_cond_locks && linkToMem.getRespSuccess());
                success = have_lock;

                if (! have_lock)
                begin
                    tokScoreboard.setStoreSquashed(tok.index);
                end

                prf.write(tok, dst_pr, zeroExtend(pack(have_lock)));
                debugLog.record(fshow(tok.index) + $format(": DoStores: ST_C Writing (PR%0d <= %0d)", dst_pr, pack(have_lock)));
            end

            linkToMem.deq();

            // Update the scoreboard.
            tokScoreboard.storeFinish(tok.index);
        end

        // Return to the timing partition. End of macro-operation (path 1).
        linkDoStores.makeResp(initFuncpRspDoStores(tok, success));
        debugLog.record(fshow(tok.index) + $format(": DoStores: End."));

    endrule

endmodule
