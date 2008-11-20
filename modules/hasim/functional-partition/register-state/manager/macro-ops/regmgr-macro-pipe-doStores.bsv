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


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_DoStores#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_MEMORY_QUEUE linkToMem,
    BRAM_MULTI_READ#(2, TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalMemAddrs,
    BRAM#(TOKEN_INDEX, ISA_VALUE) tokStoreValue)
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
    FIFO#(Tuple2#(TOKEN, ISA_MEMOP_TYPE)) stores1Q <- mkFIFO();
    FIFO#(TOKEN) stores3Q <- mkFIFO();

    Reg#(STATE_STORES2) stateStores2 <- mkReg(STORES2_NORMAL);

    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* doStores ******* //

    // 2-stage macro operation. Stage 2 can stall in two different ways.

    // When:   When the timing model requests it.
    // Effect: Read the effective address and result, do a store to the memory state.
    // Soft Inputs:  Token
    // Soft Returns: Token


    // doStores1

    // When:   When the timing model starts a doStores().
    // Effect: Broke off from doStores1 to meet faster timing.  Prepare to store.

    rule doStoresS (state.readyToBegin());

        // Get the input from the timing model. Begin macro-operation.
        let req = linkDoStores.getReq();
        linkDoStores.deq();
        let tok = req.token;

        // Confirm timing model propagated poison bit correctly
        assertion.poisonBit(tokIsPoisoned(tok) == isValid(tokScoreboard.getFault(tok.index)));

        // If it's not actually a store, it's an exception.
        let isStore = tokScoreboard.isStore(tok.index);
        assertion.instructionIsActuallyAStore(isStore);

        storesSQ.enq(req);

    endrule

    // doStores1

    // When:   Follows doStoresS.
    // Effect: Lookup the destination of this token.  The predicate remains
    //         readyToBegin() because the scoreboard isn't yet updated.

    rule doStores1 (state.readyToBegin());

        let req = storesSQ.first();
        storesSQ.deq();
        let tok = req.token;

        if (tokScoreboard.emulateInstruction(tok.index) ||
            ! tokScoreboard.isAllocated(tok.index))
        begin
            // Log it.
            debugLog.record($format("TOKEN %0d: DoStores: Ignoring junk token or emulated instruction.", tok.index));

            // Respond to the timing model. End of macro-operation.
            linkDoStores.makeResp(initFuncpRspDoStores(tok));
        end
        else // Everything's fine.
        begin
            // Log it.
            debugLog.record($format("TOKEN %0d: DoStores: Begin.", tok.index)); 

            // Update the scoreboard.
            tokScoreboard.storeStart(tok.index);

            // Read the store value.
            tokStoreValue.readReq(tok.index);

            // Read the effective address(es).
            tokPhysicalMemAddrs.readPorts[1].readReq(tok.index);

            // Get the store type.
            let st_type = tokScoreboard.getStoreType(tok.index);

            // Pass to the next stage.
            stores1Q.enq(tuple2(tok, st_type));
        end

    endrule
    
    // doStores2

    // When:   After we get a response from the address RAM.
    // Effect: Read the physical register file and the effective address. 

    rule doStores2 (state.readyToContinue() &&& stateStores2 matches tagged STORES2_NORMAL);

        // Read the parameters from the previous stage.
        match {.tok, .st_type} = stores1Q.first();

        // Get the offset.
        let offset = tokScoreboard.getMemOpOffset(tok.index);

        // Get the store value.
        let store_val <- tokStoreValue.readRsp();

        // Get the physical address(es).
        let p_addrs <- tokPhysicalMemAddrs.readPorts[1].readRsp();
        
        case (p_addrs) matches
            tagged ONE .p_addr:
            begin

                // There's only one address, but we may still need to do a read-modify-write.

                if (isaStoreRequiresReadModifyWrite(st_type))
                begin

                    // We're doing read-modify-write. Request a load.
                    let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr};
                    linkToMem.makeReq(tagged REQ_LOAD m_req);

                    // Log it.
                    debugLog.record($format("TOKEN %0d: DoStores2: Load Req for Read-Modify-Write. (PA: 0x%h).", tok.index, p_addr)); 

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
                    stores1Q.deq(); 

                    // Convert the store.
                    let mem_store_value = isaStoreValueToMemValue(store_val, st_type);
                    debugLog.record($format("TOKEN %0d: DoStores2: ISA Store (V: 0x%h, T: %0d, O: %b) = 0x%h", tok.index, store_val,  pack(st_type), offset, mem_store_value)); 

                    // Make the request to the memory state.
                    let m_req = MEMSTATE_REQ_STORE {tok: tok, addr: p_addr, value: mem_store_value};
                    linkToMem.makeReq(tagged REQ_STORE m_req);

                    // Log it.
                    debugLog.record($format("TOKEN %0d: DoStores2: Sending Store to Memory (PA: 0x%h, V: 0x%h).", tok.index, p_addr, mem_store_value)); 

                    // Wait for response from memory
                    stores3Q.enq(tok);
                end

            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Two addresses means load two values, then modify them, then write them back.
                // Make the first load now.
                let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr1};
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Log it.
                debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Load Req 1 (PA1: 0x%h, PA2: 0x%h).", tok.index, p_addr1, p_addr2)); 

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
    
    rule doStores2RMW (state.readyToContinue() &&& stateStores2 matches tagged STORES2_RMW_RSP .store_info);
    
        // Get the info from the previous stage.
        match {.tok, .st_type} = stores1Q.first();
        
        // Get the load from memory.
        MEM_VALUE existing_val = linkToMem.getResp();
        linkToMem.deq();

        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Got RMW Load Rsp (V: 0x%h).", tok.index, existing_val)); 

        // Merge the values.
        let new_mem_val = isaStoreValueToMemValueRMW(existing_val, store_info.storeValue, store_info.offset, store_info.opType);
        debugLog.record($format("TOKEN %0d: doStores2: ISA StoreRMW (EV: 0x%h, V: 0x%h, T: %0d, O: %b) = 0x%h", tok.index, existing_val, store_info.storeValue,  pack(store_info.opType), store_info.offset, new_mem_val)); 

        // Write the store to memory.
        let mem_addr = getFirst(store_info.memAddrs);
        let m_req = MEMSTATE_REQ_STORE {tok: tok, addr: mem_addr, value: new_mem_val};
        linkToMem.makeReq(tagged REQ_STORE m_req);

        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Sending RMW Store to Memory (PA: 0x%h, V: 0x%h).", tok.index, mem_addr, new_mem_val)); 

        // Unstall this stage.
        stateStores2 <= tagged STORES2_NORMAL;
        stores1Q.deq();

        // Wait for response from memory
        stores3Q.enq(tok);
    
    endrule
    
    // doStores2SpanReq
    
    // When:   After a store has stalled to do a spanning load.
    // Effect: Make the second request, then start to wait for responses.
    
    rule doStores2SpanReq (state.readyToContinue() &&& stateStores2 matches tagged STORES2_SPAN_REQ .store_info);
    
        // Get the data from the previous stage.
        match {.tok, .st_type} = stores1Q.first();
    
        // Make the second load request.
        let p_addr2 = getSecondOfTwo(store_info.memAddrs);
        let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr2};
        linkToMem.makeReq(tagged REQ_LOAD m_req);
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Load Req 2 (PA2: 0x%h).", tok.index, p_addr2));

        // Wait for the first response.
        stateStores2 <= tagged STORES2_SPAN_RSP1 store_info;
    
    endrule

    // doStores2SpanRsp1
    
    // When:   After the first load comes back from a spanning store.
    // Effect: Record the intermediate value, wait for the final response.
    
    rule doStores2SpanRsp1 (state.readyToContinue() &&& stateStores2 matches tagged STORES2_SPAN_RSP1 .store_info);
    
        // Get the value from the previous stage.
        match {.tok, .st_type} = stores1Q.first();
    
        // Get the first value from memory.
        MEM_VALUE existing_val1 = linkToMem.getResp();
        linkToMem.deq();
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Load Rsp 1 (V: 0x%h).", tok.index, existing_val1));
        
        // Wait for the second response.
        stateStores2 <= tagged STORES2_SPAN_RSP2 tuple2(store_info, existing_val1);
    
    endrule

    // doStores2SpanRsp2
    
    // When:   After the second load comes back from a spanning store.
    // Effect: Figure out the values and make the first store request.
    
    rule doStores2SpanRsp2 (state.readyToContinue() &&& stateStores2 matches tagged STORES2_SPAN_RSP2 {.store_info, .existing_val1});
    
        // Get the value from the previous stage.
        match {.tok, .st_type} = stores1Q.first();

        // Get the second value from memory.
        MEM_VALUE existing_val2 = linkToMem.getResp();
        linkToMem.deq();
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Load Rsp 2 (V: 0x%h).", tok.index, existing_val2));
        
        // Use the ISA-provided conversion function.
        match {.new_val1, .new_val2} = isaStoreValueToSpanningMemValues(existing_val1, existing_val2, store_info.storeValue, store_info.offset, store_info.opType);
        debugLog.record($format("TOKEN %0d: DoStores2: ISA StoreSpan (EV1: 0x%h, EV2, 0x%h, V: 0x%h, T: %0d, O: %b) = 0x%h, 0x%h", tok.index, existing_val1, existing_val2, store_info.storeValue,  pack(store_info.opType), store_info.offset, new_val1, new_val2));

        // Make the first store request.
        MEM_ADDRESS p_addr1 = getFirst(store_info.memAddrs);
        let m_req = MEMSTATE_REQ_STORE {tok: tok, addr: p_addr1, value: new_val1};
        linkToMem.makeReq(tagged REQ_STORE m_req);

        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Req 1 (PA1: V1: 0x%h).", tok.index, p_addr1, new_val1));

        // Stall to make the second request.
        stateStores2 <= tagged STORES2_SPAN_END tuple2(store_info, new_val2);
    
    endrule

    // doStores2SpanEnd
    
    // When:   After making the first store request for a spanning store.
    // Effect: Make the second store request. Unstall the pipeline.

    rule doStores2SpanEnd (state.readyToContinue() &&& stateStores2 matches tagged STORES2_SPAN_END {.store_info, .new_val2});
    
        let tok = store_info.token;
    
        // First store queued?
        linkToMem.deq();

        // Make the second store.
        let p_addr2 = getSecondOfTwo(store_info.memAddrs);
        let m_req = MEMSTATE_REQ_STORE {tok: tok, addr: p_addr2, value: new_val2};
        linkToMem.makeReq(tagged REQ_STORE m_req);
        
        // Log it.
        debugLog.record($format("TOKEN %0d: DoStores2: Spanning Store Req 2 (PA2: V2: 0x%h).", tok.index, p_addr2, new_val2));
        
        // Unstall this stage.
        stores1Q.deq();
        stateStores2 <= tagged STORES2_NORMAL;
        
        // Wait for response from memory
        stores3Q.enq(tok);

    endrule
    
    //
    // doStores3
    //     Wait for response from memory that store was queued and return
    //     message to timing model.
    //
    rule doStores3 (state.readyToContinue());
        
        let tok = stores3Q.first();
        stores3Q.deq();

        // Wait for memory
        linkToMem.deq();

        // Update the scoreboard.
        tokScoreboard.storeFinish(tok.index);

        // Return to the timing partition. End of macro-operation (path 1).
        linkDoStores.makeResp(initFuncpRspDoStores(tok));
        debugLog.record($format("TOKEN %0d: DoStores: End.", tok.index));

    endrule

endmodule
