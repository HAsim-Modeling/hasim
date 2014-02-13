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

// STATE_DTRANS2
typedef union tagged
{
    void        DTRANS2_NORMAL;
    ISA_ADDRESS DTRANS2_SPAN_REQ;
}
STATE_DTRANS2
    deriving (Eq, Bits);


// STATE_DTRANS3
typedef union tagged
{
    void        DTRANS3_NORMAL;
    struct
    {
        MEM_ADDRESS firstPA;
        Bool firstRefFaulted;
    }
    DTRANS3_SPAN_RSP;
}
STATE_DTRANS3
    deriving (Eq, Bits);


// DTRANS_INFO
typedef union tagged
{
    TOKEN DTRANS_NORMAL;
    TOKEN DTRANS_SPAN;
}
DTRANS_INFO
    deriving (Eq, Bits);

function TOKEN getDTransToken(DTRANS_INFO i);

    case (i) matches
        tagged DTRANS_NORMAL .tok: return tok;
        tagged DTRANS_SPAN   .tok: return tok;
    endcase

endfunction


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_DoDTranslate#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_TLB_TRANSLATE link_dtlb_trans,
    BROM#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr,
    BRAM_MULTI_READ#(2, TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalMemAddrs)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_doDTranslate.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(Maybe#(FUNCP_REQ_DO_DTRANSLATE),
                       FUNCP_RSP_DO_DTRANSLATE) linkDoDTranslate <-
        mkFUNCPInterfaceServer("funcp_doDTranslate");


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

    FIFO#(Maybe#(Tuple2#(TOKEN, ISA_MEMOP_TYPE))) dTrans1Q <- mkFIFO();
    FIFO#(DTRANS_INFO) dTrans2Q <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, UP_TO_TWO#(MEM_ADDRESS))) dTransSaveTransQ <- mkLFIFO();

    Reg#(STATE_DTRANS2) stateDTrans2 <- mkReg(DTRANS2_NORMAL);
    Reg#(STATE_DTRANS3) stateDTrans3 <- mkReg(DTRANS3_NORMAL);


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* DoDTranslate ******* //

    // 3-stage macro-operation
    
    // When:         The timing model tells us to translate an effective address.
    // Effect:       Access the TLB, return and cache the result.
    // Soft Inputs:  TOKEN
    // Soft Returns: One or Two MEM_ADDRESS, depending on the alignment.



    // doDTranslateNoMsg --
    //
    // The timing model is required to notify the functional model at the point
    // of address translation even if no translation is requested.  This is
    // used to maintain order in the functional model.  Failure to do this
    // introduces run-to-run variation in some models.
    rule doDTranslateNoMsg1 (! isValid(linkDoDTranslate.getReq));
        linkDoDTranslate.deq();

        dTrans1Q.enq(tagged Invalid);
    endrule

    rule doDTranslateNoMsg2 (! isValid(dTrans1Q.first));
        dTrans1Q.deq();

        // Tell the TLB
        link_dtlb_trans.noReq();

        // No response
    endrule


    // doDTranslate1
    //
    // When:   The timing model makes a new DTranslate req.
    // Effect: Retrieve the effective address.
    //
    rule doDTranslate1 (linkDoDTranslate.getReq() matches tagged Valid .req &&&
                        req.token matches .tok &&&
                        state.readyToBegin(tokContextId(tok)) &&&
                        tokScoreboard.canStartDTrans(tok.index));

        // Get the input from the timing model. Begin macro operation.
        linkDoDTranslate.deq();
        debugLog.record(fshow(tok.index) + $format(": DoDTranslate: Start."));

        // Update scoreboard.
        tokScoreboard.dTransStart(tok.index);

        // This operation on non-Load/Stores is a problem.
        let is_load = tokScoreboard.isLoad(tok.index);
        let is_store = tokScoreboard.isStore(tok.index);
        assertion.dTranslateOnMemOp(is_load || is_store);
        if (! is_load && ! is_store)
            debugLog.record(fshow(tok.index) + $format(": DoDTranslate: ERROR -- not a memory reference!"));

        // Get the optype since we're using the port now.
        let op_type = is_load ? tokScoreboard.getLoadType(tok.index) : tokScoreboard.getStoreType(tok.index);

        // Retrieve the mem address.
        tokMemAddr.readReq(tok.index);

        // Pass to the next stage.
        dTrans1Q.enq(tagged Valid tuple2(tok, op_type));

    endrule

    // doDTranslate2
    
    // When:   After the response come back from the RAM.
    // Effect: Make the request to the DTLB.
    
    (* conservative_implicit_conditions *)
    rule doDTranslate2 (state.readyToContinue() &&&
                        stateDTrans2 matches tagged DTRANS2_NORMAL &&&
                        dTrans1Q.first() matches tagged Valid {.tok, .op_type});
        
        // Get the response from memory.        
        let vaddr <- tokMemAddr.readRsp();

        // Align the address.
        match {.aligned_addr, .offset_addr} = isaAlignAddress(vaddr);
        
        // Record the offset for the load/store stage.
        tokScoreboard.setMemOpOffset(tok.index, offset_addr);

        if (!isaMemOpSpansTwoMemValues(vaddr, op_type))
        begin
        
            // A normal DTranslate. No stall.
            dTrans1Q.deq();

            // Get the translation from the TLB.
            link_dtlb_trans.makeReq(normalTLBQuery(tokContextId(tok), aligned_addr));

            // Log it.
            debugLog.record(fshow(tok.index) + $format(": DoDTranslate2: DTLB Req (VA: 0x%h AA: 0x%h)", vaddr, aligned_addr));
  
            // Pass to the next stage.
            dTrans2Q.enq(tagged DTRANS_NORMAL tok);

        end
        else // A spanning load/store.
        begin

            // Log it.
            debugLog.record(fshow(tok.index) + $format(": DoDTranslate2: DTLB Req Spanning 1 (VA: 0x%h, AA1: 0x%h)", vaddr, aligned_addr));
  
            // A spanning DTranslate. Make the first request to the TLB.
            let tlb_req = normalTLBQuery(tokContextId(tok), aligned_addr);
            tlb_req.notLastQuery = True;
            link_dtlb_trans.makeReq(tlb_req);
            
            // Stall to make the second request.
            stateDTrans2 <= tagged DTRANS2_SPAN_REQ aligned_addr;

        end

    endrule

    // doDTranslate2Span
    
    // When:   After doDTranslate2 stalls because of a spanning access.
    // Effect: Make the second request to the TLB and unstall.

    rule doDTranslate2Span (state.readyToContinue() &&& stateDTrans2 matches tagged DTRANS2_SPAN_REQ .aligned_addr1);
    
        // Get the value from the previous stage.
        match {.tok, .op_type} = validValue(dTrans1Q.first());
        
        // Calculate the second virtual address.
        let aligned_addr2 = aligned_addr1 + fromInteger(valueOf(SizeOf#(MEM_VALUE)) / 8);
        
        // Make the second request to the tlb.
        link_dtlb_trans.makeReq(normalTLBQuery(tokContextId(tok), aligned_addr2));

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoDTranslate2: DTLB Req Spanning 2 (AA2: 0x%h)", aligned_addr2));
  
        // Unstall this stage.
        dTrans1Q.deq();  
        stateDTrans2 <= tagged DTRANS2_NORMAL;  

        // Pass to the next stage.
        dTrans2Q.enq(tagged DTRANS_SPAN tok);
    
    endrule

    // doDTranslate3
    
    // When:   Some time after doDTranslate3.
    // Effect: Get the response from the TLB, record it and return it.

    rule doDTranslate3 (state.readyToContinue() &&& stateDTrans3 matches tagged DTRANS3_NORMAL);
        
        // Get the response from the TLB.
        let translated_addr = link_dtlb_trans.getResp();
        link_dtlb_trans.deq();

        // If the TLB couldn't translate it we're in big trouble.
        MEM_ADDRESS mem_addr = translated_addr.pa;
        Bool page_fault = translated_addr.pageFault;

        case (dTrans2Q.first()) matches
            tagged DTRANS_NORMAL .tok:
            begin

                // A single access. We do not stall.
                dTrans2Q.deq();

                // Log it.
                debugLog.record(fshow(tok.index) + $format(": DoDTranslate3: DTLB Rsp (PA: 0x%h)", mem_addr));

                if (page_fault)
                begin
                    debugLog.record(fshow(tok.index) + $format(": DoDTranslate3: DTLB PAGE FAULT"));
                    tokScoreboard.setFault(tok.index, FAULT_DTRANS);
                end

                // Record the physical addr.
                dTransSaveTransQ.enq(tuple2(tok, tagged ONE mem_addr));

                // Update the scoreboard.
                tokScoreboard.dTransFinish(tok.index);

                // Return it to the timing partition. End of macro-operation (path 1)
                linkDoDTranslate.makeResp(initFuncpRspDoDTranslate(tok, mem_addr, page_fault));
                debugLog.record(fshow(tok.index) + $format(": doDTranslate: End (path 1)."));
                

            end
            tagged DTRANS_SPAN .tok:
            begin

                // A spanning access.

                // Log it.
                debugLog.record(fshow(tok.index) + $format(": DoDTranslate3: DTLB Span Rsp 1 (PA1: 0x%h)", mem_addr));

                if (page_fault)
                begin
                    debugLog.record(fshow(tok.index) + $format(": DoDTranslate3: DTLB PAGE FAULT"));
                    tokScoreboard.setFault(tok.index, FAULT_DTRANS);
                end

                // Return the first part to the timing partition.
                linkDoDTranslate.makeResp(initFuncpRspDoDTranslate_part1(tok, mem_addr, page_fault));

                // Stall this stage to get the second response.
                stateDTrans3 <= tagged DTRANS3_SPAN_RSP
                                       {
                                           firstPA: mem_addr,
                                           firstRefFaulted: page_fault
                                       };

            end
        endcase
    
    endrule
    
    rule doDTranslate3Span (state.readyToContinue() &&& stateDTrans3 matches tagged DTRANS3_SPAN_RSP .trans1);
    
        // Get the value from the previous stage.
        let tok = getDTransToken(dTrans2Q.first());

        // Propagate poison bit from first translation
        tok.poison = tok.poison || trans1.firstRefFaulted;

        // Get the response from the TLB.
        let translated_addr = link_dtlb_trans.getResp();
        link_dtlb_trans.deq();

        // If the TLB couldn't translate it we're in big trouble.
        MEM_ADDRESS mem_addr2 = translated_addr.pa;
        Bool page_fault = translated_addr.pageFault;

        if (page_fault)
        begin
            debugLog.record(fshow(tok.index) + $format(": DoDTranslate3: DTLB Spanning 2 PAGE FAULT"));
            tokScoreboard.setFault(tok.index, FAULT_DTRANS2);
        end

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoDTranslate3: DTLB Span Rsp 2 (PA2: 0x%h)", mem_addr2));

        // Record the physical addresses.
        dTransSaveTransQ.enq(tuple2(tok, tagged TWO tuple2(trans1.firstPA, mem_addr2)));

        // Unstall the pipeline.
        dTrans2Q.deq();
        stateDTrans3 <= DTRANS3_NORMAL;

        // Update the scoreboard.
        tokScoreboard.dTransFinish(tok.index);

        // Return the rest to the timing partition. End of macro-operation (path 2).
        linkDoDTranslate.makeResp(initFuncpRspDoDTranslate_part2(tok, mem_addr2, page_fault));
        debugLog.record(fshow(tok.index) + $format(": DoDTranslate: End (path 2)."));
    
    endrule


    //
    // doDTranslateSaveTrans --
    //     Delay the BRAM write to tokPhysicalMemAddrs to this cycle, with a short path
    //     through a FIFO, to relax timing from the translation arriving from the VA
    //     to PA service.  The BRAM write only takes a cycle and will complete before
    //     a client can access it.
    //
    rule doDTranslateSaveTrans (True);
        match {.tok, .trans} = dTransSaveTransQ.first();
        dTransSaveTransQ.deq();

        tokPhysicalMemAddrs.write(tok.index, trans);
    endrule

endmodule
