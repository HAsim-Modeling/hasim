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
`include "asim/provides/funcp_memstate_base_types.bsh"
  

// ========================================================================
//
//   Internal data structures
//
// ========================================================================


// STATE_ITRANS1
typedef union tagged
{
    void        ITRANS1_NORMAL;
    Tuple2#(ISA_ADDRESS, MEM_OFFSET) ITRANS1_SPAN_REQ;
}
STATE_ITRANS1
    deriving (Eq, Bits);


// STATE_ITRANS2
typedef enum
{
    ITRANS2_NORMAL, 
    ITRANS2_SPAN_RSP
}
STATE_ITRANS2
    deriving (Eq, Bits);


// ITRANS_INFO
typedef struct
{
    CONTEXT_ID contextId;
    MEM_OFFSET offset;
    Bool span;
}
ITRANS_INFO
    deriving (Eq, Bits); 

function ITRANS_INFO initITransNormal(CONTEXT_ID ctx_id, MEM_OFFSET offs);

    return ITRANS_INFO {contextId: ctx_id, offset: offs, span: False};

endfunction

function ITRANS_INFO initITransSpan(CONTEXT_ID ctx_id, MEM_OFFSET offs);

    // NOTE: offset refers to offset of the first addr.
    return ITRANS_INFO {contextId: ctx_id, offset: offs, span: True};

endfunction


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_DoITranslate#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_TLB_TRANSLATE link_itlb_trans)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_doITranslate.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_DO_ITRANSLATE,
                       FUNCP_RSP_DO_ITRANSLATE) linkDoITranslate <-
        mkFUNCPInterfaceServer("funcp_doITranslate");


    // ====================================================================
    //
    //   Local names for global data 
    //
    // ====================================================================

    let state = glob.state;


    // ====================================================================
    //
    //   Local state
    //
    // ====================================================================

    FIFO#(ITRANS_INFO) iTransQ <- mkSizedFIFO(8);
    Reg#(STATE_ITRANS1) stateITrans1 <- mkReg(ITRANS1_NORMAL);
    Reg#(STATE_ITRANS2) stateITrans2 <- mkReg(ITRANS2_NORMAL);


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================

    // ******* DoITranslate ******* //

    // 2-stage macro-operation. Stages 1 and 2 may stall.
    
    // When:         The timing model tells us to translate a fetch address.
    // Effect:       Record the virtual address, access the TLB, return and cache the result.
    // Soft Inputs:  ISA_ADDRESS.
    // Soft Returns: One or Two MEM_ADDRESS, depending on the alignment.


    // doITranslate1
    
    // When:   The timing model makes a new ITranslate req.
    // Effect: Record the virtual address, make the req to the TLB.

    rule doITranslate1 (True);

        // Get the input from the timing model. Begin macro operation.
        let req = linkDoITranslate.getReq();
        let vaddr = req.virtualAddress;

        // Align the address.
        match {.aligned_addr, .offset_addr} = isaAlignAddress(vaddr);

        if (!isaFetchSpansTwoMemValues(vaddr))
        begin
        
            // A normal ITranslate. No stall.
            linkDoITranslate.deq();

            // Get the translation from the TLB.
            link_itlb_trans.makeReq(normalTLBQuery(req.contextId, aligned_addr));
            
            // Log it.
            debugLog.record($format("DoITranslate1: ITLB Req (VA: 0x%h, AA: 0x%h)", vaddr, aligned_addr));
  
            // Pass to the next stage.
            iTransQ.enq(initITransNormal(req.contextId, offset_addr));

        end
        else     // A spanning fetch.
        begin

            // Log it.
            debugLog.record($format("DoITranslate1: Spanning ITLB Req 1 (VA: 0x%h, AA1: 0x%h)", vaddr, aligned_addr));
  
            // A spanning ITranslate. Make the first request to the TLB.
            link_itlb_trans.makeReq(normalTLBQuery(req.contextId, aligned_addr));
  
            // Stall to make the second request.
            stateITrans1 <= tagged ITRANS1_SPAN_REQ tuple2(aligned_addr, offset_addr);

        end

    endrule

    // doITranslate1Span
    
    // When:   After doITranslate1 stalls because of an unaligned access.
    // Effect: Make the second request to the TLB and unstall.

    rule doITranslate1Span (stateITrans1 matches tagged ITRANS1_SPAN_REQ {.aligned_addr1, .offset});
         
        // Get the data from the previous stage.
        let req = linkDoITranslate.getReq();
        let ctx_id = req.contextId;
    
        // Calculate the second virtual address.
        let aligned_addr2 = aligned_addr1 + fromInteger(valueOf(SizeOf#(MEM_VALUE)) / 8);
    
        // Make the second request to the tlb.
        link_itlb_trans.makeReq(normalTLBQuery(ctx_id, aligned_addr2));

        // Log it.
        debugLog.record($format("DoITranslate1: Second ITLB Req 2 (AA2: 0x%h)", aligned_addr2));
  
        // Unstall this stage.
        linkDoITranslate.deq();  
        stateITrans1 <= tagged ITRANS1_NORMAL;  

        // Pass to the next stage.
        iTransQ.enq(initITransSpan(ctx_id, offset));
    
    endrule

    // doITranslate2
    
    // When:   Some time after doITranslate1.
    // Effect: Get the response from the TLB, record it and return it.

    rule doITranslate2 (stateITrans2 == ITRANS2_NORMAL);
    
        // Get the response from the TLB.
        let translated_addr = link_itlb_trans.getResp();
        link_itlb_trans.deq();
        MEM_ADDRESS mem_addr = translated_addr.pa;
        Bool page_fault = translated_addr.pageFault;

        // Get the data from the previous stage.
        let itrans_info = iTransQ.first();
        
        if (!itrans_info.span)
        begin

            // A single access. We do not stall.
            iTransQ.deq();

            // Log it.
            debugLog.record($format("DoITranslate2: ITLB Rsp (PA: 0x%h)", mem_addr));

            if (page_fault)
            begin
                debugLog.record($format("DoITranslate2: ITLB PAGE FAULT"));
            end

            // Return it to the timing partition. End of macro-operation (path 1)
            linkDoITranslate.makeResp(initFuncpRspDoITranslate(itrans_info.contextId, mem_addr, itrans_info.offset, page_fault));
            debugLog.record($format("DoITranslate: End (path 1)."));

        end
        else
        begin

            // A spanning access.

            // Log it.
            debugLog.record($format("DoITranslate2: ITLB Spanning Rsp 1 (PA1: 0x%h)", mem_addr));

            if (page_fault)
            begin
                debugLog.record($format("DoITranslate2: ITLB PAGE FAULT"));
            end

            // Return the first part to the timing partition.
            linkDoITranslate.makeResp(initFuncpRspDoITranslate_part1(itrans_info.contextId, mem_addr, itrans_info.offset, page_fault));

            // Stall this stage to get the second response.
            stateITrans2 <= ITRANS2_SPAN_RSP;

        end
    
    endrule
    
    // doITranslate2Span
    
    // When:   After doITranslate1 stalls for a lookup which spans two locations.
    // Effect: Get the second response, record it, and return it.

    rule doITranslate2Span (stateITrans2 == ITRANS2_SPAN_RSP);
    
        // Get the data from the previous stage.
        let itrans_info = iTransQ.first();
        let ctx_id = itrans_info.contextId;
    
        // Get the response from the TLB.
        let translated_addr = link_itlb_trans.getResp();
        link_itlb_trans.deq();
        MEM_ADDRESS mem_addr2 = translated_addr.pa;
        Bool page_fault = translated_addr.pageFault;

        if (page_fault)
        begin
            debugLog.record($format("DoITranslate2: ITLB Spanning 2 PAGE FAULT"));
        end

        // Log it.
        debugLog.record($format("DoITranslate2: ITLB Spanning Rsp 2 (PA2: 0x%h)", mem_addr2));

        // Unstall the pipeline.
        stateITrans2 <= ITRANS2_NORMAL;
        iTransQ.deq();

        // Return the rest to the timing partition. End of macro-operation (path 2).
        // Note: part 2 has an offset of zero.... otherwise it would be non-contiguous
        linkDoITranslate.makeResp(initFuncpRspDoITranslate_part2(ctx_id, mem_addr2, 0, page_fault));
        debugLog.record($format("DoITranslate: End (path 2)."));
    
    endrule

endmodule
