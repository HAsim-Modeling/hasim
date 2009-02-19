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
    ISA_ADDRESS ITRANS1_SPAN_REQ;
}
STATE_ITRANS1
    deriving (Eq, Bits);


// STATE_ITRANS2
typedef union tagged
{
    void        ITRANS2_NORMAL;
    struct
    {
        MEM_ADDRESS firstPA;
        Bool firstRefFaulted;
    }
    ITRANS2_SPAN_RSP;
}
STATE_ITRANS2
    deriving (Eq, Bits);


// ITRANS_INFO
typedef union tagged
{
    TOKEN ITRANS_NORMAL;
    TOKEN ITRANS_SPAN;
}
ITRANS_INFO
    deriving (Eq, Bits); 


function TOKEN getITransToken(ITRANS_INFO i);

    case (i) matches
        tagged ITRANS_NORMAL .tok: return tok;
        tagged ITRANS_SPAN   .tok: return tok;
    endcase

endfunction


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_DoITranslate#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_TLB_TRANSLATE link_itlb_trans,
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_ADDRESS) tokAddr,
    BRAM#(TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalAddrs)
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
                       FUNCP_RSP_DO_ITRANSLATE) linkDoITranslate <- mkConnection_Server("funcp_doITranslate");


    // ====================================================================
    //
    //   Local names for global data 
    //
    // ====================================================================

    let state = glob.state;
    let tokScoreboard = glob.tokScoreboard;


    // ====================================================================
    //
    //   Local state
    //
    // ====================================================================

    FIFO#(ITRANS_INFO) iTransQ <- mkFIFO();
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
    // Soft Inputs:  TOKEN, ISA_ADDRESS.
    // Soft Returns: One or Two MEM_ADDRESS, depending on the alignment.


    // doITranslate1
    
    // When:   The timing model makes a new ITranslate req.
    // Effect: Record the virtual address, make the req to the TLB.

    rule doITranslate1 (state.readyToBegin(tokContextId(linkDoITranslate.getReq().token)));

        // Get the input from the timing model. Begin macro operation.
        let req = linkDoITranslate.getReq();
        let tok = req.token;
        let vaddr = req.address;
        debugLog.record(fshow(tok.index) + $format(": DoITranslate: Begin."));
        
        // Update scoreboard.
        tokScoreboard.iTransStart(tok.index);

        // Record the address. (For relative branches, etc.)
        tokAddr.write(tok.index, vaddr);
        
        // Align the address.
        match {.aligned_addr, .offset_addr} = isaAlignAddress(vaddr);
        
        // Record the offset for the fetch stage.
        tokScoreboard.setFetchOffset(tok.index, offset_addr);

        if (!isaFetchSpansTwoMemValues(vaddr))
        begin
        
            // A normal ITranslate. No stall.
            linkDoITranslate.deq();

            // Get the translation from the TLB.
            link_itlb_trans.makeReq(normalTLBQuery(tok, aligned_addr));
            
            // Log it.
            debugLog.record(fshow(tok.index) + $format(": DoITranslate1: ITLB Req (VA: 0x%h, AA: 0x%h)", vaddr, aligned_addr));
  
            // Pass to the next stage.
            iTransQ.enq(tagged ITRANS_NORMAL tok);

        end
        else     // A spanning fetch.
        begin

            // Log it.
            debugLog.record(fshow(tok.index) + $format(": DoITranslate1: Spanning ITLB Req 1 (VA: 0x%h, AA1: 0x%h)", vaddr, aligned_addr));
  
            // A spanning ITranslate. Make the first request to the TLB.
            link_itlb_trans.makeReq(normalTLBQuery(tok, aligned_addr));
  
            // Stall to make the second request.
            stateITrans1 <= tagged ITRANS1_SPAN_REQ aligned_addr;

        end

    endrule

    // doITranslate1Span
    
    // When:   After doITranslate1 stalls because of an unaligned access.
    // Effect: Make the second request to the TLB and unstall.

    rule doITranslate1Span (state.readyToContinue() &&& stateITrans1 matches tagged ITRANS1_SPAN_REQ .aligned_addr1);
         
        // Get the data from the previous stage.
        let req = linkDoITranslate.getReq();
        let tok = req.token;
    
        // Calculate the second virtual address.
        let aligned_addr2 = aligned_addr1 + fromInteger(valueOf(SizeOf#(MEM_VALUE)) / 8);
    
        // Make the second request to the tlb.
        link_itlb_trans.makeReq(normalTLBQuery(tok, aligned_addr2));

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoITranslate1: Second ITLB Req 2 (AA2: 0x%h)", aligned_addr2));
  
        // Unstall this stage.
        linkDoITranslate.deq();  
        stateITrans1 <= tagged ITRANS1_NORMAL;  

        // Pass to the next stage.
        iTransQ.enq(tagged ITRANS_SPAN tok);
    
    endrule

    // doITranslate2
    
    // When:   Some time after doITranslate1.
    // Effect: Get the response from the TLB, record it and return it.

    rule doITranslate2 (state.readyToContinue() &&& stateITrans2 matches tagged ITRANS2_NORMAL);
    
        // Get the response from the TLB.
        let translated_addr = link_itlb_trans.getResp();
        link_itlb_trans.deq();

        // If the TLB couldn't translate it we're in big trouble.
        MEM_ADDRESS mem_addr = translated_addr.pa;
        Bool page_fault = translated_addr.pageFault;

        // Get the data from the previous stage.
        case (iTransQ.first()) matches
            tagged ITRANS_NORMAL .tok:
            begin

                // A single access. We do not stall.
                iTransQ.deq();

                // Log it.
                debugLog.record(fshow(tok.index) + $format(": DoITranslate2: ITLB Rsp (PA: 0x%h)", mem_addr));

                if (page_fault)
                begin
                    debugLog.record(fshow(tok.index) + $format(": DoITranslate2: ITLB PAGE FAULT"));
                    tokScoreboard.setFault(tok.index, FAULT_ITRANS);
                end

                // Record the physical addr.
                tokPhysicalAddrs.write(tok.index, tagged ONE mem_addr);

                // Update the scoreboard.
                tokScoreboard.iTransFinish(tok.index);

                // Return it to the timing partition. End of macro-operation (path 1)
                linkDoITranslate.makeResp(initFuncpRspDoITranslate(tok, mem_addr, page_fault));
                debugLog.record(fshow(tok.index) + $format(": DoITranslate: End (path 1)."));

            end
            tagged ITRANS_SPAN .tok:
            begin

                // A spanning access.

                // Log it.
                debugLog.record(fshow(tok.index) + $format(": DoITranslate2: ITLB Spanning Rsp 1 (PA1: 0x%h)", mem_addr));

                if (page_fault)
                begin
                    debugLog.record(fshow(tok.index) + $format(": DoITranslate2: ITLB PAGE FAULT"));
                    tokScoreboard.setFault(tok.index, FAULT_ITRANS);
                end

                // Return the first part to the timing partition.
                linkDoITranslate.makeResp(initFuncpRspDoITranslate_part1(tok, mem_addr, page_fault));

                // Stall this stage to get the second response.
                stateITrans2 <= tagged ITRANS2_SPAN_RSP
                                       {
                                           firstPA: mem_addr,
                                           firstRefFaulted: page_fault
                                       };

            end
        endcase
    
    endrule
    
    // doITranslate2Span
    
    // When:   After doITranslate1 stalls for a lookup which spans two locations.
    // Effect: Get the second response, record it, and return it.

    rule doITranslate2Span (state.readyToContinue() &&& stateITrans2 matches tagged ITRANS2_SPAN_RSP .trans1);
    
        // Get the data from the previous stage.
        let tok = getITransToken(iTransQ.first());
    
        // Propagate poison bit from first translation
        tok.poison = tok.poison || trans1.firstRefFaulted;

        // Get the response from the TLB.
        let translated_addr = link_itlb_trans.getResp();
        link_itlb_trans.deq();

        // If the TLB couldn't translate it we're in big trouble.
        MEM_ADDRESS mem_addr2 = translated_addr.pa;
        Bool page_fault = translated_addr.pageFault;

        if (page_fault)
        begin
            debugLog.record(fshow(tok.index) + $format(": DoITranslate2: ITLB Spanning 2 PAGE FAULT"));
            tokScoreboard.setFault(tok.index, FAULT_ITRANS2);
        end

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoITranslate2: ITLB Spanning Rsp 2 (PA2: 0x%h)", mem_addr2));

        // Record the physical addr.
        tokPhysicalAddrs.write(tok.index, tagged TWO tuple2(trans1.firstPA, mem_addr2));

        // Unstall the pipeline.
        stateITrans2 <= ITRANS2_NORMAL;
        iTransQ.deq();

        // Update the scoreboard.
        tokScoreboard.iTransFinish(tok.index);

        // Return the rest to the timing partition. End of macro-operation (path 2).
        linkDoITranslate.makeResp(initFuncpRspDoITranslate_part2(tok, mem_addr2, page_fault));
        debugLog.record(fshow(tok.index) + $format(": DoITranslate: End (path 2)."));
    
    endrule

endmodule
