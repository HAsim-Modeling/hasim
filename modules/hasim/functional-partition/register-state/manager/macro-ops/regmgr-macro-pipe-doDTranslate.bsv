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
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr,
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

    Connection_Server#(FUNCP_REQ_DO_DTRANSLATE,
                       FUNCP_RSP_DO_DTRANSLATE) linkDoDTranslate <- mkConnection_Server("funcp_doDTranslate");


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

    FIFO#(Tuple2#(TOKEN, ISA_MEMOP_TYPE)) dTrans1Q <- mkFIFO();
    FIFO#(DTRANS_INFO) dTrans2Q <- mkFIFO();

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


    // doDTranslate1
    
    // When:   The timing model makes a new DTranslate req.
    // Effect: Retrieve the effective address.

    rule doDTranslate1 (state.readyToBegin());

        // Get the input from the timing model. Begin macro operation.
        let req = linkDoDTranslate.getReq();
        linkDoDTranslate.deq();
        let tok = req.token;
        debugLog.record(fshow(tok.index) + $format(": DoDTranslate: Start."));

        // Update scoreboard.
        tokScoreboard.dTransStart(tok.index);

        // This operation on non-Load/Stores is a problem.
        let is_load = tokScoreboard.isLoad(tok.index);
        let is_store = tokScoreboard.isStore(tok.index);
        assertion.dTranslateOnMemOp(is_load || is_store);

        // Get the optype since we're using the port now.
        let op_type = is_load ? tokScoreboard.getLoadType(tok.index) : tokScoreboard.getStoreType(tok.index);

        // Retrieve the mem address.
        tokMemAddr.readReq(tok.index);

        // Pass to the next stage.
        dTrans1Q.enq(tuple2(tok, op_type));

    endrule

    // doDTranslate2
    
    // When:   After the response come back from the RAM.
    // Effect: Make the request to the DTLB.
    
    (* conservative_implicit_conditions *)
    rule doDTranslate2 (state.readyToContinue() &&& stateDTrans2 matches tagged DTRANS2_NORMAL);
    
        // Get the value from the previous stage.
        match {.tok, .op_type} = dTrans1Q.first();
        
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
            link_dtlb_trans.makeReq(normalTLBQuery(tok, aligned_addr));

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
            link_dtlb_trans.makeReq(normalTLBQuery(tok, aligned_addr));
            
            // Stall to make the second request.
            stateDTrans2 <= tagged DTRANS2_SPAN_REQ aligned_addr;

        end

    endrule

    // doDTranslate2Span
    
    // When:   After doDTranslate2 stalls because of a spanning access.
    // Effect: Make the second request to the TLB and unstall.

    rule doDTranslate2Span (state.readyToContinue() &&& stateDTrans2 matches tagged DTRANS2_SPAN_REQ .aligned_addr1);
    
        // Get the value from the previous stage.
        match {.tok, .op_type} = dTrans1Q.first();
        
        // Calculate the second virtual address.
        let aligned_addr2 = aligned_addr1 + fromInteger(valueOf(SizeOf#(MEM_VALUE)) / 8);
        
        // Make the second request to the tlb.
        link_dtlb_trans.makeReq(normalTLBQuery(tok, aligned_addr2));

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
                tokPhysicalMemAddrs.write(tok.index, tagged ONE mem_addr);

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
        tokPhysicalMemAddrs.write(tok.index, tagged TWO tuple2(trans1.firstPA, mem_addr2));

        // Unstall the pipeline.
        dTrans2Q.deq();
        stateDTrans3 <= DTRANS3_NORMAL;

        // Update the scoreboard.
        tokScoreboard.dTransFinish(tok.index);

        // Return the rest to the timing partition. End of macro-operation (path 2).
        linkDoDTranslate.makeResp(initFuncpRspDoDTranslate_part2(tok, mem_addr2, page_fault));
        debugLog.record(fshow(tok.index) + $format(": DoDTranslate: End (path 2)."));
    
    endrule

endmodule
