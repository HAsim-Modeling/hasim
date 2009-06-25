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

// LOADS_INFO
typedef struct
{
    TOKEN token;
    UP_TO_TWO#(MEM_ADDRESS) memAddrs;
    ISA_MEMOP_TYPE opType;
    MEM_OFFSET offset;
}
LOADS_INFO
    deriving (Eq, Bits);

// STATE_LOADS2
typedef union tagged
{
    void       LOADS2_NORMAL;
    LOADS_INFO LOADS2_SPAN_REQ;
}
STATE_LOADS2
   deriving (Eq, Bits);

// STATE_LOADS3
typedef union tagged
{
    void       LOADS3_NORMAL;
    MEM_VALUE  LOADS3_SPAN_RSP;
    
}
STATE_LOADS3
    deriving (Eq, Bits);


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_DoLoads#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_MEMORY_QUEUE linkToMem,
    REGSTATE_PHYSICAL_REGS_WRITE_REG prf,
    BROM#(TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalMemAddrs,
    BROM#(TOKEN_INDEX, REGMGR_DST_REGS) tokDsts)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_doLoads.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_DO_LOADS, 
                       FUNCP_RSP_DO_LOADS) linkDoLoads <- mkConnection_Server("funcp_doLoads");


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

    FIFO#(TOKEN) loads1Q  <- mkSizedFIFO(valueOf(MAX_FUNCP_INFLIGHT_MEMREFS));
    FIFO#(LOADS_INFO) loads2Q  <- mkSizedFIFO(valueOf(MAX_FUNCP_INFLIGHT_MEMREFS));

    Reg#(STATE_LOADS2) stateLoads2 <- mkReg(LOADS2_NORMAL);
    Reg#(STATE_LOADS3) stateLoads3 <- mkReg(LOADS3_NORMAL);


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* doLoads ******* //

    // 3 or 4-stage macro operation which makes Loads read memory.

    // When:   When the timing model requests it.
    // Effect: Read the effective address, do a load from the memory state, and write it back.
    // Soft Inputs:  Token
    // Soft Returns: Token
    
    // doLoads1

    // When:   When the timing model starts a doLoads().
    // Effect: Lookup the effective address(es) of this token.

    (* conservative_implicit_conditions *)
    rule doLoads1 (linkDoLoads.getReq().token matches .tok &&&
                   state.readyToBegin(tokContextId(tok)) &&&
                   tokScoreboard.canStartLoad(tok.index));

        // Get the input from the timing model. Begin macro-operation.
        let req = linkDoLoads.getReq();
        linkDoLoads.deq();

        // Confirm timing model propagated poison bit correctly
        assertion.poisonBit(tokIsPoisoned(tok) == isValid(tokScoreboard.getFault(tok.index)));

        // If it's not actually a load, it's an exception.
        let isLoad = tokScoreboard.isLoad(tok.index);
        assertion.instructionIsActuallyALoad(isLoad);

        // Emulated loads were taken care of previously.  Also ignore killed tokens.
        if (tokScoreboard.emulateInstruction(tok.index) ||
            ! tokScoreboard.isAllocated(tok.index))
        begin
            // Log it.
            debugLog.record(fshow(tok.index) + $format(": DoLoads1: Ignoring junk token or emulated instruction."));

            // Respond to the timing model. End of macro-operation.
            linkDoLoads.makeResp(initFuncpRspDoLoads(tok));
        end
        else // Everything's okay.
        begin
            // Log it.
            debugLog.record(fshow(tok.index) + $format(": DoLoads: Begin.")); 

            // Update the scoreboard.
            tokScoreboard.loadStart(tok.index);

            // Read the effective address.
            tokPhysicalMemAddrs.readReq(tok.index);

            // Pass to the next stage.
            loads1Q.enq(tok);
        end

    endrule

    // doLoads2

    // When:   After doLoads1 occurs
    // Effect: Make the request to the memory state.

    rule doLoads2 (state.readyToContinue() &&& stateLoads2 matches tagged LOADS2_NORMAL);

        // Read the parameters from the previous stage.
        let tok = loads1Q.first();

        // Get the address(es).
        let p_addrs <- tokPhysicalMemAddrs.readRsp();
        
        // Get the offset.
        let offset = tokScoreboard.getMemOpOffset(tok.index);
        
        // Get the optype.
        let l_type = tokScoreboard.getLoadType(tok.index);
        
        case (p_addrs) matches
            tagged ONE .p_addr:
            begin

                // Normal Load. We're not stalled.
                loads1Q.deq();

                // Log it.
                debugLog.record(fshow(tok.index) + $format(": DoLoads2: Requesting Load (PA: 0x%h)", p_addr));

                // Make the request to the DMem.
                let m_req = memStateReqLoad(tok, p_addr, False);
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Read the destination so we can writeback the correct register.
                tokDsts.readReq(tok.index);

                // Pass it on to the final stage.
                let load_info = LOADS_INFO {token: tok, memAddrs: p_addrs, offset: offset, opType: l_type};
                loads2Q.enq(load_info);

            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Log it.
                debugLog.record(fshow(tok.index) + $format(": DoLoads2: Starting Spanning Load (PA1: 0x%h)", p_addr1));

                // Make the request to the DMem.
                let m_req = memStateReqLoad(tok, p_addr1, False);
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Stall this stage for the second req.
                let load_info = LOADS_INFO {token: tok, memAddrs: p_addrs, offset: offset, opType: l_type};
                stateLoads2 <= tagged LOADS2_SPAN_REQ load_info;

            end
        endcase

    endrule

    rule doLoads2Span (state.readyToContinue() &&& stateLoads2 matches tagged LOADS2_SPAN_REQ .load_info);

        // Kick the second request to MemState.
        let p_addr2 = getSecondOfTwo(load_info.memAddrs);
        let m_req = memStateReqLoad(load_info.token, p_addr2, False);
        linkToMem.makeReq(tagged REQ_LOAD m_req);

        // Log it.
        debugLog.record(fshow(load_info.token.index) + $format(": DoLoads2: Finishing Spanning Load (PA2: 0x%h)", p_addr2));

        // Read the destination so we can writeback the correct register.
        tokDsts.readReq(load_info.token.index);

        // Unstall this stage.
        loads1Q.deq();
        stateLoads2 <= tagged LOADS2_NORMAL;

        // Pass it on to the final stage.
        loads2Q.enq(load_info);

    endrule


    // doLoads3

    // When:   Load response is available after doLoads2.
    // Effect: If there was just one request, record the resut, kick back to timing model.
    //         Otherwise stall to get the second response.

    rule doLoads3 (state.readyToContinue() &&& stateLoads3 matches tagged LOADS3_NORMAL);

        // Get the data from the previous stage.
        let load_info = loads2Q.first();
        let tok = load_info.token;

        // Get resp from the Mem State.
        MEM_VALUE v = linkToMem.getResp();
        linkToMem.deq();
     
        case (load_info.memAddrs) matches
            tagged ONE .p_addr:
            begin

                // Normal load. We are not stalled.
                loads2Q.deq();

                // Convert the value using the ISA-provided conversion function.
                ISA_VALUE val = isaLoadValueFromMemValue(v, load_info.offset, load_info.opType);
                debugLog.record(fshow(tok.index) + $format(": DoLoads3: ISA Load (V: 0x%h, T: %0d, O: %b) = 0x%h", v, pack(load_info.opType), load_info.offset, val)); 

                // Get the destination for the purposes of writeback.
                let dsts <- tokDsts.readRsp();

                // We assume that the destination for the load is destination 1.
                let dst_pr = validValue(dsts.pr[0]);

                // Log it.
                debugLog.record(fshow(tok.index) + $format(": DoLoads3: Load Response Writing (PR%0d <= 0x%h)", dst_pr, val));

                // Update the physical register file.
                prf.write(dst_pr, val);

                // Update the scoreboard.
                tokScoreboard.loadFinish(tok.index);

                // Respond to the timing model. End of macro-operation (path 1).
                linkDoLoads.makeResp(initFuncpRspDoLoads(tok));

            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Log it.
                debugLog.record(fshow(load_info.token.index) + $format(": DoLoads3: First Span Response (V1: 0x%h)", v));

                // We needed two loads for this guy. Stall for the second response.
                stateLoads3 <= tagged LOADS3_SPAN_RSP v;

            end

        endcase
    

    endrule

    // doLoads3Span

    // When:   After doLoads3 has stalled waiting for a second response.
    // Effect: Use both responses to create the value. Write it back and return to the timing model.

    rule doLoads3Span (state.readyToContinue() &&& stateLoads3 matches tagged LOADS3_SPAN_RSP .v1);
    
        // Get the data from the previous stage.
        LOADS_INFO load_info = loads2Q.first();
        let tok = load_info.token;
        
        // Get resp from the Mem State.
        MEM_VALUE v2 = linkToMem.getResp();
        linkToMem.deq();
        
        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoLoads3: Second Span Response (V2: 0x%h)", v2));

        // Convert the value using the ISA-provided conversion function.
        ISA_VALUE val = isaLoadValueFromSpanningMemValues(v1, v2, load_info.offset, load_info.opType);
        debugLog.record(fshow(tok.index) + $format(": DoLoads3: ISA SpanLoad (V1: 0x%h, V2: 0x%hm, T: %0d, O: %b) = 0x%h", v1, v2, pack(load_info.opType), load_info.offset, val)); 

        // Get the destination for the purposes of writeback.
        let dsts <- tokDsts.readRsp();

        // We assume that the destination for the load is destination 1.
        let dst_pr = validValue(dsts.pr[0]);

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": DoLoads3: Load Response Writing (PR%0d <= 0x%h)", dst_pr, val));

        // Update the physical register file.
        prf.write(dst_pr, val);

        // Unstall this stage.
        loads2Q.deq();
        stateLoads3 <= tagged LOADS3_NORMAL;

        // Update the scoreboard.
        tokScoreboard.loadFinish(tok.index);

        // Respond to the timing model. End of macro-operation (path 2).
        linkDoLoads.makeResp(initFuncpRspDoLoads(tok));
        
    endrule
    
endmodule
