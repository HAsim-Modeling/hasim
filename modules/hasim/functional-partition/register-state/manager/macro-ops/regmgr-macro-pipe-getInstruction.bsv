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

// INST_INFO
typedef struct
{
    CONTEXT_ID contextId;
    MEM_OFFSET offset;
    Bool span;
}
INST_INFO
    deriving (Eq, Bits);

function INST_INFO initFetchNormal(CONTEXT_ID ctx_id, MEM_OFFSET offs);

    return INST_INFO {contextId: ctx_id, offset: offs, span: False};

endfunction


function INST_INFO initFetchSpan(CONTEXT_ID ctx_id, MEM_OFFSET offs);

    return INST_INFO {contextId: ctx_id, offset: offs, span: True};

endfunction


// STATE_INST1
typedef union tagged
{
    void        INST1_NORMAL;
    MEM_OFFSET  INST1_SPAN_REQ;
}
STATE_INST1
    deriving (Eq, Bits);


// STATE_INST2
typedef union tagged
{
    void        INST2_NORMAL;
    ISA_VALUE   INST2_SPAN_RSP;
}
STATE_INST2
    deriving (Eq, Bits);


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_GetInstruction#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_MEMORY_QUEUE linkToMem)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_getInstruction.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_GET_INSTRUCTION,
                       FUNCP_RSP_GET_INSTRUCTION) linkGetInst <- mkConnection_Server("funcp_getInstruction");


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

    FIFO#(TOKEN) inst1Q  <- mkSizedFIFO(valueOf(MAX_FUNCP_INFLIGHT_MEMREFS));
    FIFO#(INST_INFO) inst2Q  <- mkSizedFIFO(valueOf(MAX_FUNCP_INFLIGHT_MEMREFS));
    Reg#(STATE_INST1) stateInst1 <- mkReg(INST1_NORMAL);
    Reg#(STATE_INST2) stateInst2 <- mkReg(INST2_NORMAL);

    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================
    
    // 3-stage macro-operation. Stages 2 and 3 may stall.
    
    // When:         The timing model tells us to fetch the instruction at a given address.
    // Effect:       Reads the DT (twice if unaligned), records the instruction.
    // Soft Inputs:  TOKEN from timing model.
    // Soft Returns: TOKEN and ISA_INSTRUCTION.


    // getInstruction1
    
    // When:   The timing model makes a new FETCH req.
    // Effect: Pass the physical address on to memory. If the address spans
    //         memory locations stall for a second request.

    rule getInstruction1 (stateInst1 matches tagged INST1_NORMAL);

        // Read input. Beginning of macro-operation.
        let req = linkGetInst.getReq();
        linkGetInst.deq();

        // Get the info from the previous stage.
        let tok = inst1Q.first();
        
        if (!req.hasMore)
        begin

            // A normal non-spanning request.
            debugLog.record($format("GetInstruction2: Load Req (PA: 0x%h)", req.physicalAddress));

            // Kick to Mem State.
            let youngest_tok = tokScoreboard.youngest(req.contextId);
            let dummy_tok = TOKEN { index: youngest_tok,
                                    poison: False,
                                    dummy: True,
                                    timep_info: TOKEN_TIMEP_INFO { scratchpad: 0 } };
            let m_req = memStateReqInstr(dummy_tok, req.physicalAddress);
            linkToMem.makeReq(tagged REQ_LOAD m_req);
            
            // Pass it to the next stage.
            inst2Q.enq(initFetchNormal(req.contextId, req.offset));

        end
        else
        begin

            // Log it.
            debugLog.record($format("GetInstruction2: Spanning Load Req 1 (PA1: 0x%h)", req.physicalAddress));

            // Kick the first request to the MemState.
            let youngest_tok = tokScoreboard.youngest(req.contextId);
            let dummy_tok = TOKEN { index: youngest_tok,
                                    poison: False,
                                    dummy: True,
                                    timep_info: TOKEN_TIMEP_INFO { scratchpad: 0 } };
            let m_req = memStateReqInstr(dummy_tok, req.physicalAddress);
            linkToMem.makeReq(tagged REQ_LOAD m_req);

            // Stall to make the second request.
            stateInst1 <= tagged INST1_SPAN_REQ req.offset;

        end
            
    endrule
    
    // getInstruction1Span
    
    // When:   After getInstruction1 stalls because of a spanning instruction.
    // Effect: Make the second request to memory. Unstall the stage.

    rule getInstruction1Span (stateInst1 matches tagged INST1_SPAN_REQ .offset);
            
        // Get the rest of the req from the timing model.
        let req = linkGetInst.getReq();
        linkGetInst.deq();
        
        // Kick the second request to MemState.
        let p_addr2 = req.physicalAddress;
        let youngest_tok = tokScoreboard.youngest(req.contextId);
        let dummy_tok = TOKEN { index: youngest_tok,
                                poison: False,
                                dummy: True,
                                timep_info: TOKEN_TIMEP_INFO { scratchpad: 0 } };
        
        let m_req = memStateReqInstr(dummy_tok, p_addr2);
        linkToMem.makeReq(tagged REQ_LOAD m_req);

        // Log it.
        debugLog.record($format("GetInstruction2: Spanning Load Req 2 (PA2: 0x%h)", p_addr2));

        // Unstall this stage.
        stateInst1 <= tagged INST1_NORMAL;
        
        // Pass it to the next stage.
        inst2Q.enq(initFetchSpan(req.contextId, offset));
        
    endrule

    // getInstruction2

    // When:   Physical address and instruction are available after getInstruction2.
    // Effect: If there was just one request, kick the instruction back to timing model.
    //         Otherwise stall to get the second response.

    rule getInstruction2 (stateInst2 matches tagged INST2_NORMAL);

        // Get the data from the previous stage.
        let fetch_info = inst2Q.first();

        // Get resp from the Mem State.
        MEM_VALUE v = linkToMem.getResp();
        linkToMem.deq();
     
        if (!fetch_info.span)
        begin

            // Normal fetch. We are not stalled.
            inst2Q.deq();

            ISA_INSTRUCTION inst = isaInstructionFromMemValue(v, fetch_info.offset);

            // Log it.
            debugLog.record($format("GetInstruction3: Load Rsp (V: 0x%h, I: 0x%h)", v, inst));

            // Send response to timing partition. End of macro-operation (path 1).
            linkGetInst.makeResp(initFuncpRspGetInstruction(fetch_info.contextId, inst));
            debugLog.record($format("GetInstruction: End (path 1)."));


        end
        else
        begin

            // Log it.
            debugLog.record($format("getInstruction3: Spanning Load Rsp 1 (V1: 0x%h)", v));

            // We need two fetches for this guy. Stall for the second response.
            stateInst2 <= tagged INST2_SPAN_RSP v;

        end

    endrule

    // getInstruction2Span

    // When:   After getInstruction2 has stalled waiting for a second response.
    // Effect: Use both responses to create the instruction. Record it and return it to the timing model.

    rule getInstruction2Span (stateInst2 matches tagged INST2_SPAN_RSP .v1);
    
        // Get the data from the previous stage.
        INST_INFO fetch_info = inst2Q.first();
        
        // Get resp from the Mem State.
        MEM_VALUE v2 = linkToMem.getResp();
        linkToMem.deq();

        // Convert the raw bits to an instruction.
        ISA_INSTRUCTION inst = isaInstructionFromSpanningMemValues(v1, v2, fetch_info.offset);

        // Log it.
        debugLog.record($format("GetInstruction2: Spanning Load Rsp 2 (V2: 0x%h, I: 0x%h)", v2, inst));

        // Unstall this stage.
        inst2Q.deq();
        stateInst2 <= tagged INST2_NORMAL;
        
        // Send response to timing partition. End of macro-operation (path 2).
        linkGetInst.makeResp(initFuncpRspGetInstruction(fetch_info.contextId, inst));
        debugLog.record($format("GetInstruction: End (path 2)."));
        
    endrule

endmodule
