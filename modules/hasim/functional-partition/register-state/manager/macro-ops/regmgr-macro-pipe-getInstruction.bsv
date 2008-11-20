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

// INST_INFO
typedef struct
{
    TOKEN token;
    UP_TO_TWO#(MEM_ADDRESS) memAddrs;
}
INST_INFO
    deriving (Eq, Bits);

// STATE_INST2
typedef union tagged
{
    void        INST2_NORMAL;
    INST_INFO   INST2_SPAN_REQ;
}
STATE_INST2
    deriving (Eq, Bits);


// STATE_INST3
typedef union tagged
{
    void        INST3_NORMAL;
    ISA_VALUE   INST3_SPAN_RSP;
}
STATE_INST3
    deriving (Eq, Bits);


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_GetInstruction#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_MEMORY_QUEUE linkToMem,
    BRAM#(TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalAddrs,
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_INSTRUCTION) tokInst)
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

    FIFO#(TOKEN) inst1Q  <- mkFIFO();
    FIFO#(INST_INFO) inst2Q  <- mkFIFO();
    Reg#(STATE_INST2) stateInst2 <- mkReg(INST2_NORMAL);
    Reg#(STATE_INST3) stateInst3 <- mkReg(INST3_NORMAL);

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
    // Effect: Retrieve the phsyical address(es).

    rule getInstruction1 (state.readyToBegin());

        // Read input. Beginning of macro-operation.
        let req = linkGetInst.getReq();
        let tok = req.token;
        linkGetInst.deq();
        debugLog.record($format("TOKEN %0d: GetInstruction: Begin.", tok.index));

        // Update scoreboard.
        tokScoreboard.fetStart(tok.index);

        // Retrieve the physical address.
        tokPhysicalAddrs.readReq(tok.index);
        
        // Send to the next stage.
        inst1Q.enq(tok);
        
    endrule
    
    // getInstruction2 
    
    // When:   Some time after getInstruction1 and we are not stalled.
    // Effect: Pass the physical address on to memory. If the address spans
    //         memory locations stall for a second request.

    rule getInstruction2 (state.readyToContinue() &&& stateInst2 matches tagged INST2_NORMAL);
    
        // Get the info from the previous stage.
        let tok = inst1Q.first();
        
        // Get the physical address(es).
        let p_addrs <- tokPhysicalAddrs.readRsp();
        
        case (p_addrs) matches
            tagged ONE .p_addr:
            begin

                // We are not stalled.
                inst1Q.deq();
                
                // Log it.
                debugLog.record($format("TOKEN %0d: GetInstruction2: Load Req (PA: 0x%h)", tok.index, p_addr));

                // Kick to Mem State.
                let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr};
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Pass it to the next stage.
                inst2Q.enq(INST_INFO {token: tok, memAddrs: p_addrs});

            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Log it.
                debugLog.record($format("TOKEN %0d: GetInstruction2: Spanning Load Req 1 (PA1: 0x%h, PA2: 0x%h)", tok.index, p_addr1, p_addr2));

                // Kick the first request to the MemState.
                let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr1};
                linkToMem.makeReq(tagged REQ_LOAD m_req);

                // Stall to make the second request.
                stateInst2 <= tagged INST2_SPAN_REQ (INST_INFO {token: tok, memAddrs: p_addrs});

            end

        endcase
        
    
    endrule
    
    // getInstruction2Span
    
    // When:   After getInstruction2 stalls because of a spanning instruction.
    // Effect: Make the second request to memory. Unstall the stage.

    rule getInstruction2Span (state.readyToContinue() &&& stateInst2 matches tagged INST2_SPAN_REQ .fetch_info);
            
        // Get the data from the previous stage.
        let tok = inst1Q.first();
        
        // Kick the second request to MemState.
        let p_addr2 = getSecondOfTwo(fetch_info.memAddrs);
        let m_req = MEMSTATE_REQ_LOAD {tok: tok, addr: p_addr2};
        linkToMem.makeReq(tagged REQ_LOAD m_req);

        // Log it.
        debugLog.record($format("TOKEN %0d: GetInstruction2: Spanning Load Req 2 (PA2: 0x%h)", tok.index, p_addr2));

        // Unstall this stage.
        inst1Q.deq();
        stateInst2 <= tagged INST2_NORMAL;
        
        // Pass it to the next stage.
        inst2Q.enq(fetch_info);
        
    endrule

    // getInstruction3

    // When:   Physical address and instruction are available after getInstruction2.
    // Effect: If there was just one request, record the instruction, kick back to timing model.
    //         Otherwise stall to get the second response.

    rule getInstruction3 (state.readyToContinue() &&& stateInst3 matches tagged INST3_NORMAL);

        // Get the data from the previous stage.
        let fetch_info = inst2Q.first();

        // Get resp from the Mem State.
        MEM_VALUE v = linkToMem.getResp();
        linkToMem.deq();
     
        case (fetch_info.memAddrs) matches
            tagged ONE .p_addr:
            begin

                // Normal fetch. We are not stalled.
                inst2Q.deq();
                
                // Get the offset from ITranslate.
                let offset = tokScoreboard.getFetchOffset(fetch_info.token.index);

                ISA_INSTRUCTION inst = isaInstructionFromMemValue(v, offset);

                // Log it.
                debugLog.record($format("TOKEN %0d: GetInstruction3: Load Rsp (V: 0x%h, I: 0x%h)", fetch_info.token.index, v, inst));

                // Record the instruction.
                tokInst.write(fetch_info.token.index, inst);

                // Update scoreboard.
                tokScoreboard.fetFinish(fetch_info.token.index);

                // Send response to timing partition. End of macro-operation (path 1).
                linkGetInst.makeResp(initFuncpRspGetInstruction(fetch_info.token, inst));
                debugLog.record($format("TOKEN %0d: GetInstruction: End (path 1).", fetch_info.token.index));


            end
            tagged TWO {.p_addr1, .p_addr2}:
            begin

                // Log it.
                debugLog.record($format("TOKEN %0d: getInstruction3: Spanning Load Rsp 1 (V1: 0x%h)", fetch_info.token.index, v));

                // We need two fetches for this guy. Stall for the second response.
                stateInst3 <= tagged INST3_SPAN_RSP v;

            end
        endcase

    endrule

    // getInstruction3Span

    // When:   After getInstruction3 has stalled waiting for a second response.
    // Effect: Use both responses to create the instruction. Record it and return it to the timing model.

    rule getInstruction3Span (state.readyToContinue() &&& stateInst3 matches tagged INST3_SPAN_RSP .v1);
    
        // Get the data from the previous stage.
        INST_INFO fetch_info = inst2Q.first();
        
        // Get resp from the Mem State.
        MEM_VALUE v2 = linkToMem.getResp();
        linkToMem.deq();
        
        // Get the offset from ITranslate.
        let offset = tokScoreboard.getFetchOffset(fetch_info.token.index);

        // Convert the raw bits to an instruction.
        ISA_INSTRUCTION inst = isaInstructionFromSpanningMemValues(v1, v2, offset);

        // Log it.
        debugLog.record($format("TOKEN %0d: GetInstruction3: Spanning Load Rsp 2 (V2: 0x%h, I: 0x%h)", fetch_info.token.index, v2, inst));

        // Record the instruction.
        tokInst.write(fetch_info.token.index, inst);

        // Update scoreboard.
        tokScoreboard.fetFinish(fetch_info.token.index);

        // Unstall this stage.
        inst2Q.deq();
        stateInst3 <= tagged INST3_NORMAL;
        
        // Send response to timing partition. End of macro-operation (path 2).
        linkGetInst.makeResp(initFuncpRspGetInstruction(fetch_info.token, inst));
        debugLog.record($format("TOKEN %0d: GetInstruction: End (path 2).", fetch_info.token.index));
        
    endrule

endmodule
