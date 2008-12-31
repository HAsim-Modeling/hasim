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

// Library includes.

import FIFO::*;
import FIFOF::*;
import Vector::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_modellib.bsh"
 
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"
  
// Dictionary includes
`include "asim/dict/STATS_REGMGR_GETRESULTS.bsh"

// RRR includes
`include "asim/provides/rrr.bsh"
`include "asim/rrr/service_ids.bsh"
`include "asim/provides/isa_emulator.bsh"
`include "asim/rrr/remote_client_stub_ISA_EMULATOR.bsh"
`include "asim/rrr/remote_server_stub_ISA_EMULATOR.bsh"


// ========================================================================
//
//   Internal data structures
//
// ========================================================================

// STATE_RES4
typedef union tagged
{
    void        RES4_NORMAL;
    struct
    {
        Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE))) remainingValues;
        FUNCP_ISA_EXECUTION_RESULT result;
        Bit#(4)    current; 
    }
    RES4_ADDITIONAL_WB;
}
STATE_RES4
    deriving (Eq, Bits);



// ========================================================================
//
//   Internal method for managing a local architectural to physical
//   register map.  This map is used by the emulation code to recover
//   the register mappings for an emulated instruction.
//
// ========================================================================

interface FUNCP_EMULATION_REG_MAP;
    method Action updateMap(Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)) update);
    method Action mapReq(ISA_REG_INDEX ar);
    method ActionValue#(Maybe#(FUNCP_PHYSICAL_REG_INDEX)) mapResp(ISA_REG_INDEX ar);
    method Action reset();
endinterface

module mkEmulationRegMap
    // interface:
    (FUNCP_EMULATION_REG_MAP);
    
    // Separate valid bits so they can be cleared in a single operation
    Reg#(Vector#(ISA_NUM_REGS, Bool)) emulMapValid <- mkRegU();
    
    // Main architectural to physical register map table
    BRAM#(Bit#(TLog#(ISA_NUM_REGS)), FUNCP_PHYSICAL_REG_INDEX) emulMapTable <- mkBRAM();
    
    // Update queue state for writing new mappings to the table
    FIFOF#(Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING))) updateQ <- mkFIFOF();
    Reg#(Bit#(TLog#(ISA_MAX_DSTS))) updIdx <- mkReg(0);


    //
    // doUpdate --
    //     Loop over all destination mappings for an instruction, writing them
    //     to the map table one per cycle.
    //
    rule doUpdate (True);
        let upd = updateQ.first();

        // Update map table for current index in instruction's mapping vector
        if (upd[updIdx] matches tagged Valid {.ar, .pr})
        begin
            emulMapValid[pack(ar)] <= True;
            emulMapTable.write(pack(ar), pr);
        end
        
        // Is this the last mapping for the vector?
        Integer last_idx = 0;
        for (Integer x = 1; x < valueof(ISA_MAX_DSTS); x = x + 1)
        begin
            if (isValid(upd[x]))
                last_idx = x;
        end
        
        if (fromInteger(last_idx) > updIdx)
        begin
            // More work on this update request
            updIdx <= updIdx + 1;
        end
        else
        begin
            // Done with this request
            updateQ.deq();
            updIdx <= 0;
        end
    endrule


    //
    // updateMap --
    //     Write a new vector of mappings for an instruction to the table.
    //
    method Action updateMap(Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)) update);
        updateQ.enq(update);
    endmethod


    //
    // mapReq/Resp --
    //     Return physical register mapping for a single architectural register.
    //
    method Action mapReq(ISA_REG_INDEX ar) if (! updateQ.notEmpty());
        emulMapTable.readReq(pack(ar));
    endmethod


    method ActionValue#(Maybe#(FUNCP_PHYSICAL_REG_INDEX)) mapResp(ISA_REG_INDEX ar);
        let r <- emulMapTable.readRsp();

        if (emulMapValid[pack(ar)])
            return tagged Valid r;
        else
            return tagged Invalid;
    endmethod


    //
    // reset --
    //     Invalidate all mappings.
    //
    method Action reset() if (! updateQ.notEmpty());
        emulMapValid <= replicate(False);
    endmethod

endmodule: mkEmulationRegMap



// ========================================================================
//
//   Primary execute stage pipeline.
//
// ========================================================================

module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_GetResults#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_REG_MAPPING_GETRESULTS regMapping,
    REGSTATE_PHYSICAL_REGS_RW_REGS prf,
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokAddr,
    BRAM#(TOKEN_INDEX, ISA_INST_SRCS) tokWriters,
    BRAM_MULTI_READ#(3, TOKEN_INDEX, ISA_INST_DSTS) tokDsts,
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_INSTRUCTION) tokInst,
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr,
    BRAM#(TOKEN_INDEX, ISA_VALUE) tokStoreValue)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_getResults.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_GET_RESULTS, 
                       FUNCP_RSP_GET_RESULTS) linkGetResults <- mkConnection_Server("funcp_getResults");

    Connection_Client#(FUNCP_ISA_DATAPATH_REQ,
                       FUNCP_ISA_DATAPATH_RSP) linkToDatapath <- mkConnection_Client("isa_datapath");

    // Emulation RRR Stubs
    ClientStub_ISA_EMULATOR client_stub <- mkClientStub_ISA_EMULATOR();
    ServerStub_ISA_EMULATOR server_stub <- mkServerStub_ISA_EMULATOR();


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

    // Intermediate state between pipeline stages
    FIFO#(Tuple2#(TOKEN, Bool)) res1Q <- mkFIFO();
    FIFO#(TOKEN) res2Q <- mkFIFO();
    FIFO#(Tuple2#(TOKEN, ISA_ADDRESS)) res3Q   <- mkFIFO();
    FIFO#(ISA_REG_INDEX) syncPRFReqQ <- mkFIFO();
    FIFO#(Tuple2#(Bool, ISA_REG_INDEX)) syncQ <- mkFIFO();
    FIFO#(Tuple2#(ISA_REG_INDEX, ISA_VALUE)) updateRegQ <- mkFIFO();

    Reg#(STATE_RES4) stateRes4 <- mkReg(RES4_NORMAL);

    // Which token's instruction are we emulating?
    Reg#(TOKEN) emulatingToken <- mkRegU();
    // PC of emulating token
    Reg#(ISA_ADDRESS) emulatingPC <- mkRegU();
    // Which register are we currently synchronizing?
    Reg#(ISA_REG_INDEX) synchronizingCurReg <- mkReg(minBound);


    Stat stat_isa_emul <- mkStatCounter(`STATS_REGMGR_GETRESULTS_EMULATED_INSTRS);

    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================

    // ******* getResults ******* //
    
    // 4-stage macro operation. Stages 2 and 4 can stall.

    // When:   When the timing model requests an execution.
    // Effect: Perform register reads, then send to datapath for execution.
    // Soft Inputs:  Token
    // Soft Returns: Token, Result
    
    // getResults1

    // When:   When the timing model starts a getResults().
    // Effect: Lookup the locations of this token's sources.

    rule getResults1 (state.readyToBegin());

        // Get parameter from the timing model. Begin macro-operation.
        let req = linkGetResults.getReq();
        linkGetResults.deq();
        let tok = req.token;
        debugLog.record($format("TOKEN %0d: GetResults: Begin.", tok.index));

        // Update the scoreboard.
        tokScoreboard.exeStart(tok.index);
        
        // Token active or was it killed?
        let tok_active = tokScoreboard.isAllocated(tok.index);

        if (tokScoreboard.emulateInstruction(tok.index) && tok_active)
        begin

            // Record that we're emulating an instruction.
            state.setState(RSM_DrainingForEmulate);

            // Record which token is being emulated.
            emulatingToken <= tok;

             // Log it.
            debugLog.record($format("TOKEN %0d: GetResults1: Beginning Instruction Emulation.", tok.index));

        end
        else
        begin
        
            // Look up the writers.
            tokWriters.readReq(tok.index);

            // Pass it along to the next stage.
            res1Q.enq(tuple2(tok, tok_active));
        
        end

    endrule

    // getResults2
    // When:   After getResults1.
    // Effect: Use the writers to look up values from the PRF. 
    //         Also retreive the instruction itself and the PC.
    //         If the writers are not all ready then a stall can occur.

    rule getResults2 (state.readyToContinue());

        // Get input from getResults1.
        match { .tok, .tok_active } = res1Q.first();
        res1Q.deq();

        // Response from previous stage.
        let ws <- tokWriters.readRsp();
        
        // We let junk proceed
        if (! tok_active)
        begin
            // No values are needed for junk
            debugLog.record($format("TOKEN %0d: GetResults2: Letting Junk Proceed!", tok.index));

            prf.readRegVecReq(Vector::replicate(tagged Invalid));
        end
        else
        begin
            debugLog.record($format("TOKEN %0d: GetResults2: Requesting srcs", tok.index));

            prf.readRegVecReq(ws);
        end

        tokAddr.readReq(tok.index);
        tokInst.readPorts[1].readReq(tok.index);
        res2Q.enq(tok);

    endrule

    
    // getResults3
    // When:    After getResults2 or alternatively getResults2StallEnd
    // Effect:  Send all the data to the datapath.

    rule getResults3 (state.readyToContinue());

        // Get input from the previous stage.
        let tok = res2Q.first();
        res2Q.deq();

        // Get all the data the previous stage kicked off.
        let addr <- tokAddr.readRsp();
        let inst <- tokInst.readPorts[1].readRsp();

        // Log it.
        debugLog.record($format("TOKEN %0d: GetResults3: Sending to Datapath.", tok.index));

        // Send it to the datapath.
        linkToDatapath.makeReq(initISADatapathReq(tok, inst, addr));

        // Look up the destinations for the writeback.
        tokDsts.readPorts[0].readReq(tok.index);

        // Pass it to the next stage.
        res3Q.enq(tuple2(tok, addr));

    endrule
    
    // getResults4
    // When:   After getResults3 and the datapath returns the result.
    // Effect: If one or fewer destinations, write back the result and 
    //         return the result to the timing partition.
    //         If more results then the stall and continue to write them back.

    rule getResults4 (state.readyToContinue() &&& stateRes4 matches tagged RES4_NORMAL);

        // Get the token from the previous stage.
        match {.tok, .addr} = res3Q.first();

        // Get the response from the datapath.
        let rsp = linkToDatapath.getResp();
        let wbvals = rsp.writebacks;
        linkToDatapath.deq();

        // Tag illegal instruction.  An error will be raised on attempts to commit.
        if (rsp.except != FUNCP_ISA_EXCEPT_NONE)
        begin
            debugLog.record($format("TOKEN %0d: GetResults: Illegal instruction", tok.index));
            tokScoreboard.setFault(tok.index, FAULT_ILLEGAL_INSTR);
        end

        // Update the memaddress (only useful for loads/stores)
        if (rsp.timepResult matches tagged REffectiveAddr .ea)
            tokMemAddr.write(tok.index, ea);

        // Get the destination response
        let dsts <- tokDsts.readPorts[0].readRsp();
        
        // The first dest should always be valid (it may not be architecturally visible)
        let dst = validValue(dsts[0]);

        // Perform the first writeback, if any.
        case (wbvals[0]) matches
            tagged Invalid:  noAction; // Not writing back, either a Load, or no dests.
            tagged Valid .v: 
            begin // Do the first writeback.
                
                if (tokScoreboard.isStore(tok.index))
                begin
                
                    // Stores write dest0 insto the token table instead of the PRF.
                    tokStoreValue.write(tok.index, v);
                
                end
                else  // A normal PRF writeback
                begin
            
                    prf.write(dst, v);
                    debugLog.record($format("TOKEN %0d: GetResults4: Writing (PR%0d <= 0x%x)", tok.index, dst, v));
                
                end

            end
        endcase
        
        // Is there anything more to writeback?

        Bool writing_back_more = False;

        for (Integer x = 1; x < valueof(ISA_MAX_DSTS); x = x + 1)
        begin // There is more to do if both the dest and val are valid.
          writing_back_more = writing_back_more || (isValid(dsts[x]) && isValid(wbvals[x]));
        end

        if (!writing_back_more)
        begin
        
            // We're done, so don't stall.
            res3Q.deq();

            // Update scoreboard.
            tokScoreboard.exeFinish(tok.index);

            // Return timing model. End of macro-operation (path 1).
            linkGetResults.makeResp(initFuncpRspGetResults(tok, addr, rsp.timepResult));
            debugLog.record($format("TOKEN %0d: GetResults: End (path 1).", tok.index));

        end
        else // We've got to write back more.
        begin
            
            // Log it.
            debugLog.record($format("TOKEN %0d: GetResults4: Writing back additional values.", tok.index));

            // Marshall up the values for writeback.

            Vector#(TSub#(ISA_MAX_DSTS, 1), Maybe#(Tuple2#(FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE))) remaining_values = newVector();
            for (Integer x = 1; x < valueof(ISA_MAX_DSTS) ; x = x + 1)
            begin
                remaining_values[x-1] = case (dsts[x]) matches
                                         tagged Invalid:  tagged Invalid;
                                         tagged Valid .d:
                                           case (wbvals[x]) matches 
                                              tagged Invalid:  tagged Invalid; // Not writing it now - presumably it's a load.
                                              tagged Valid .v: tagged Valid tuple2(d, v);
                                           endcase
                                     endcase;
            end

            // Stall the pipeline.
            stateRes4 <= tagged RES4_ADDITIONAL_WB
                                {
                                    remainingValues: remaining_values,
                                    result: rsp.timepResult,
                                    current: 0 
                                };
        end
      
    endrule

    // getResults4AdditionalWriteback
    
    // When:   After a result from getResults4 writes back additonal destinations.
    // Effect: Finish the writebacks of the physical register file.
    
    if(valueOf(ISA_MAX_DSTS) > 1)
    begin

        rule getResults4AdditionalWriteback (state.readyToContinue() &&& stateRes4 matches tagged RES4_ADDITIONAL_WB .wb_info);
        
            // Get the info from the previous stage.
            match {.tok, .addr} = res3Q.first();
            
            // Do the writeback.
            case (wb_info.remainingValues[wb_info.current]) matches
                tagged Invalid:
                begin
                    // Hopefully this doesn't happen too much.
                    debugLog.record($format("TOKEN %0d: GetResults4: Skipping Dest %0d", tok.index, wb_info.current + 1));

                end
                tagged Valid {.dst, .val}:
                begin

                    // An actual writeback.
                    prf.write(dst, val);
                    debugLog.record($format("TOKEN %0d: GetResults4: Writing Dest %0d (PR%0d <= 0x%x)", tok.index, wb_info.current + 1, dst, val));

                end
            endcase
      
            // We're done when we've checked every additional dest.
            if (wb_info.current == fromInteger(valueOf(ISA_MAX_DSTS) - 2))
            begin
      
                // We're done. Unstall the pipeline.
                res3Q.deq();
                stateRes4 <= tagged RES4_NORMAL;

                // Update scoreboard.
                tokScoreboard.exeFinish(tok.index);
          
                // Return to timing model. End of macro-operation (path 2).
                linkGetResults.makeResp(initFuncpRspGetResults(tok, addr, wb_info.result));
                debugLog.record($format("TOKEN %0d: GetResults: End (path 2).", tok.index));

            end
            else
            begin
            
                // We're not done. Update the state for next time.
                stateRes4 <= tagged RES4_ADDITIONAL_WB
                                    {
                                        remainingValues: wb_info.remainingValues,
                                        result: wb_info.result,
                                        current: wb_info.current + 1
                                    };
            
            end
    
        endrule
    end
    else
    begin
        //
        // Dummy rule to keep execution_order pragma below happy
        //
        rule getResults4AdditionalWriteback (True);
        endrule
    end

    
    // ******* emulateInstruction ******* //

    //    
    // When:   After the getResults operation detects an instruction which must be emulated.
    // Effect: First this sends every archtectural register value to software.
    //         Then it makes a call to emulate the instruction.
    //         Then it accepts any number of register updates from software.
    //         Finally it gets an ACK and returns the result of getResults to the timing model.

    // Multi-stage macro-operation that interacts with software via RRR.
    // This is completely unpipelined and always stalls the whole system.
    
    // Message while discovering instruction's register mappings
    FIFO#(Tuple3#(TOKEN_INDEX, Bool, Bool)) emulateRegMapQ <- mkFIFO();

    // Current token for register mapping discovery
    Reg#(TOKEN_INDEX) emulateMapCurTok <- mkRegU();

    // Done finding all register mappings?
    Reg#(Bool) emulateMapReqDone <- mkRegU();

    // Map table that reverses all register mappings from tokens younger than
    // emulated token.
    FUNCP_EMULATION_REG_MAP emulateRegMap <- mkEmulationRegMap();

    // Done writing registers to host?
    Reg#(Bool) doneSyncingRegs <- mkRegU();


    // emulateInstruction1
    //
    // When:   After getResults operation puts us in the emulation state.
    // Effect: Stall until all younger operations have completed. Then we can proceed.

    rule emulateInstruction1 (state.getState() == RSM_DrainingForEmulate && tokScoreboard.canEmulate());
        // Did the timing model do drain before correctly?
        let ctx_id = tokContextId(emulatingToken);
        assertion.expectedOldestTok(emulatingToken.index == tokScoreboard.oldest(ctx_id));
        if (emulatingToken.index != tokScoreboard.oldest(ctx_id))
            debugLog.record($format("TOKEN %0d: emulateInstruction1:  Token is not oldest! (Oldest: %0d)", emulatingToken.index, tokScoreboard.oldest(ctx_id)));
               
        emulateMapCurTok <= tokScoreboard.youngestDecoded(ctx_id);
        emulateMapReqDone <= False;
        emulateRegMap.reset();

        state.setState(RSM_EmulateGenRegMap);

        debugLog.record($format("TOKEN %0d: emulateInstruction1:  Youngest decoded token is %0d (youngest is %0d)", emulatingToken.index, tokScoreboard.youngestDecoded(ctx_id), tokScoreboard.youngest(ctx_id)));
    endrule


    //
    // emulateInstruction1_GenRegMapReq --
    //     Loop from youngest token to the token being emulated and request
    //     details of registers written by each token.  Details will be
    //     consumed by the next rule.
    //
    rule emulateInstruction1_GenRegMapReq (state.getState() == RSM_EmulateGenRegMap && ! emulateMapReqDone);
        // Look up the token properties
        regMapping.readRewindReq(emulateMapCurTok);
        tokInst.readPorts[1].readReq(emulateMapCurTok);

        // Pass it to the next stage that will generate the map table
        let done = (emulateMapCurTok == emulatingToken.index);
        let tok_active = tokScoreboard.isAllocated(emulateMapCurTok);
        emulateRegMapQ.enq(tuple3(emulateMapCurTok, tok_active, done));

        emulateMapCurTok <= emulateMapCurTok - 1;
        emulateMapReqDone <= done;
    endrule


    //
    // emulateInstruction1_GenRegMapResp --
    //     Receives information about register map changes tokens, starting with
    //     the youngest token and working back to the emulated token.  Build
    //     a map of registers from the perspective of the emulated token.
    //
    rule emulateInstruction1_GenRegMapResp (state.getState() == RSM_EmulateGenRegMap);

        match { .tok_idx, .tok_active, .done } = emulateRegMapQ.first();
        emulateRegMapQ.deq();

        let rewind_info <- regMapping.readRewindRsp();
        let inst <- tokInst.readPorts[1].readRsp();

        //
        // Note old register mappings.  We must check the freelist position for
        // its valid bit, since it is the only structure reset when the token
        // is allocated.  An invalid free list position is a clue that the
        // token has not been through decode.
        //
        if (!done && tok_active &&& rewind_info matches tagged Valid .rw)
        begin
            Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)) new_map = ?;

            for (Integer x = 0; x < valueOf(ISA_MAX_DSTS); x = x + 1)
            begin
                if (isaGetDst(inst, x) matches tagged Valid .arc_r &&&
                    rw.regsToFree[x] matches tagged Valid .r)
                begin
                    // Set the mapping back
                    new_map[x] = tagged Valid tuple2(arc_r, r);
                    debugLog.record($format("TOKEN %0d: EmulateInstruction1: Note mapping from token %0d (%0d/%0d)", emulatingToken.index, tok_idx, arc_r, r));
                end
                else
                begin
                    new_map[x] = tagged Invalid;
                end
            end

            emulateRegMap.updateMap(new_map);
        end

        // Done with map table discovery?
        if (done)
        begin
            debugLog.record($format("TOKEN %0d: EmulateInstruction1: Map discovery done", emulatingToken.index));  

            // Reset the counter for syncing registers.
            synchronizingCurReg <= minBound;
            doneSyncingRegs <= False;

            // Start syncing registers.
            state.setState(RSM_SyncingRegisters);
        end

    endrule


    // emulateInstruction2_Req
    
    // When:   After the getResults operation puts us into the emulation state, this
    //         rule happens once for each architectural register.
    // Effect: Look up the current physical register in the maptable and request it from the regfile.
    
    rule emulateInstruction2_Req (state.getState() == RSM_SyncingRegisters && ! doneSyncingRegs);
    
        // Some ISA's have a sparse packing of register names.  They should define Arith so we 
        // don't transmit them spuriously.

        // Request architectural to physical register mapping
        emulateRegMap.mapReq(synchronizingCurReg);
        regMapping.readMapReq(synchronizingCurReg);

        Bool done = False;

        // Was this our last request?
        if (synchronizingCurReg == maxBound)
        begin
            // Request the inst and current PC
            tokInst.readPorts[0].readReq(emulatingToken.index);
            tokAddr.readReq(emulatingToken.index);

            done = True;
        end
        
        // Pass messages to prf request and value forwarding rules
        syncPRFReqQ.enq(synchronizingCurReg);
        syncQ.enq(tuple2(done, synchronizingCurReg));
        
        // Increment, and possibly repeat.
        synchronizingCurReg <= synchronizingCurReg + 1;
        doneSyncingRegs <= done;

    endrule


    // emulateInstruction2_PRFReq
    //
    // When:   After each occurance of emulateInstruction2_Req
    // Effect: Complete map from architectural register to physical register.  Request
    //         the physical register's value.
    //
    rule emulateInstruction2_PRFReq (True);
    
        let ar = syncPRFReqQ.first();
        syncPRFReqQ.deq();

        let emulate_map_pr <- emulateRegMap.mapResp(ar);
        let map_pr <- regMapping.readMapRsp();

        FUNCP_PHYSICAL_REG_INDEX pr = ?;
        if (emulate_map_pr matches tagged Valid .emul_pr)
            // Found a changed mapping during the token walk
            pr = emul_pr;
        else
            // Mapping hasn't changed since emulated instruction was decoded
            pr = map_pr;
    
        // Make the request to the regfile.
        prf.readReq(pr);

        //Log it.
        debugLog.record($format("TOKEN %0d: EmulateInstruction2: Reading Register R%0d (PR%0d).", emulatingToken.index, ar, pr));

    endrule


    // emulateInstruction2_Rsp
    
    // When:   After each occurance of emulateInstruction2_Req
    // Effect: Get the register value response and send it on to software via RRR.

    rule emulateInstruction2_Rsp (True);
    
        // Get the register from the previous stage.
        match {.done, .arch_reg} = syncQ.first();
        syncQ.deq();
        
        // Get the register value from the regfile.
        let reg_val <- prf.readRsp();
        
        // Send the regsiter on to software via RRR
        client_stub.makeRequest_sync(tuple2(arch_reg, reg_val));

        if (done)
        begin
            // End the loop.
            state.setState(RSM_RequestingEmulation);
        end
        
        //Log it.
        debugLog.record($format("TOKEN %0d: EmulateInstruction2: Transmitting Register R%0d = 0x%h.", emulatingToken.index, arch_reg, reg_val));
    
    endrule
    
    // emulateInstruction3
    
    // When:   After emulateInstruction1 has transmitted every architectural register.
    // Effect: Send the instruction emulation request to software via RRR.

    rule emulateInstruction3 (state.getState() == RSM_RequestingEmulation);
        
        // Get the instruction and current pc
        ISA_INSTRUCTION inst <- tokInst.readPorts[0].readRsp();
        ISA_ADDRESS       pc <- tokAddr.readRsp();
        
        emulatingPC <= pc;

        // Send the request on to software via RRR
        client_stub.makeRequest_emulate(tuple2(inst, pc));
        
        //Log it.
        debugLog.record($format("TOKEN %0d: EmulateInstruction3: Requesting Emulation of inst 0x%h from address 0x%h", emulatingToken.index, inst, pc));
        stat_isa_emul.incr();

        //Go to receiving updates.
        state.setState(RSM_UpdatingRegisters);

    endrule

    // emulateInstruction3_UpdateReg
    //
    // When:   Whenever the software decides that it should update a register in hardware.
    //         These updates should really only occur when we're emulating an instruction.
    //         If they come during any other time then this is a fatal error.
    // Effect: Request physical register mapping and forward details to
    //         emualteInstruction3_UpdateRegWrite.
    //
    rule emulateInstruction3_UpdateReg (True);
        
        // Get an update request from software.
        match {.ar, .v} <- server_stub.acceptRequest_updateRegister();
        
        // Assert that we're in the state we expected to be in.
        assertion.regUpdateAtExpectedTime(state.getState() == RSM_UpdatingRegisters);

        // Request architectural to physical register mapping
        emulateRegMap.mapReq(ar);
        regMapping.readMapReq(ar);

        updateRegQ.enq(tuple2(ar, v));

    endrule

    //
    // emulateInstruction3_UpdateRegWrite --
    //     Receive register mapping and values from previous rules.  Write value
    //     to physical register.
    //
    rule emulateInstruction3_UpdateRegWrite (True);
        
        match {.ar, .v} = updateRegQ.first();
        updateRegQ.deq();

        let emulate_map_pr <- emulateRegMap.mapResp(ar);
        let map_pr <- regMapping.readMapRsp();

        FUNCP_PHYSICAL_REG_INDEX pr = ?;
        if (emulate_map_pr matches tagged Valid .emul_pr)
            // Found a changed mapping during the token walk
            pr = emul_pr;
        else
            // Mapping hasn't changed since emulated instruction was decoded
            pr = map_pr;

        // Update the regfile.
        prf.write(pr, v);

        // Log it.
        debugLog.record($format("TOKEN %0d: EmulateInstruction3: Writing ((%0d/PR%0d) <= 0x%h)", emulatingToken.index, ar, pr, v));
    
    endrule

    // emulateInstruction4
    
    // When:   After the software has finished all of its register writes it will send an ACK.
    // Effect: This means the emulation is complete. Resume normal operations.
    //         Return a NOP to the timing model.

    rule emulateInstruction4 (True);
        
        // Get the ACK from software that they're complete.
        let newPc <- client_stub.getResponse_emulate();
        
        // Assert that we're in the state we expected to be in.
        assertion.emulationFinishedAtExpectedTime(state.getState() == RSM_UpdatingRegisters);
        
        // We are no longer emulating an instruction.
        // Resume normal operations.
        state.setState(RSM_Running);

        // Update scoreboard.
        tokScoreboard.exeFinish(emulatingToken.index);

        // Hack alert -- until RRR allows us to pass multiple objects cleanly
        // we pass a branch target and flags as a single 64 bit value.  We use
        // the low 2 bits as flags.  This works for Alpha and MIPS but won't
        // work for x86.
        let tgtFlags = newPc[1:0];
        newPc[1:0] = 0;                         // Clear the flags
        let resp = case(tgtFlags)
                       0: tagged RNop;
                       1: tagged RBranchTaken newPc;
                       2: tagged RNop;          // Unused
                       3: tagged RTerminate (newPc[2] == 1); // Bit 2 is 1 for pass
                   endcase;

        //Log it
        debugLog.record($format("TOKEN %0d: EmulateInstruction3: Emulation finished.", emulatingToken.index));
  
        // Send the response to the timing model.
        // End of macro-operation.
        linkGetResults.makeResp(initFuncpRspGetResults(emulatingToken, emulatingPC, resp));

        debugLog.record($format("TOKEN %0d: GetResults: End (path 3).", emulatingToken.index));

    endrule

endmodule
