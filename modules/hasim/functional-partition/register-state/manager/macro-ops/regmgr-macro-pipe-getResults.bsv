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
import GetPut::*;
import Connectable::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_services_lib.bsh"
`include "asim/provides/soft_services_deps.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/debug_scan_service.bsh"
 
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"

// Dictionary includes
`include "asim/dict/PARAMS_FUNCP_REGSTATE_MANAGER.bsh"

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

//
// States for getResults emulation pipeline stages.
//
typedef enum
{
    RSM_RES_Running,
    RSM_RES_DrainingForEmulate,
    RSM_RES_EmulateGenRegMap,
    RSM_RES_SyncingRegisters,
    RSM_RES_RequestingEmulation,
    RSM_RES_UpdatingRegisters,
    RSM_RES_FinishingEmulation
}
REGMGR_RES_STATE_ENUM
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
    BROM#(TOKEN_INDEX, ISA_ADDRESS) tokAddr,
    BRAM#(TOKEN_INDEX, ISA_INST_SRCS) tokWriters,
    BROM#(TOKEN_INDEX, REGMGR_DST_REGS) tokDsts,
    BROM#(TOKEN_INDEX, ISA_INSTRUCTION) tokInst,
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_ADDRESS) tokMemAddr,
    BRAM#(TOKEN_INDEX, ISA_VALUE) tokStoreValue)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_getResults.out");
    STDIO#(Bit#(32)) stdio <- mkStdIO_Debug();
    let msgSendToDP <- getGlobalStringUID("FUNCP GETRESULTS: send to dp TOKEN (%d, %d)\n");
    let msgRecvFromDP <- getGlobalStringUID("FUNCP GETRESULTS: recv from dp TOKEN (%d, %d)\n");
    let msgPRFWrite <- getGlobalStringUID("FUNCP GETRESULTS: prf write TOKEN (%d, %d) prf %d\n");
    let msgEmulStart <- getGlobalStringUID("FUNCP GETRESULTS: emulate start TOKEN (%d, %d)\n");
    let msgEmulDone <- getGlobalStringUID("FUNCP GETRESULTS: emulate done TOKEN (%d, %d)\n");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_GET_RESULTS, 
                       FUNCP_RSP_GET_RESULTS) linkGetResults <- mkConnection_Server("funcp_getResults");

    Connection_Client#(FUNCP_ISA_DATAPATH_REQ,
                       FUNCP_ISA_DATAPATH_RSP) linkToDatapath <- mkConnection_Client("isa_datapath");

    Connection_Receive#(FUNCP_ISA_WRITEBACK) linkISAWritebacks <- mkConnection_Receive("isa_datapath_writeback");

    // Send linkISAWritebacks through a local FIFO to relax timing
    FIFO#(FUNCP_ISA_WRITEBACK) writebacks <- mkFIFO();
    mkConnection(toGet(linkISAWritebacks), toPut(writebacks));

    // Emulation RRR Stubs
    ClientStub_ISA_EMULATOR emul_client_stub <- mkClientStub_ISA_EMULATOR();
    ServerStub_ISA_EMULATOR emul_server_stub <- mkServerStub_ISA_EMULATOR();


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
   
    // Counters to track when contexts should be sleeping.
    LUTRAM#(CONTEXT_ID, Bit#(32)) contextSleeping <- mkLUTRAM(0);
    
    // Dynamic parameter with the sleep interval.
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();
    Param#(32) paramSleepInterval <- mkDynamicParameter(`PARAMS_FUNCP_REGSTATE_MANAGER_SLEEP_INTERVAL, paramNode);

    // Intermediate state between pipeline stages. Bool indicates if there's a bubble.
    FIFOF#(Tuple2#(TOKEN, Bool)) res1Q <- mkFIFOF();
    FIFOF#(Tuple2#(TOKEN, Bool)) res2Q <- mkFIFOF();
    FIFOF#(Tuple3#(TOKEN, Bool, ISA_ADDRESS)) res3Q   <- mkFIFOF();
    FIFOF#(ISA_REG_INDEX) syncPRFReqQ <- mkFIFOF();
    FIFOF#(Tuple2#(Bool, ISA_REG_INDEX)) syncQ <- mkFIFOF();
    FIFOF#(Tuple2#(ISA_REG_INDEX, ISA_VALUE)) updateRegQ <- mkFIFOF();

    // Which token's instruction are we emulating?
    Reg#(TOKEN) emulatingToken <- mkRegU();
    // PC of emulating token
    Reg#(ISA_ADDRESS) emulatingPC <- mkRegU();
    // Which register are we currently synchronizing?
    Reg#(ISA_REG_INDEX) synchronizingCurReg <- mkReg(minBound);

    Reg#(REGMGR_RES_STATE_ENUM) state_res <- mkReg(RSM_RES_Running);

    STAT stat_isa_emul <- mkStatCounter(statName("REGMGR_GETRESULTS_EMULATED_INSTRS",
                                                 "FUNCP: Emulated Instructions"));

    RWire#(Tuple2#(Bool, Bool)) getResults1DbgData <- mkRWire();
    rule getResults1Dbg (True);
        let tok = linkGetResults.getReq().token;
        getResults1DbgData.wset(tuple2(state.readyToBegin(tokContextId(tok)),
                                       tokScoreboard.canStartExe(tok.index)));
    endrule

    RWire#(Tuple2#(TOKEN_INDEX, Bool)) res3QDbgData <- mkRWire();
    rule getRes3QDbg (True);
        match {.tok, .need_rsp, .addr} = res3Q.first();
        res3QDbgData.wset(tuple2(tok.index, need_rsp));
    endrule

    let res1_dbg_data = getResults1DbgData.wget();

    let res3_dbg_data = res3QDbgData.wget();
    Maybe#(TOKEN_INDEX) res_3Q_dbg_tokidx = (isValid(res3_dbg_data) ?
                                             tagged Valid tpl_1(validValue(res3_dbg_data)) :
                                             tagged Invalid);
    Bool res_3Q_dbg_need_rsp = isValid(res3_dbg_data) && tpl_2(validValue(res3_dbg_data));

    DEBUG_SCAN_FIELD_LIST dbg_list = List::nil;
    dbg_list <- addDebugScanField(dbg_list, "State", state_res);
    dbg_list <- addDebugScanField(dbg_list, "stage1ReadyToBegin", isValid(res1_dbg_data) && tpl_1(validValue(res1_dbg_data)));
    dbg_list <- addDebugScanField(dbg_list, "stage1CanStartExe", isValid(res1_dbg_data) && tpl_2(validValue(res1_dbg_data)));
    dbg_list <- addDebugScanField(dbg_list, "stage1HaveToken", isValid(res1_dbg_data));
    dbg_list <- addDebugScanField(dbg_list, "res1QnotFull", res1Q.notFull);
    dbg_list <- addDebugScanField(dbg_list, "res1QnotEmpty", res1Q.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "res2QnotFull", res2Q.notFull);
    dbg_list <- addDebugScanField(dbg_list, "res2QnotEmpty", res2Q.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "res3QneedRsp", res_3Q_dbg_need_rsp);
    dbg_list <- addDebugScanMaybeField(dbg_list, "res3QtokIdx", res_3Q_dbg_tokidx);
    dbg_list <- addDebugScanField(dbg_list, "res3QnotFull", res3Q.notFull);
    dbg_list <- addDebugScanField(dbg_list, "res3QnotEmpty", res3Q.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "syncPRFReqQnotFull", syncPRFReqQ.notFull);
    dbg_list <- addDebugScanField(dbg_list, "syncPRFReqQnotEmpty", syncPRFReqQ.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "syncQnotFull", syncQ.notFull);
    dbg_list <- addDebugScanField(dbg_list, "syncQnotEmpty", syncQ.notEmpty);
    dbg_list <- addDebugScanField(dbg_list, "updateRegQnotFull", updateRegQ.notFull);
    dbg_list <- addDebugScanField(dbg_list, "updateRegQnotEmpty", updateRegQ.notEmpty);

    let dbgNode <- mkDebugScanNode("FUNCP REGMGR getResults", dbg_list);


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

    (* conservative_implicit_conditions *)
    rule getResults1 (linkGetResults.getReq().token matches .tok &&&
                      state.readyToBegin(tokContextId(tok)) &&&
                      tokScoreboard.canStartExe(tok.index) &&&
                      state_res == RSM_RES_Running);

        // Get parameter from the timing model. Begin macro-operation.
        let req = linkGetResults.getReq();
        linkGetResults.deq();
        debugLog.record(fshow(tok.index) + $format(": GetResults: Begin."));

        // Token active or was it killed?
        let tok_active = !tokIsDummy(tok);

        if (tokScoreboard.emulateInstruction(tok.index) && tok_active)
        begin
            // If we were sleeping, we just decrement the counter.
            // Otherwise we actually emulate the instruction.
            
            let sleep_val = contextSleeping.sub(tokContextId(tok));
            
            if (sleep_val != 0)
            begin
                // Just sleeping.
                contextSleeping.upd(tokContextId(tok), sleep_val - 1);
                
                // Update the scoreboard.
                tokScoreboard.exeStart(tok.index);

                res1Q.enq(tuple2(tok, False));
                
            end
            else
            begin

                // Emulate the instruction.
                state.setEmulate(tokContextId(tok));

                // Record that we're emulating an instruction.
                state_res <= RSM_RES_DrainingForEmulate;

                // Record which token is being emulated.
                emulatingToken <= tok;

                 // Log it.
                debugLog.record(fshow(tok.index) + $format(": GetResults1: Beginning Instruction Emulation."));

            end

        end
        else
        begin
        
            // Update the scoreboard.
            tokScoreboard.exeStart(tok.index);
        
            // Look up the writers.
            tokWriters.readReq(tok.index);

            // Pass it along to the next stage.
            res1Q.enq(tuple2(tok, True));
        
        end

    endrule

    // getResults2
    // When:   After getResults1.
    // Effect: Use the writers to look up values from the PRF. 
    //         Also retreive the instruction itself and the PC.
    //         If the writers are not all ready then a stall can occur.

    rule getResults2 (res1Q.first() matches {.tok, .need_res} &&& need_res &&& state.readyToContinue());

        // Get input from getResults1.
        res1Q.deq();
        let tok_active = !tokIsDummy(tok);

        // Response from previous stage.
        let ws <- tokWriters.readRsp();
        
        // We let junk proceed
        if (! tok_active)
        begin
            // No values are needed for junk
            debugLog.record(fshow(tok.index) + $format(": GetResults2: Letting Junk Proceed!"));

            prf.readRegVecReq(tok, Vector::replicate(tagged Invalid));
        end
        else
        begin
            debugLog.record(fshow(tok.index) + $format(": GetResults2: Requesting srcs"));

            prf.readRegVecReq(tok, ws);
        end

        tokAddr.readReq(tok.index);
        tokInst.readReq(tok.index);
        tokDsts.readReq(tok.index);

        res2Q.enq(tuple2(tok, True));

    endrule

    rule getResults2Bubble (res1Q.first() matches {.tok, .need_res} &&& !need_res &&& state.readyToContinue());
    
        res1Q.deq();
        tokAddr.readReq(tok.index);
        res2Q.enq(tuple2(tok, False));
        
    endrule
    
    // getResults3
    // When:    After getResults2 or alternatively getResults2StallEnd
    // Effect:  Send all the data to the datapath.

    rule getResults3 (res2Q.first() matches {.tok, .need_rsp} &&& need_rsp &&& state.readyToContinue());

        // Get input from the previous stage.
        res2Q.deq();

        // Get all the data the previous stage kicked off.
        let addr <- tokAddr.readRsp();
        let inst <- tokInst.readRsp();
        let dsts <- tokDsts.readRsp();

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": GetResults3: Sending to Datapath."));
        stdio.printf(msgSendToDP, list2(zeroExtend(tokContextId(tok)),
                                        zeroExtend(tokTokenId(tok))));

        // Send it to the datapath.
        linkToDatapath.makeReq(initISADatapathReq(tok, inst, addr, dsts.pr));

        // Pass it to the next stage.
        res3Q.enq(tuple3(tok, True, addr));

    endrule

    rule getResults3Bubble (res2Q.first() matches {.tok, .need_res} &&& !need_res &&& state.readyToContinue());
    
        res2Q.deq();
        let addr <- tokAddr.readRsp();
        res3Q.enq(tuple3(tok, False, addr));
        
    endrule
    
    // getResults4
    // When:   After getResults3 and the datapath returns the result.
    // Effect: Forward the response from the ISA datapath to the timing model.
    //         This rule does not update the physical registers.  That is
    //         handled by getResultsWritebacks below.
    //
    //         The pipeline can safely return a message to the timing model before
    //         register writebacks are complete because functional pipelines
    //         that depenend on the register values will block until a
    //         register's valid bit is set in getResultsWritebacks.
    //
    rule getResults4 (res3Q.first() matches {.tok, .need_rsp, .addr} &&& need_rsp &&& state.readyToContinue());

        // Get the token from the previous stage.
        res3Q.deq();

        // Get the response from the datapath.
        let rsp = linkToDatapath.getResp();
        linkToDatapath.deq();

        // Tag illegal instruction.  An error will be raised on attempts to commit.
        if (rsp.except != FUNCP_ISA_EXCEPT_NONE)
        begin
            debugLog.record(fshow(tok.index) + $format(": GetResults: Illegal instruction"));
            tokScoreboard.setFault(tok.index, FAULT_ILLEGAL_INSTR);
        end

        // Update the memaddress (only useful for loads/stores)
        if (rsp.timepResult matches tagged REffectiveAddr .ea)
            tokMemAddr.write(tok.index, ea);

        // Update scoreboard.
        tokScoreboard.exeFinish(tok.index);

        // Return timing model. End of macro-operation (path 1).
        linkGetResults.makeResp(initFuncpRspGetResults(tok, addr, rsp.timepResult));
        debugLog.record(fshow(tok.index) + $format(": GetResults: End (path 1)."));
        stdio.printf(msgRecvFromDP, list2(zeroExtend(tokContextId(tok)),
                                          zeroExtend(tokTokenId(tok))));
    endrule

    rule getResults4Bubble (res3Q.first() matches {.tok, .need_res, .addr} &&& !need_res &&& state.readyToContinue());
    
        res3Q.deq();

        // Update scoreboard.
        tokScoreboard.setAllDestsValid(tok.index);
        tokScoreboard.exeFinish(tok.index);

        // Return timing model. End of macro-operation (path 2).
        linkGetResults.makeResp(initFuncpRspGetResults(tok, addr, tagged RBranchTaken addr));
        
    endrule

    //
    // getResultsWritebacks --
    //     This rule is independent of the getResults pipeline.  It receives
    //     writeback messages from the ISA datapath.  The first writeback for
    //     a store is assumed to be the store value.  A token is marked complete
    //     when the tokDone bit is sent in the writebacks message.
    //
    rule getResultsWritebacks (True);
        let wb = writebacks.first();
        writebacks.deq();

        let tok = wb.token;

        // By convention, the first writeback for a store is the store
        // value.  Handle it specially.
        if (tokScoreboard.isStore(tok.index) &&
           ! tokScoreboard.isStoreDataValid(tok.index))
        begin
            // Stores write dest0 into the token table instead of the PRF.
            tokStoreValue.write(tok.index, wb.value);
            tokScoreboard.setStoreDataValid(tok.index);
            debugLog.record(fshow(tok.index) + $format(": GetResultsWB: Writing STORE val 0x%x", wb.value));
        end
        else if (wb.physDst matches tagged Valid .pr)
        begin
            // Normal physical register update
            prf.write(tok, pr, wb.value);
            debugLog.record(fshow(tok.index) + $format(": GetResultsWB: Writing (PR%0d <= 0x%x)", pr, wb.value));
            stdio.printf(msgPRFWrite, list3(zeroExtend(tokContextId(tok)),
                                            zeroExtend(tokTokenId(tok)),
                                            zeroExtend(pr)));
        end

        // All writebacks complete for token?
        if (wb.tokDone)
        begin
            tokScoreboard.setAllDestsValid(tok.index);
            debugLog.record(fshow(tok.index) + $format(": GetResultsWB: Done"));
        end
    endrule


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

    rule emulateInstruction1 ((state_res == RSM_RES_DrainingForEmulate) && tokScoreboard.canEmulate(emulatingToken.index));
        // Did the timing model do drain before correctly?
        let ctx_id = tokContextId(emulatingToken);
        assertion.expectedOldestTok(emulatingToken.index == tokScoreboard.oldest(ctx_id));
        if (emulatingToken.index != tokScoreboard.oldest(ctx_id))
            debugLog.record(fshow(emulatingToken.index) + $format(": emulateInstruction1:  Token is not oldest! (Oldest: %0d)", tokScoreboard.oldest(ctx_id)));
               
        // Update the scoreboard.
        tokScoreboard.exeStart(emulatingToken.index);
        
        emulateMapCurTok <= tokScoreboard.youngest(ctx_id);
        emulateMapReqDone <= False;
        emulateRegMap.reset();

        state_res <= RSM_RES_EmulateGenRegMap;

        debugLog.record(fshow(emulatingToken.index) + $format(": emulateInstruction1:  Youngest token is %0d.", tokScoreboard.youngest(ctx_id)));
    endrule


    //
    // emulateInstruction1_GenRegMapReq --
    //     Loop from youngest token to the token being emulated and request
    //     details of registers written by each token.  Details will be
    //     consumed by the next rule.
    //
    rule emulateInstruction1_GenRegMapReq ((state_res == RSM_RES_EmulateGenRegMap) && ! emulateMapReqDone);
        // Look up the token properties
        regMapping.readRewindReq(emulateMapCurTok);
        tokInst.readReq(emulateMapCurTok);

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
    (* conservative_implicit_conditions *)
    rule emulateInstruction1_GenRegMapResp (state_res == RSM_RES_EmulateGenRegMap);

        match { .tok_idx, .tok_active, .done } = emulateRegMapQ.first();
        emulateRegMapQ.deq();

        let rewind_info <- regMapping.readRewindRsp();
        let inst <- tokInst.readRsp();

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
                    debugLog.record(fshow(emulatingToken.index) + $format(": EmulateInstruction1: Note mapping from token %0d (%0d/%0d)", tok_idx, arc_r, r));
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
            debugLog.record(fshow(emulatingToken.index) + $format(": EmulateInstruction1: Map discovery done"));  

            // Reset the counter for syncing registers.
            synchronizingCurReg <= minBound;
            doneSyncingRegs <= False;

            // Start syncing registers.
            state_res <= RSM_RES_SyncingRegisters;
        end

    endrule


    // emulateInstruction2_Req
    
    // When:   After the getResults operation puts us into the emulation state, this
    //         rule happens once for each architectural register.
    // Effect: Look up the current physical register in the maptable and request it from the regfile.
    
    rule emulateInstruction2_Req ((state_res == RSM_RES_SyncingRegisters) && ! doneSyncingRegs);
    
        // Some ISA's have a sparse packing of register names.  They should define Arith so we 
        // don't transmit them spuriously.

        // Request architectural to physical register mapping
        emulateRegMap.mapReq(synchronizingCurReg);
        regMapping.readMapReq(tokContextId(emulatingToken), synchronizingCurReg);

        Bool done = False;

        // Was this our last request?
        if (synchronizingCurReg == maxBound)
        begin
            // Request the inst and current PC
            tokInst.readReq(emulatingToken.index);
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
        prf.readReq(emulatingToken, pr);

        //Log it.
        debugLog.record(fshow(emulatingToken.index) + $format(": EmulateInstruction2: Reading Register R%0d (PR%0d).", ar, pr));

    endrule


    // emulateInstruction2_Rsp
    
    // When:   After each occurance of emulateInstruction2_Req
    // Effect: Get the register value response and send it on to software via RRR.

    rule emulateInstruction2_Rsp (state_res == RSM_RES_SyncingRegisters);
    
        // Get the register from the previous stage.
        match {.done, .arch_reg} = syncQ.first();
        syncQ.deq();
        
        // Get the register value from the regfile.
        let reg_val <- prf.readRsp();
        
        // Send the regsiter on to software via RRR
        emul_client_stub.makeRequest_sync(contextIdToRRR(tokContextId(emulatingToken)),
                                          zeroExtend(pack(arch_reg)),
                                          reg_val);

        if (done)
        begin
            // End the loop.
            state_res <= RSM_RES_RequestingEmulation;
        end
        
        //Log it.
        debugLog.record(fshow(emulatingToken.index) + $format(": EmulateInstruction2: Transmitting Register R%0d = 0x%h.", arch_reg, reg_val));
    
    endrule
    
    // emulateInstruction3
    
    // When:   After emulateInstruction1 has transmitted every architectural register.
    // Effect: Send the instruction emulation request to software via RRR.

    rule emulateInstruction3 (state_res == RSM_RES_RequestingEmulation);
        
        // Get the instruction and current pc
        ISA_INSTRUCTION inst <- tokInst.readRsp();
        ISA_ADDRESS       pc <- tokAddr.readRsp();
        
        emulatingPC <= pc;

        // Send the request on to software via RRR
        emul_client_stub.makeRequest_emulate(contextIdToRRR(tokContextId(emulatingToken)),
                                             inst, pc);
        
        //Log it.
        debugLog.record(fshow(emulatingToken.index) + $format(": EmulateInstruction3: Requesting Emulation of inst 0x%h from address 0x%h", inst, pc));
        stdio.printf(msgEmulStart, list2(zeroExtend(tokContextId(emulatingToken)),
                                         zeroExtend(tokTokenId(emulatingToken))));

        stat_isa_emul.incr();

        //Go to receiving updates.
        state_res <= RSM_RES_UpdatingRegisters;

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
        let upd <- emul_server_stub.acceptRequest_updateRegister();
        CONTEXT_ID ctx_id = truncate(upd.ctxId);
        ISA_REG_INDEX ar = unpack(truncate(upd.rName));
        ISA_VALUE v = upd.rValue;
        
        // Request architectural to physical register mapping
        emulateRegMap.mapReq(ar);
        regMapping.readMapReq(tokContextId(emulatingToken), ar);

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
        prf.write(emulatingToken, pr, v);

        // Assert that we're in the state we expected to be in.
        assertion.regUpdateAtExpectedTime(state_res == RSM_RES_UpdatingRegisters);

        // Protocol requires that the host send AR 0 last so all registers are
        // guaranteed updated before emulation finishes.
        if (ar == 0)
        begin
            state_res <= RSM_RES_FinishingEmulation;
        end

        // Log it.
        debugLog.record(fshow(emulatingToken.index) + $format(": EmulateInstruction3: Writing ((%0d/PR%0d) <= 0x%h)", ar, pr, v));
    
    endrule

    // emulateInstruction4
    
    // When:   After the software has finished all of its register writes it will send an ACK.
    // Effect: This means the emulation is complete. Resume normal operations.
    //         Return a NOP to the timing model.

    rule emulateInstruction4 (state_res == RSM_RES_FinishingEmulation);
        
        // Get the ACK from software that they're complete.
        ISA_ADDRESS newPc <- emul_client_stub.getResponse_emulate();
        
        // We are no longer emulating an instruction.
        // Resume normal operations.
        state_res <= RSM_RES_Running;
        state.clearEmulate();

        // Update scoreboard.
        tokScoreboard.exeFinish(emulatingToken.index);
        tokScoreboard.setAllDestsValid(emulatingToken.index);

        // Hack alert -- until RRR allows us to pass multiple objects cleanly
        // we pass a branch target and flags as a single 64 bit value.  We use
        // the low 2 bits as flags.  This works for Alpha and MIPS but won't
        // work for x86.
        let tgtFlags = newPc[1:0];
        newPc[1:0] = 0;                         // Clear the flags
        FUNCP_ISA_EXECUTION_RESULT resp = tagged RNop; 
        case (tgtFlags)
            0: 
            begin
                resp = tagged RNop;
            end
            1: 
            begin
                resp = tagged RBranchTaken newPc;
            end
            2: 
            begin
                resp = tagged RBranchTaken emulatingPC;         // Sleep
                contextSleeping.upd(tokContextId(emulatingToken), paramSleepInterval);
            end
            3:
            begin
                resp = tagged RTerminate (newPc[2] == 1); // Bit 2 is 1 for pass
            end
        endcase

        //Log it
        debugLog.record(fshow(emulatingToken.index) + $format(": EmulateInstruction3: Emulation finished."));
        stdio.printf(msgEmulDone, list2(zeroExtend(tokContextId(emulatingToken)),
                                        zeroExtend(tokTokenId(emulatingToken))));
  
        // Send the response to the timing model.
        // End of macro-operation.
        linkGetResults.makeResp(initFuncpRspGetResults(emulatingToken, emulatingPC, resp));

        debugLog.record(fshow(emulatingToken.index) + $format(": GetResults: End (path 3)."));

    endrule

endmodule
