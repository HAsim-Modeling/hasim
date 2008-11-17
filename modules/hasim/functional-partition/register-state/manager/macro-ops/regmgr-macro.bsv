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

//
// Register state manager for macro instructions.  This module instantiates
// the pipelines that provide the functional interface to the timing
// model.
//

// Library includes.

import FIFO::*;
import Vector::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_modellib.bsh"
 
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"
`include "asim/provides/funcp_regstate_scoreboard.bsh"
`include "asim/provides/funcp_regstate_freelist.bsh"
`include "asim/provides/funcp_regstate_snapshot.bsh"
`include "asim/provides/funcp_memstate_manager.bsh"
`include "asim/provides/funcp_memory.bsh"

// ISA includes

`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/hasim_isa_datapath.bsh"


// mkFUNCP_RegStateManager

// The manager of the register state, and the bulk of the work of the functional partition.

module [HASIM_MODULE] mkFUNCP_RegStateManager
    //interface:
                ();

    // ====================================================================
    //
    //   Register state manager global storage
    //
    // ====================================================================

    // Tables to track info about in-flight instructions.

    // The address we got the instruction from (told to us by the timing model).
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokAddr <- mkLiveTokenBRAM();

    // The physical address(es) for the instruction.
    BRAM#(TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalAddrs <- mkLiveTokenBRAM();

    // The instruction that was at that address (from mem_state).
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_INSTRUCTION) tokInst <- mkLiveTokenBRAMMultiRead();

    // The destinations of the instruction (a convenience which saves us from reading the instruction/maptable).
    BRAM_MULTI_READ#(3, TOKEN_INDEX, ISA_INST_DSTS) tokDsts <- mkLiveTokenBRAMMultiRead();

    // If an instruction has sources in other inflight instructions it will be noted here.
    BRAM#(TOKEN_INDEX, ISA_INST_SRCS) tokWriters <- mkLiveTokenBRAM();

    // The memaddress is used by Loads/Stores so we don't have to repeat the calculation.
    BRAM#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr <- mkLiveTokenBRAM();

    // The value a store will write to memory
    BRAM#(TOKEN_INDEX, ISA_VALUE) tokStoreValue <- mkLiveTokenBRAM();

    // The physical memaddress(es) for the instruction.
    BRAM_MULTI_READ#(2, TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalMemAddrs <- mkLiveTokenBRAMMultiRead();

    // Position of freelist for token's physical regs.  Used by rewind.
    BRAM#(TOKEN_INDEX, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) tokFreeListPos <- mkLiveTokenBRAM();

    // The physical registers to free when the token is committed/killed.
    BRAM#(TOKEN_INDEX, ISA_INST_DSTS) tokRegsToFree <- mkLiveTokenBRAM();

    // The Physical Register File

    BRAM_MULTI_READ#(3, FUNCP_PHYSICAL_REG_INDEX, ISA_VALUE) prf <- mkBRAMMultiRead();
    
    // Valid bits for PRF
    Vector#(FUNCP_NUM_PHYSICAL_REGS, Reg#(Bool)) prfValids = newVector();
    
    for (Integer x = 0; x < valueOf(FUNCP_NUM_PHYSICAL_REGS); x = x + 1)
    begin
      prfValids[x] <- mkReg(True);
    end

    // The Map Table

    // This gets pounded nearly every FPGA cycle, so it's NOT in RAM.
    // Also this lets us snapshot/reload the entire maptable in a single cyle.

    // The highest register in the ISA (the last one which is initially valid).
    ISA_REG_INDEX         highestReg = maxBound;
    FUNCP_PHYSICAL_REG_INDEX maxInit = zeroExtend(pack(highestReg));

    // The initial map is that all architectural registers are mapped 1-to-1 to
    // physical registers and are all valid.

    Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX) initMap = newVector();
    
    // Note: this loop ends at _architectural_ register size.
    
    for (Integer x  = 0; x < valueof(ISA_NUM_REGS); x = x + 1)
    begin
      initMap[x] = fromInteger(x);
    end

    Reg#(Vector#(ISA_NUM_REGS, FUNCP_PHYSICAL_REG_INDEX)) maptable   <- mkReg(initMap);

    // ******* High-Level FSM State *******

    // The epoch tells us when to discard junk tokens that were in flight when
    // the timing partition killed them.
    Reg#(TOKEN_BRANCH_EPOCH) branchEpoch <- mkReg(0);

    // The fault epoch tells us when to discard junk tokens that were in flight
    // killed by the timing partition's fault handler.
    Reg#(TOKEN_FAULT_EPOCH) faultEpoch <- mkReg(0);
     
    // This queue records where load responses should be sent.
    FIFO#(MEM_PATH) memPathQ <- mkSizedFIFO(16);

    // ====================================================================
    //
    //   Shared soft connections
    //
    // ====================================================================

    // Connection to TLB
    Connection_Client#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP) link_itlb <- mkConnection_Client("funcp_itlb");
    Connection_Client#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP) link_dtlb <- mkConnection_Client("funcp_dtlb");

    Connection_Client#(MEMSTATE_REQ, MEM_VALUE) linkToMem <- mkConnection_Client("funcp_memstate");

    // ====================================================================
    //
    //   Submodules holding global state
    //
    // ====================================================================

    // The Token State is a big scoreboard which tracks the status of inflight tokens.
    FUNCP_SCOREBOARD tokScoreboard <- mkFUNCP_Scoreboard();

    // The Freelist tracks which physical registers are available.
    FUNCP_FREELIST freelist <- mkFUNCP_Freelist(`REGSTATE_LOGFILE_PREFIX);

    // The Snapshots allow for fast rewinds.
    FUNCP_SNAPSHOT snapshots <- mkFUNCP_Snapshot();

    // A state variable to indicate what we're doing on a high-level.
    REGMANAGER_STATE state <- mkRegmanagerState(RSM_Initializing);

    
    // ====================================================================
    //
    //   Pipeline submodules (connections to timing module)
    //
    // ====================================================================

    let newInFlight <- mkFUNCP_RegMgrMacro_Pipe_NewInFlight(
                            state,
                            tokScoreboard,
                            snapshots,
                            tokFreeListPos,
                            branchEpoch,
                            faultEpoch);

    let doITranslate <- mkFUNCP_RegMgrMacro_Pipe_DoITranslate(
                            state,
                            tokScoreboard,
                            tokAddr,
                            tokPhysicalAddrs,
                            link_itlb);

    let getInstruction <- mkFUNCP_RegMgrMacro_Pipe_GetInstruction(
                            state,
                            tokScoreboard,
                            tokPhysicalAddrs,
                            tokInst,
                            linkToMem,
                            memPathQ);
    
    let getDependencies <- mkFUNCP_RegMgrMacro_Pipe_GetDependencies(
                            state,
                            tokScoreboard,
                            snapshots,
                            freelist,
                            tokFreeListPos,
                            prfValids,
                            maptable,
                            tokRegsToFree,
                            tokWriters,
                            tokDsts,
                            tokInst);

    let getResults <- mkFUNCP_RegMgrMacro_Pipe_GetResults(
                            state,
                            tokScoreboard,
                            tokAddr,
                            snapshots,
                            prfValids,
                            prf,
                            tokWriters,
                            tokDsts,
                            tokInst,
                            tokMemAddr,
                            tokStoreValue);

    let doDTranslate <- mkFUNCP_RegMgrMacro_Pipe_DoDTranslate(
                            state,
                            tokScoreboard,
                            tokMemAddr,
                            tokPhysicalMemAddrs,
                            link_dtlb);

    let doLoads <- mkFUNCP_RegMgrMacro_Pipe_DoLoads(
                            state,
                            tokScoreboard,
                            prfValids,
                            prf,
                            tokPhysicalMemAddrs,
                            tokDsts,
                            linkToMem,
                            memPathQ);

    let doStores <- mkFUNCP_RegMgrMacro_Pipe_DoStores(
                            state,
                            tokScoreboard,
                            tokPhysicalMemAddrs,
                            linkToMem,
                            memPathQ,
                            tokStoreValue);

    let commitResults <- mkFUNCP_RegMgrMacro_Pipe_CommitResults(
                            state,
                            tokScoreboard,
                            freelist,
                            tokRegsToFree);

    let commitStores <- mkFUNCP_RegMgrMacro_Pipe_CommitStores(
                            state,
                            tokScoreboard,
                            linkToMem);

    let exception <- mkFUNCP_RegMgrMacro_Pipe_Exception(
                            state,
                            tokScoreboard,
                            tokAddr,
                            tokInst,
                            snapshots,
                            freelist,
                            tokFreeListPos,
                            tokRegsToFree,
                            maptable,
                            tokDsts,
                            tokMemAddr,
                            link_itlb,
                            link_dtlb,
                            linkToMem,
                            branchEpoch,
                            faultEpoch);


    // ====================================================================
    //
    //   Rules for initialization of global state
    //
    // ====================================================================

    // When:    Only at the beginning of time (after a reset).
    // Effects: Makes sure all RAMS are in the right state before we begin computing.
    //

    Reg#(Bit#(1)) initPhase <- mkReg(0);

    // This register stores the current Phys Reg we are initializing.
    Reg#(FUNCP_PHYSICAL_REG_INDEX) initPrfIdx <- mkReg(0);


    rule initialize_prf (state.getState() == RSM_Initializing && initPhase == 0);

        // For safety we start all physical registers at zero.
        // In the future this might change.
        prf.write(initPrfIdx, 0);
        
        // We're done if we've initialized the last register.
        if (initPrfIdx >= maxInit)
        begin
            initPhase <= 1;
        end

        initPrfIdx <= initPrfIdx + 1;

    endrule
  
    //
    // Initialize all token indexed objects.  Doing initialization here instead
    // of using the initialized constructors for RAM saves testing the init
    // predicate on each access to the storage.
    //

    Reg#(TOKEN_INDEX) initTokIdx <- mkReg(0);

    //
    // Urgency
    //
    // A total ordering of all non-trivial rules in the system specifying which
    // should get to proceed in the case of a conflict. The logic here is
    // straightforward. In terms of macro-operations, the "later" operations are
    // favored:
    //
    // newInFlight < doITrans < getInst < getDeps < getResult < doDTrans < doLoads < doStores < commitResults < commitStores
    //
    // Thus getResults() should be favored over getDeps().
    //
    // Within a single macro-operation a similar philosophy holds: favor the later
    // stages of the pipeline. Thus:
    //
    // doLoads1 < doLoads2 < doLoads3
    //
    // This is _particularly_ important for the getDeps stages, which modify
    // the maptable.  We specify all of this as a TOTAL ORDER, which is tedious,
    // but guaranteed to be complete.
    //
    // Do not change the following lines unless you understand all this and have
    // a good reason.
    //
    (* descending_urgency=
        "exception.rewindToToken4, exception.rewindToToken3Slow, exception.rewindToToken3Fast, exception.rewindToToken2, exception.rewindToToken1, exception.rewindToTokenS, exception.handleFault3, exception.handleFault2, exception.handleFault1, exception.handleFaultS, commitStores.commitStores, commitResults.commitResults2, commitResults.commitResults1, doStores.doStores2SpanEnd, doStores.doStores2SpanRsp2, doStores.doStores2SpanRsp1, doStores.doStores2SpanReq, doStores.doStores2RMW, doStores.doStores2, doStores.doStores1, doLoads.doLoads3Span, doLoads.doLoads3, doLoads.doLoads2Span, doLoads.doLoads2, doLoads.doLoads1, doDTranslate.doDTranslate3Span, doDTranslate.doDTranslate3, doDTranslate.doDTranslate2Span, doDTranslate.doDTranslate2, doDTranslate.doDTranslate1, getResults.emulateInstruction4, getResults.emulateInstruction3_UpdateReg, getResults.emulateInstruction3, getResults.emulateInstruction2_Rsp, getResults.emulateInstruction2_Req, getResults.emulateInstruction1, getResults.getResults4AdditionalWriteback, getResults.getResults4, getResults.getResults3, getResults.getResults2StallEnd, getResults.getResults2, getResults.getResults1, getDependencies.getDependencies2AdditionalMappings, getDependencies.getDependencies2, getDependencies.getDependencies1, getInstruction.getInstruction3Span, getInstruction.getInstruction3, getInstruction.getInstruction2Span, getInstruction.getInstruction2, getInstruction.getInstruction1, doITranslate.doITranslate2Span, doITranslate.doITranslate2, doITranslate.doITranslate1Span, doITranslate.doITranslate1, newInFlight.newInFlight, initialize_prf, initialize_tok_idx" *)

    //
    // The execution_order pragma doesn't affect the schedule but does get rid of
    // compiler warnings caused by the appearance of multiple writers to the
    // prfvalids vector.  According to Bluespec the order here affects the
    // priority encoder within a cycle but not the scheduling rules.
    //
    (* execution_order =
        "getResults.emulateInstruction3_UpdateReg, getResults.getResults4AdditionalWriteback, getResults.getResults4, getDependencies.getDependencies2AdditionalMappings, getDependencies.getDependencies2" *)

    rule initialize_tok_idx (state.getState() == RSM_Initializing && initPhase == 1);

        tokAddr.write(initTokIdx, 0);
        tokPhysicalAddrs.write(initTokIdx, tagged ONE 0);
        tokInst.write(initTokIdx, 0);
        tokDsts.write(initTokIdx, Vector::replicate(tagged Invalid));
        tokWriters.write(initTokIdx, Vector::replicate(tagged Invalid));
        tokMemAddr.write(initTokIdx, 0);
        tokStoreValue.write(initTokIdx, 0);
        tokPhysicalMemAddrs.write(initTokIdx, tagged ONE 0);
        tokFreeListPos.write(initTokIdx, tagged Invalid);
        tokRegsToFree.write(initTokIdx, Vector::replicate(tagged Invalid));

        // Done?
        if (initTokIdx == maxBound)
        begin
            state.setState(RSM_Running);
        end

        initTokIdx <= initTokIdx + 1;

    endrule

endmodule
