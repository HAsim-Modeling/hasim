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
import FShow::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
 
// Functional Partition includes.

`include "asim/provides/funcp_interface.bsh"
`include "asim/provides/funcp_regstate_base_types.bsh"
`include "asim/provides/funcp_regstate_connections.bsh"
`include "asim/provides/funcp_regstate_data.bsh"
`include "asim/provides/funcp_memstate_manager.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"

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
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_ADDRESS) tokAddr <- mkLiveTokenBRAMMultiRead(False);

    // The memaddress is used by Loads/Stores so we don't have to repeat the calculation.
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_ADDRESS) tokMemAddr <- mkLiveTokenBRAMMultiRead(False);

    // The instruction that was at that address (from mem_state).
    BRAM_MULTI_READ#(2, TOKEN_INDEX, ISA_INSTRUCTION) tokInst <- mkLiveTokenBRAMMultiRead(True);

    // The destinations of the instruction (a convenience which saves us from reading the instruction/maptable).
    BRAM_MULTI_READ#(4, TOKEN_INDEX, REGMGR_DST_REGS) tokDsts <- mkLiveTokenBRAMMultiRead(True);

    // If an instruction has sources in other inflight instructions it will be noted here.
    BRAM#(TOKEN_INDEX, ISA_INST_SRCS) tokWriters <- mkLiveTokenBRAM();

    // The value a store will write to memory
    BRAM#(TOKEN_INDEX, ISA_VALUE) tokStoreValue <- mkLiveTokenBRAM();

    // The physical memaddress(es) for the instruction.
    BRAM_MULTI_READ#(2, TOKEN_INDEX, UP_TO_TWO#(MEM_ADDRESS)) tokPhysicalMemAddrs <- mkLiveTokenBRAMMultiRead(True);


    // ******* High-Level FSM State *******

    // ====================================================================
    //
    //   Submodules holding global state or managing external connections
    //
    // ====================================================================

    // ITLB and DTLB connections
    REGSTATE_TLB_CONNECTION linkITLB <- mkFUNCP_Regstate_Connect_TLB(FUNCP_ITLB);
    REGSTATE_TLB_CONNECTION linkDTLB <- mkFUNCP_Regstate_Connect_TLB(FUNCP_DTLB);

    // Memory connection
    REGSTATE_MEMORY_CONNECTION linkToMem <- mkFUNCP_Regstate_Connect_Memory();

    // Physical register file
    REGSTATE_PHYSICAL_REGS#(2) prf <- mkFUNCP_Regstate_Physical_Regs();

    // The Freelist tracks which physical registers are available.
    FUNCP_FREELIST freelist <- mkFUNCP_Freelist(`REGSTATE_LOGFILE_PREFIX);

    // Global data.  A catch-all class for state used everywhere.
    REGMGR_GLOBAL_DATA globData <- mkFUNCP_RegStateManager_GlobalData();

    // The Map Table
    REGSTATE_REG_MAPPING regMapping <- mkFUNCP_Regstate_RegMapping();

    
    // ====================================================================
    //
    //   Pipeline submodules (connections to timing module)
    //
    // ====================================================================

    let doITranslate <- mkFUNCP_RegMgrMacro_Pipe_DoITranslate(
                            globData,
                            linkITLB.translate);

    let getInstruction <- mkFUNCP_RegMgrMacro_Pipe_GetInstruction(
                            globData,
                            linkToMem.getInstructionQueue);
    
    let getDependencies <- mkFUNCP_RegMgrMacro_Pipe_GetDependencies(
                            globData,
                            regMapping.getDependencies,
                            prf.getDependencies,
                            freelist,
                            tokAddr,
                            tokInst,
                            tokWriters,
                            tokDsts);

    let getResults <- mkFUNCP_RegMgrMacro_Pipe_GetResults(
                            globData,
                            regMapping.getResults,
                            prf.getResults,
                            tokAddr.readPorts[0],
                            tokWriters,
                            tokDsts.readPorts[0],
                            tokInst.readPorts[1],
                            tokMemAddr,
                            tokStoreValue);

    let doDTranslate <- mkFUNCP_RegMgrMacro_Pipe_DoDTranslate(
                            globData,
                            linkDTLB.translate,
                            tokMemAddr.readPorts[0],
                            tokPhysicalMemAddrs);

    let doLoads <- mkFUNCP_RegMgrMacro_Pipe_DoLoads(
                            globData,
                            linkToMem.doLoadsQueue,
                            prf.doLoads,
                            tokPhysicalMemAddrs.readPorts[0],
                            tokDsts.readPorts[1]);

    let doStores <- mkFUNCP_RegMgrMacro_Pipe_DoStores(
                            globData,
                            linkToMem.doStoresQueue,
                            tokPhysicalMemAddrs.readPorts[1],
                            tokStoreValue);

    let commitResults <- mkFUNCP_RegMgrMacro_Pipe_CommitResults(
                            globData,
                            regMapping.commitResults,
                            freelist,
                            linkToMem.commitResultsQueue,
                            tokDsts.readPorts[2]);

    let commitStores <- mkFUNCP_RegMgrMacro_Pipe_CommitStores(
                            globData,
                            linkToMem.commitStoresQueue);

    let exception <- mkFUNCP_RegMgrMacro_Pipe_Exception(
                            globData,
                            linkITLB.fault,
                            linkDTLB.fault,
                            linkToMem.exceptionQueue,
                            regMapping.exceptionQueue,
                            tokDsts.readPorts[3],
                            freelist,
                            tokAddr.readPorts[1],
                            tokMemAddr.readPorts[1]);


    // ====================================================================
    //
    //   Rules for initialization of global state
    //
    // ====================================================================

    // When:    Only at the beginning of time (after a reset).
    // Effects: Makes sure all RAMS are in the right state before we begin computing.
    //

    //
    // Initialize all token indexed objects.  Doing initialization here instead
    // of using the initialized constructors for RAM saves testing the init
    // predicate on each access to the storage.
    //

    Reg#(CONTEXT_ID) initCtxIdx <- mkReg(0);
    Reg#(TOKEN_INDEX) initTokIdx <- mkReg(0);
    Reg#(Bit#(1)) initState <- mkReg(0);

    //
    // Urgency
    //
    // A total ordering of all non-trivial rules in the system specifying which
    // should get to proceed in the case of a conflict. The logic here is
    // straightforward. In terms of macro-operations, the "later" operations are
    // favored:
    //
    // doITrans < getInst < getDeps < getResult < doDTrans < doLoads < doStores < commitResults < commitStores
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
        "exception.rewindToToken4, exception.rewindToToken3, exception.rewindToToken2, exception.rewindToToken1, exception.rewindToTokenS, exception.handleFault2, exception.handleFault1, exception.handleFaultS, commitStores.commitStores1, commitResults.commitResults2, commitResults.commitResults1, doStores.doStores3, doStores.doStores2SpanEnd, doStores.doStores2SpanRsp2, doStores.doStores2SpanRsp1, doStores.doStores2SpanReq, doStores.doStores2RMW, doStores.doStores2, doStores.doStores1, doStores.doStoresS, doLoads.doLoads3Span, doLoads.doLoads3, doLoads.doLoads2Span, doLoads.doLoads2, doLoads.doLoads1, doDTranslate.doDTranslate3Span, doDTranslate.doDTranslate3, doDTranslate.doDTranslate2Span, doDTranslate.doDTranslate2, doDTranslate.doDTranslate1, getResults.emulateInstruction4, getResults.emulateInstruction3_UpdateRegWrite, getResults.emulateInstruction3_UpdateReg, getResults.emulateInstruction3, getResults.emulateInstruction2_Rsp, getResults.emulateInstruction2_Req, getResults.emulateInstruction2_PRFReq, getResults.emulateInstruction1_GenRegMapResp, getResults.emulateInstruction1_GenRegMapReq, getResults.emulateInstruction1, getResults.getResults4AdditionalWriteback, getResults.getResults4, getResults.getResults3, getResults.getResults2, getResults.getResults1, getDependencies.getDependencies4, getDependencies.getDependencies3, getDependencies.getDependencies2, getDependencies.getDependencies1, getInstruction.getInstruction2Span, getInstruction.getInstruction2, getInstruction.getInstruction1Span, getInstruction.getInstruction1, doITranslate.doITranslate2Span, doITranslate.doITranslate2, doITranslate.doITranslate1Span, doITranslate.doITranslate1, initializeRegmgrTok" *)

    rule initializeRegmgrTok ((initState == 0) &&
                              (globData.state.getState() == RSM_Initializing));

        tokAddr.write(initTokIdx, 0);
        tokInst.write(initTokIdx, 0);
        tokDsts.write(initTokIdx, REGMGR_DST_REGS { ar: replicate(tagged Invalid), pr: replicate(tagged Invalid) });
        tokWriters.write(initTokIdx, replicate(tagged Invalid));
        tokMemAddr.write(initTokIdx, 0);
        tokStoreValue.write(initTokIdx, 0);
        tokPhysicalMemAddrs.write(initTokIdx, tagged ONE 0);

        // Done?
        Bit#(TOKEN_INDEX_SIZE) idx_as_bit = pack(initTokIdx);
        if (idx_as_bit == maxBound)
        begin
            globData.state.setState(RSM_Running);
        end

        initTokIdx <= unpack(idx_as_bit + 1);

    endrule


endmodule
