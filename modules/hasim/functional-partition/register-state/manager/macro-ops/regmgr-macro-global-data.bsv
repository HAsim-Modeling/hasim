//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

//
// Global data is a set of interfaces passed as a single wrapper interface
// to all functional pipelines that are part of the timing model / functional
// model interface.
//
// Global data elements are supposed to be univerally visible objects that
// don't have interfaces specific to individual pipeline stages.
//

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"
`include "asim/provides/fpga_components.bsh"
 
// Dictionary includes
`include "asim/dict/ASSERTIONS_REGMGR.bsh"


//
// Assertions.  These are in global storage instead of private to each pipeline
// so a single assertions ring stop can be allocated for the register state
// manager.
//
interface REGMGR_ASSERTIONS;

    interface ASSERTION expectedOldestTok;
    interface ASSERTION regUpdateAtExpectedTime;
    interface ASSERTION emulationFinishedAtExpectedTime;
    interface ASSERTION invalidNumDsts;
    interface ASSERTION emulatedInstrNoDsts;
    interface ASSERTION illegalInstruction;
    interface ASSERTION instructionIsActuallyAStore;
    interface ASSERTION poisonBit;
    interface ASSERTION_WITH_MSG instructionIsActuallyALoad;
    interface ASSERTION dTranslateOnMemOp;

endinterface: REGMGR_ASSERTIONS


//
// Global data wrapper.
//
interface REGMGR_GLOBAL_DATA;

    // State -- controls when rules may fire
    interface REGMGR_STATE state;

    // The Token State is a big scoreboard which tracks the status of inflight tokens.
    interface FUNCP_SCOREBOARD tokScoreboard;

    // Assertions
    interface REGMGR_ASSERTIONS assertion;

endinterface: REGMGR_GLOBAL_DATA


module [HASIM_MODULE] mkFUNCP_RegStateManager_GlobalData
    // interface:
    (REGMGR_GLOBAL_DATA);
    
    // ====================================================================
    //
    // Internal objects
    //
    // ====================================================================

    REGMGR_STATE regmgrState <- mkRegmanagerState(RSM_Initializing);

    FUNCP_SCOREBOARD scoreboard <- mkFUNCP_Scoreboard();

    // Testable assertions
    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_REGMGR__BASE);
    ASSERTION assertExpectedOldestTok                 <- mkAssertionChecker(`ASSERTIONS_REGMGR_EXPECTED_OLDEST_TOK, ASSERT_ERROR, assertNode);
    ASSERTION assertRegUpdateAtExpectedTime           <- mkAssertionChecker(`ASSERTIONS_REGMGR_UNEXPECTED_REG_UPDATE, ASSERT_WARNING, assertNode);
    ASSERTION assertEmulationFinishedAtExpectedTime   <- mkAssertionChecker(`ASSERTIONS_REGMGR_UNEXPECTED_EMULATION_FINISHED, ASSERT_WARNING, assertNode);
    ASSERTION assertInvalidNumDsts                    <- mkAssertionChecker(`ASSERTIONS_REGMGR_INVALID_NUM_DSTS, ASSERT_ERROR, assertNode);
    ASSERTION assertEmulatedInstrNoDsts               <- mkAssertionChecker(`ASSERTIONS_REGMGR_EMULATED_INSTR_HAS_DST, ASSERT_ERROR, assertNode);
    ASSERTION assertIllegalInstruction                <- mkAssertionChecker(`ASSERTIONS_REGMGR_ILLEGAL_INSTRUCTION, ASSERT_ERROR, assertNode);
    ASSERTION assertInstructionIsActuallyAStore       <- mkAssertionChecker(`ASSERTIONS_REGMGR_STORE_ON_NONSTORE, ASSERT_WARNING, assertNode);
    ASSERTION assertPoisonBit                         <- mkAssertionChecker(`ASSERTIONS_REGMGR_BAD_POISON_BIT, ASSERT_ERROR, assertNode);
    ASSERTION_WITH_MSG assertInstructionIsActuallyALoad <- mkAssertionCheckerWithMsg(`ASSERTIONS_REGMGR_LOAD_ON_NONLOAD, ASSERT_ERROR, assertNode);
    ASSERTION assertDTranslateOnMemOp                 <- mkAssertionChecker(`ASSERTIONS_REGMGR_DTRANSLATE_ON_MEMOP, ASSERT_ERROR, assertNode);


    // ====================================================================
    //
    // Visible interfaces
    //
    // ====================================================================

    interface REGMGR_STATE state = regmgrState;

    interface FUNCP_SCOREBOARD tokScoreboard = scoreboard;

    interface REGMGR_ASSERTIONS assertion;
        interface ASSERTION expectedOldestTok = assertExpectedOldestTok;
        interface ASSERTION regUpdateAtExpectedTime = assertRegUpdateAtExpectedTime;
        interface ASSERTION emulationFinishedAtExpectedTime = assertEmulationFinishedAtExpectedTime;
        interface ASSERTION invalidNumDsts = assertInvalidNumDsts;
        interface ASSERTION emulatedInstrNoDsts = assertEmulatedInstrNoDsts;
        interface ASSERTION illegalInstruction = assertIllegalInstruction;
        interface ASSERTION instructionIsActuallyAStore = assertInstructionIsActuallyAStore;
        interface ASSERTION poisonBit = assertPoisonBit;
        interface ASSERTION_WITH_MSG instructionIsActuallyALoad = assertInstructionIsActuallyALoad;
        interface ASSERTION dTranslateOnMemOp = assertDTranslateOnMemOp;
    endinterface: assertion

endmodule
