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


module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_CommitStores#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_MEMORY_QUEUE linkToMem)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_commitStores.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_COMMIT_STORES,
                       FUNCP_RSP_COMMIT_STORES) linkCommitStores <- mkConnection_Server("funcp_commitStores");  


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

    FIFO#(TOKEN) commitQ <- mkFIFO();


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* commitStores ******* //

    // 2-stage macro operation which commits global stores.

    // When:   When the timing model requests it.
    // Effect: Tell the memory state to make a store globally visible.
    // Soft Inputs:  Token
    // Soft Returns: Token
    
    rule commitStores1 (state.readyToBegin(tokContextId(linkCommitStores.getReq().token)));

        // Get the input from the timing model. Begin macro-operation.
        let req = linkCommitStores.getReq();
        linkCommitStores.deq();
        let tok = req.token;

        // If the token was not actually a store, it's an exception.
        let isStore = tokScoreboard.isStore(tok.index);
        let fault = isValid(tokScoreboard.getFault(tok.index));
        assertion.commitedStoreIsActuallyAStore(isStore && !fault);

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": CommitStores: Committing.")); 

        linkToMem.makeReq(tagged REQ_COMMIT MEMSTATE_REQ_COMMIT {tok: tok});
        commitQ.enq(tok);

    endrule


    rule commitStores2 (state.readyToContinue());

        let tok = commitQ.first();
        commitQ.deq();

        // Get the confirmation that the memory request was forwarded to memory
        linkToMem.deq();

        // Respond to timing model. End of macro-operation.
        linkCommitStores.makeResp(initFuncpRspCommitStores(tok));

    endrule
    
endmodule
