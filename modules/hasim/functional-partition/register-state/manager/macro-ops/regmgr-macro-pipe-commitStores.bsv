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
                       FUNCP_RSP_COMMIT_STORES) linkCommitStores <-
        mkFUNCPInterfaceServer("funcp_commitStores");


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

    FIFO#(STORE_TOKEN) commitQ <- mkSizedFIFO(16);


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
    
    rule commitStores1 (state.readyToBegin(storeTokContextId(linkCommitStores.getReq().storeToken)));

        // Get the input from the timing model. Begin macro-operation.
        let req = linkCommitStores.getReq();
        linkCommitStores.deq();
        let store_tok = req.storeToken;

        // Log it.
        debugLog.record(fshow(store_tok) + $format(": CommitStores: Committing.")); 

        linkToMem.makeReq(tagged REQ_WRITE_BACK MEMSTATE_REQ_WRITE_BACK {storeTok: store_tok});
        commitQ.enq(store_tok);

    endrule


    rule commitStores2 (state.readyToContinue());

        let store_tok = commitQ.first();
        commitQ.deq();

        // Get the confirmation that the memory request was forwarded to memory
        linkToMem.deq();

        // Respond to timing model. End of macro-operation.
        linkCommitStores.makeResp(initFuncpRspCommitStores(store_tok));

    endrule
    
endmodule
