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
  

module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_NewInFlight#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_REG_MAPPING_NEWINFLIGHT regMapping,
    LUTRAM#(CONTEXT_ID, TOKEN_BRANCH_EPOCH) branchEpoch,
    LUTRAM#(CONTEXT_ID, TOKEN_FAULT_EPOCH) faultEpoch)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_newInFlight.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_NEW_IN_FLIGHT, 
                       FUNCP_RSP_NEW_IN_FLIGHT) linkNewInFlight <- mkConnection_Server("funcp_newInFlight");


    // ====================================================================
    //
    //   Local names for global data 
    //
    // ====================================================================

    let state = glob.state;
    let tokScoreboard = glob.tokScoreboard;


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================

    // 1-stage macro-operation
    
    // When:         The timing model tells us to allocate a new in-flight instruction.
    // Effect:       Allocates a slot on the token state scoreboard.
    // Soft Inputs:  req from timing model
    // Soft Returns: a TOKEN which the timing model can use to refer to that slot.

    rule newInFlight (state.readyToBegin(linkNewInFlight.getReq().context_id));

        // Get the input from the timing model. Begin macro operation.
        let req = linkNewInFlight.getReq();
        linkNewInFlight.deq();
        
        let ctx_id = req.context_id;

        // Get the next token from the scoreboard.
        let idx <- tokScoreboard.allocate(ctx_id);
        
        // Log it.
        
        debugLog.record($format("NewInFlight: Allocating ") + fshow(idx));

        // The timing partition scratchpad must be filled in by up.
        let newtok = TOKEN { index: idx,
                             poison: False,
                             epoch: TOKEN_EPOCH { branch: branchEpoch.sub(ctx_id), fault: faultEpoch.sub(ctx_id) },
                             timep_info: TOKEN_TIMEP_INFO { scratchpad: 0 } };
        
        // Reset the free list pointer so rewind knows whether this token has
        // registers allocated.
        regMapping.initToken(newtok);

        // Respond to the timing partition. End of macro operation.
        linkNewInFlight.makeResp(initFuncpRspNewInFlight(newtok));

    endrule

endmodule
