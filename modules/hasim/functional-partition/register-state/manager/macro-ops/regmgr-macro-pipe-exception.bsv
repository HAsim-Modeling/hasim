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
  

module [HASIM_MODULE] mkFUNCP_RegMgrMacro_Pipe_Exception#(
    REGMGR_GLOBAL_DATA glob,
    REGSTATE_TLB_FAULT link_itlb_fault,
    REGSTATE_TLB_FAULT link_dtlb_fault,
    BROM#(TOKEN_INDEX, ISA_ADDRESS) tokAddr,
    BROM#(TOKEN_INDEX, ISA_ADDRESS) tokMemAddr)
    //interface:
                ();

    // ====================================================================
    //
    //   Debugging state
    //
    // ====================================================================

    DEBUG_FILE debugLog <- mkDebugFile(`REGSTATE_LOGFILE_PREFIX + "_pipe_exception.out");


    // ====================================================================
    //
    //   Soft connections
    //
    // ====================================================================

    Connection_Server#(FUNCP_REQ_HANDLE_FAULT,
                       FUNCP_RSP_HANDLE_FAULT)    linkHandleFault   <- mkConnection_Server("funcp_handleFault");

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

    FIFO#(TOKEN) faultQ <- mkFIFO();


    // ====================================================================
    //
    //   Rules
    //
    // ====================================================================


    // ******* handleFault ******* //

    // Handle a fault raised in an earlier stage.  Returns the address from
    // which fetch should resume. We assume that this will result in a later
    // call to rewindToToken, as with any redirection.

    // Wait for all activity to stop and start the fault handler

    rule handleFault1 (state.readyToBegin(tokContextId(linkHandleFault.getReq().token)));

        // Get the input from the timing model.
        let req = linkHandleFault.getReq();
        linkHandleFault.deq();
        let tok = req.token;
        
        debugLog.record(fshow(req.token.index) + $format(": handleFault1")); 

        // Read all possibly interesting addresses
        tokAddr.readReq(tok.index);           // PC
        tokMemAddr.readReq(tok.index);
        
        faultQ.enq(tok);

    endrule


    (* conservative_implicit_conditions *)
    rule handleFault2 (state.readyToContinue());

        // Instruction & data addresses
        let iAddr <- tokAddr.readRsp();
        let dAddr <- tokMemAddr.readRsp();

        match { .iAddr_aligned, .iAddr_offset } = isaAlignAddress(iAddr);
        match { .dAddr_aligned, .dAddr_offset } = isaAlignAddress(dAddr);

        let tok = faultQ.first();
        faultQ.deq();

        if (tokScoreboard.getFault(tok.index) matches tagged Valid .fault)
        begin
            let mem_ref_bytes = fromInteger(valueOf(SizeOf#(MEM_VALUE)) / 8);

            //
            // handleTLBPageFault mode for TLBs returns no response.  Just triggering
            // the request is enough to allocate the page.
            //

            case (fault)
            FAULT_ITRANS:
            begin
                let addr = iAddr_aligned;
                debugLog.record(fshow(tok.index) + $format(": handleFault2: ITRANS (VA: 0x%h)", addr)); 
                link_itlb_fault.pageFault(handleTLBPageFault(tokContextId(tok), addr));
            end

            FAULT_ITRANS2:
            begin
                let addr = iAddr_aligned + mem_ref_bytes;
                debugLog.record(fshow(tok.index) + $format(": handleFault2: ITRANS2 (VA: 0x%h)", addr)); 
                link_itlb_fault.pageFault(handleTLBPageFault(tokContextId(tok), addr));
            end

            FAULT_DTRANS:
            begin
                let addr = dAddr_aligned;
                debugLog.record(fshow(tok.index) + $format(": handleFault2: DTRANS (VA: 0x%h)", addr)); 
                link_dtlb_fault.pageFault(handleTLBPageFault(tokContextId(tok), addr));
            end

            FAULT_DTRANS2:
            begin
                let addr = dAddr_aligned + mem_ref_bytes;
                debugLog.record(fshow(tok.index) + $format(": handleFault2: DTRANS2 (VA: 0x%h)", addr)); 
                link_dtlb_fault.pageFault(handleTLBPageFault(tokContextId(tok), addr));
            end

            default:
            begin
                assertion.illegalInstruction(False);
                debugLog.record(fshow(tok.index) + $format(": handleFault2: No handler for fault %d", fault)); 
            end
            endcase
        end
        else
        begin
            assertion.illegalInstruction(False);
            debugLog.record(fshow(tok.index) + $format(": handleFault2: Instruction did not fault"));
        end

        // Since we don't have a software handler, fetch should resume at the token's address.
        let resume_instr_addr = iAddr;

        // Log it.
        debugLog.record(fshow(tok.index) + $format(": handleFault2: Restart at 0x%h", resume_instr_addr)); 

        // Send response to timing model
        linkHandleFault.makeResp(initFuncpRspHandleFault(tok, resume_instr_addr));

    endrule

endmodule
