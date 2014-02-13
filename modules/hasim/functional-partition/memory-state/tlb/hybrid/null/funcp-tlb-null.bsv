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
// Null TLB.  PA == VA.
//


// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"


module [HASIM_MODULE] mkFUNCP_CPU_TLBS
    // interface:
        ();

    // Connections to functional register state manager
    Connection_Server#(Maybe#(FUNCP_TLB_QUERY), FUNCP_TLB_RESP) link_funcp_itlb <- mkConnection_Server("funcp_itlb_translate");
    Connection_Server#(Maybe#(FUNCP_TLB_QUERY), FUNCP_TLB_RESP) link_funcp_dtlb <- mkConnection_Server("funcp_dtlb_translate");

    Connection_Receive#(FUNCP_TLB_FAULT) link_funcp_itlb_fault <- mkConnection_Receive("funcp_itlb_pagefault");
    Connection_Receive#(FUNCP_TLB_FAULT) link_funcp_dtlb_fault <- mkConnection_Receive("funcp_dtlb_pagefault");

    // ***** Rules for communcation with functional register state manager *****
    
    rule itlb_req (True);
        let m_req = link_funcp_itlb.getReq();
        link_funcp_itlb.deq();
        if (m_req matches tagged Valid .req)
        begin
            link_funcp_itlb.makeResp(FUNCP_TLB_RESP { pa : truncate(req.va), pageFault: False, ioSpace: False });
        end
    endrule

    rule dtlb_req (True);
        let m_req = link_funcp_dtlb.getReq();
        link_funcp_dtlb.deq();
        if (m_req matches tagged Valid .req)
        begin
            link_funcp_dtlb.makeResp(FUNCP_TLB_RESP { pa : truncate(req.va), pageFault: False, ioSpace: False });
        end
    endrule

    // ***** No page faults needed
    rule itlb_fault (True);
        link_funcp_itlb_fault.deq();
    endrule
    
    rule dtlb_fault (True);
        link_funcp_dtlb_fault.deq();
    endrule

endmodule
