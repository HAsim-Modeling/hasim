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
    Connection_Server#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP) link_funcp_itlb <- mkConnection_Server("funcp_itlb_translate");
    Connection_Server#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP) link_funcp_dtlb <- mkConnection_Server("funcp_dtlb_translate");

    Connection_Receive#(FUNCP_TLB_FAULT) link_funcp_itlb_fault <- mkConnection_Receive("funcp_itlb_pagefault");
    Connection_Receive#(FUNCP_TLB_FAULT) link_funcp_dtlb_fault <- mkConnection_Receive("funcp_dtlb_pagefault");

    // Connection to memory translation service.  This won't be called since
    // VA == PA.
    Connection_Client#(MEM_VTOP_REQUEST, MEM_VTOP_REPLY) link_memory <- mkConnection_Client("funcp_memory_VtoP");

    // ***** Rules for communcation with functional register state manager *****
    
    rule itlb_req (True);
        let req = link_funcp_itlb.getReq();
        link_funcp_itlb.deq();
        link_funcp_itlb.makeResp(FUNCP_TLB_RESP { pa : truncate(req.va), pageFault: False, ioSpace: False });
    endrule

    rule dtlb_req (True);
        let req = link_funcp_dtlb.getReq();
        link_funcp_dtlb.deq();
        link_funcp_dtlb.makeResp(FUNCP_TLB_RESP { pa : truncate(req.va), pageFault: False, ioSpace: False });
    endrule

    // ***** No page faults needed
    rule itlb_fault (True);
        link_funcp_itlb_fault.deq();
    endrule
    
    rule dtlb_fault (True);
        link_funcp_dtlb_fault.deq();
    endrule

endmodule
