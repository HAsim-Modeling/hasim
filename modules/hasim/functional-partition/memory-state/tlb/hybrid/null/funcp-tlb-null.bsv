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
`include "asim/provides/funcp_memory.bsh"


// ===================================================================
//
// PUBLIC DATA STRUCTURES
//
// ===================================================================

//
// Query passed to TLB service.  When alloc_on_fault is set the host will
// attempt to allocate a new page when no page is currently mapped for the VA.
// Typically this bit will be set only by exception handlers in response to
// a normal failed translation.
//
typedef struct
{
    ISA_ADDRESS va;
    TOKEN tok;
}
FUNCP_TLB_QUERY
    deriving (Eq, Bits);

//
// Helper functions for constructing FUNCP_TLB_QUERY
//
function FUNCP_TLB_QUERY normalTLBQuery(TOKEN tok, ISA_ADDRESS va);
    return FUNCP_TLB_QUERY { va: va, tok: tok };
endfunction

function FUNCP_TLB_QUERY handleTLBPageFault(TOKEN tok, ISA_ADDRESS va);
    return FUNCP_TLB_QUERY { va: va, tok: tok };
endfunction


//
// Response from TLB service.  When page_fault is clear, pa holds the valid
// translation.  When page_fault is set the translation failed.  The functional
// model should service page faults on attempts to commit the token.
//
// Note:  even when page_fault is set, the pa may still be used for references.
//        In this case, pa is set to the guard page.  This allows simple timing
//        models to proceed with minimal knowledge of exception handling.
//
typedef struct
{
    Bool page_fault;
    MEM_ADDRESS pa;
}
FUNCP_TLB_RESP
    deriving (Eq, Bits);


module [HASIM_MODULE] mkFUNCP_CPU_TLBS
    // interface:
        ();

    // Connections to functional register state manager
    Connection_Server#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP) link_funcp_itlb <- mkConnection_Server("funcp_itlb");
    Connection_Server#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP) link_funcp_dtlb <- mkConnection_Server("funcp_dtlb");

    // Connection to memory translation service.  This won't be called since
    // VA == PA.
    Connection_Client#(ISA_ADDRESS, MEM_ADDRESS) link_memory <- mkConnection_Client("funcp_memory_VtoP");

    // ***** Rules for communcation with functional register state manager *****
    
    rule itlb_req (True);
        let req = link_funcp_itlb.getReq();
        link_funcp_itlb.deq();
        link_funcp_itlb.makeResp(FUNCP_TLB_RESP { pa : truncate(req.va), page_fault: False });
    endrule

    rule dtlb_req (True);
        let req = link_funcp_dtlb.getReq();
        link_funcp_dtlb.deq();
        link_funcp_dtlb.makeResp(FUNCP_TLB_RESP { pa : truncate(req.va), page_fault: False });
    endrule

endmodule
