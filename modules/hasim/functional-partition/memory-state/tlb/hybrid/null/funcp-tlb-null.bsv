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


//
// Query passed to TLB server
//
typedef Tuple2#(TOKEN, ISA_ADDRESS) FUNCP_TLB_QUERY;


module [HASIM_MODULE] mkFUNCP_CPU_TLBS
    // interface:
        ();

    // Connections to functional register state manager
    Connection_Server#(FUNCP_TLB_QUERY, Maybe#(MEM_ADDRESS)) link_funcp_itlb <- mkConnection_Server("funcp_itlb");
    Connection_Server#(FUNCP_TLB_QUERY, Maybe#(MEM_ADDRESS)) link_funcp_dtlb <- mkConnection_Server("funcp_dtlb");

    // Connection to memory translation service.  This won't be called since
    // VA == PA.
    Connection_Client#(ISA_ADDRESS, MEM_ADDRESS) link_memory <- mkConnection_Client("funcp_memory_VtoP");

    // ***** Rules for communcation with functional register state manager *****
    
    rule itlb_req (True);
        match { .tok, .va } = link_funcp_itlb.getReq();
        link_funcp_itlb.deq();
        link_funcp_itlb.makeResp(tagged Valid truncate(va));
    endrule

    rule dtlb_req (True);
        match { .tok, .va } = link_funcp_dtlb.getReq();
        link_funcp_dtlb.deq();
        link_funcp_dtlb.makeResp(tagged Valid truncate(va));
    endrule

endmodule
