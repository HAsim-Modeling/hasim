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
// Connection from the functional register state manager to the functional TLBs.
//

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
 
// Functional Partition includes.

`include "asim/provides/funcp_memstate_base_types.bsh"


//
// Separate sub-interfaces are provided for the functional translation pipeline
// and the fault pipeline.  This pushes the scheduling decision of which pipeline
// to process to the memory subsystem. (A pending fault is always processed before
// a translation request.)
//

//
// Translation interface
//
interface REGSTATE_TLB_TRANSLATE;
    method Action noReq();
    method Action makeReq(FUNCP_TLB_QUERY query);

    method FUNCP_TLB_RESP getResp();
    method Action deq();
endinterface


//
// Page fault interface
//
interface REGSTATE_TLB_FAULT;
    method Action pageFault(FUNCP_TLB_FAULT fault);
endinterface


interface REGSTATE_TLB_CONNECTION;
    interface REGSTATE_TLB_TRANSLATE translate;
    interface REGSTATE_TLB_FAULT fault;
endinterface


//
// Build the interface for either an ITLB or a DTLB, depending on the argument.
// The interfaces for ITLB and DTLB are identical.  Only the connection names
// change.
//
module [HASIM_MODULE] mkFUNCP_Regstate_Connect_TLB#(FUNCP_TLB_TYPE tlbType)
    // interface:
    (REGSTATE_TLB_CONNECTION);
    
    String connect_translate_name = (tlbType == FUNCP_ITLB) ? "funcp_itlb_translate" : "funcp_dtlb_translate";
    Connection_Client#(Maybe#(FUNCP_TLB_QUERY), FUNCP_TLB_RESP) link_funcp_tlb_trans <- mkConnection_Client(connect_translate_name);

    String connect_pagefault_name = (tlbType == FUNCP_ITLB) ? "funcp_itlb_pagefault" : "funcp_dtlb_pagefault";
    Connection_Send#(FUNCP_TLB_FAULT) link_funcp_tlb_fault <- mkConnection_Send(connect_pagefault_name);


    interface REGSTATE_TLB_TRANSLATE translate;
        method Action noReq();
            link_funcp_tlb_trans.makeReq(tagged Invalid);
        endmethod

        method Action makeReq(FUNCP_TLB_QUERY query);
            link_funcp_tlb_trans.makeReq(tagged Valid query);
        endmethod

        method FUNCP_TLB_RESP getResp();
            return link_funcp_tlb_trans.getResp();
        endmethod

        method Action deq();
            link_funcp_tlb_trans.deq();
        endmethod
    endinterface


    interface REGSTATE_TLB_FAULT fault;
        method Action pageFault(FUNCP_TLB_FAULT fault);
            link_funcp_tlb_fault.send(fault);
        endmethod
    endinterface

endmodule
