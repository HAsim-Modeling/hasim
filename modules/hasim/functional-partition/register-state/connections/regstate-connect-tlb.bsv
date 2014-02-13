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
