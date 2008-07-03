//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
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


interface FUNCP_TLB;
    
    //
    // quickTranslateVA may return a translation within the same cycle if it
    // finds a hit in a small set of LUTs.  If the method returns false then
    // the caller should resort to making a request to the "funcp_tlb" server.
    //
    method Maybe#(MEM_ADDRESS) quickTranslateVA(ISA_ADDRESS va);
    
endinterface


module [HASIM_MODULE] mkFUNCP_TLB
    // Interface:
        (FUNCP_TLB)
    provisos
        (Bits#(ISA_ADDRESS, isa_address_SZ),
         Bits#(MEM_ADDRESS, mem_address_SZ));
   
    // ***** Local State *****
    
    Connection_Server#(ISA_ADDRESS, Maybe#(MEM_ADDRESS)) link_regstate <- mkConnection_Server("funcp_tlb");

    // Connection to memory translation service
    Connection_Client#(ISA_ADDRESS, MEM_ADDRESS) link_memory <- mkConnection_Client("funcp_memory_VtoP");


    // ***** Internal functions *****
    
    function Maybe#(MEM_ADDRESS) doQuickTranslateVA(ISA_ADDRESS va);
    
        return tagged Valid truncate(va);

    endfunction


    // ***** Rules *****

    rule translate_VtoP_request (True);

        // pop a request from the link
        ISA_ADDRESS va = link_regstate.getReq();
        link_regstate.deq();

        link_regstate.makeResp(doQuickTranslateVA(va));

    endrule


    // ***** Methods *****

    method Maybe#(MEM_ADDRESS) quickTranslateVA(ISA_ADDRESS va);
    
        return doQuickTranslateVA(va);

    endmethod

endmodule
