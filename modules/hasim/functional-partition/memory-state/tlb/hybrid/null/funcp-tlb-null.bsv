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
// TLB type (instruction or data)
//
typedef enum
{
  FUNCP_ITLB,
  FUNCP_DTLB
}
  FUNCP_TLB_TYPE
      deriving (Eq, Bits);


//
// Query passed to TLB server
//
typedef Tuple2#(TOKEN, ISA_ADDRESS) FUNCP_TLB_QUERY;


interface FUNCP_TLB;
    
    //
    // quickTranslateVA may return a translation within the same cycle if it
    // finds a hit in a small set of LUTs.  If the method returns false then
    // the caller should resort to making a request to the "funcp_tlb" server.
    //
    method Maybe#(MEM_ADDRESS) quickTranslateVA(FUNCP_TLB_QUERY query);
    
endinterface


module [HASIM_MODULE] mkFUNCP_TLB#(FUNCP_TLB_TYPE tlbType, String serverConn)
    // Interface:
        (FUNCP_TLB);
   
    // ***** Local State *****
    
    Connection_Server#(FUNCP_TLB_QUERY, Maybe#(MEM_ADDRESS)) link_regstate <- mkConnection_Server(serverConn);

    // Connection to memory translation service
    let memory_link_name = (tlbType == FUNCP_ITLB) ? "funcp_memory_VtoP_I" : "funcp_memory_VtoP_D";
    Connection_Client#(ISA_ADDRESS, MEM_ADDRESS) link_memory <- mkConnection_Client(memory_link_name);


    // ***** Internal functions *****
    
    function Maybe#(MEM_ADDRESS) doQuickTranslateVA(ISA_ADDRESS va);
    
        return tagged Valid truncate(va);

    endfunction


    // ***** Rules *****

    rule translate_VtoP_request (True);

        // pop a request from the link
        match { .tok, .va } = link_regstate.getReq();
        link_regstate.deq();

        link_regstate.makeResp(doQuickTranslateVA(va));

    endrule


    // ***** Methods *****

    method Maybe#(MEM_ADDRESS) quickTranslateVA(FUNCP_TLB_QUERY query);
    
        match { .tok, .va } = query;
        return doQuickTranslateVA(va);

    endmethod

endmodule
