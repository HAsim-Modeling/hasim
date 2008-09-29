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
// Standard hybrid TLB
//

// Library includes.

import FIFO::*;
import Vector::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_cache.bsh"

`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/dict/STATS_FUNCP_TLB.bsh"

// ===================================================================
//
// PUBLIC DATA STRUCTURES
//
// ===================================================================

//
// Query passed to TLB server
//
typedef Tuple2#(TOKEN, ISA_ADDRESS) FUNCP_TLB_QUERY;


// ===================================================================
//
// PRIVATE DATA STRUCTURES
//
// ===================================================================

//
// FUNCP_TLB --
//     Interface to a single TLB (instruction or data).
//
interface FUNCP_TLB#(numeric type n_CACHE_ENTRIES);
    method Action lookupReq(TOKEN tok, ISA_ADDRESS va);
    method ActionValue#(Maybe#(MEM_ADDRESS)) lookupResp();
endinterface: FUNCP_TLB


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

typedef enum
{
    FUNCP_TLB_IDLE,
    FUNCP_TLB_BUSY
}
FUNCP_TLB_STATE
    deriving (Eq, Bits);


//
// Request/response FIFOs from individual TLBs to the containing module.
// The containing module has a unified I/D TLB cache and manages misses
// to hybrid memory translations.
//
typedef FIFO#(FUNCP_V_PAGE) VTOP_REQ_FIFO;
typedef FIFO#(Maybe#(FUNCP_P_PAGE)) VTOP_RESP_FIFO;


//
// Conversion functions between addresses and page addresses.  Page
// addresses just drop the low bits to save space in caches.
//

function ISA_ADDRESS vaFromPage(FUNCP_V_PAGE page, FUNCP_PAGE_OFFSET offset);
    return { page, offset };
endfunction

function MEM_ADDRESS paFromPage(FUNCP_P_PAGE page, FUNCP_PAGE_OFFSET offset);
    return { page, offset };
endfunction

function FUNCP_V_PAGE pageFromVA(ISA_ADDRESS va);
    Tuple2#(FUNCP_V_PAGE, FUNCP_PAGE_OFFSET) tup = unpack(va);
    match { .page, .offset } = tup;
    return page;
endfunction

function FUNCP_P_PAGE pageFromPA(MEM_ADDRESS pa);
    Tuple2#(FUNCP_P_PAGE, FUNCP_PAGE_OFFSET) tup = unpack(pa);
    match { .page, .offset } = tup;
    return page;
endfunction

function FUNCP_PAGE_OFFSET pageOffsetFromVA(ISA_ADDRESS va);
    Tuple2#(FUNCP_V_PAGE, FUNCP_PAGE_OFFSET) tup = unpack(va);
    match { .page, .offset } = tup;
    return offset;
endfunction


//
// mkFUNCP_TLB --
//   This module provides a single data or instruction TLB interface.  It has
//   a tiny cache of the most recent translations.  On a miss in that cache
//   it queries the parent class for a translation through the provided request/
//   response FIFOs.
//
module [HASIM_MODULE] mkFUNCP_TLB#(FUNCP_TLB_TYPE tlbType,
                                   VTOP_REQ_FIFO reqVtoP,
                                   VTOP_RESP_FIFO respVtoP,
                                   DEBUG_FILE debugLog)
    // Interface:
        (FUNCP_TLB#(n_CACHE_ENTRIES))
    provisos(Log#(n_CACHE_ENTRIES, TLog#(n_CACHE_ENTRIES)));
   
    String i_or_d = (tlbType == FUNCP_ITLB ? "I" : "D");

    // ***** Statistics *****

    let hit_stat_id  = (tlbType == FUNCP_ITLB) ? `STATS_FUNCP_TLB_L1_ITLB_HIT : `STATS_FUNCP_TLB_L1_DTLB_HIT;
    Stat statTLBHit <- mkStatCounter(hit_stat_id);

    let miss_stat_id = (tlbType == FUNCP_ITLB) ? `STATS_FUNCP_TLB_L1_ITLB_MISS : `STATS_FUNCP_TLB_L1_DTLB_MISS;
    Stat statTLBMiss <- mkStatCounter(miss_stat_id);

    // ***** Local State *****
    
    // State for an active request
    Reg#(ISA_ADDRESS) reqVA <- mkRegU();
    Reg#(FUNCP_TLB_STATE) state <- mkReg(FUNCP_TLB_IDLE);

    // Lookup response FIFO
    FIFO#(Maybe#(MEM_ADDRESS)) response <- mkFIFO();

    // Small local cache of recent translations
    HASIM_TINY_CACHE#(FUNCP_V_PAGE, FUNCP_P_PAGE, n_CACHE_ENTRIES) tinyCache <- mkTinyCache();

    // ***** Rules *****

    //
    // translate_VtoP_response --
    //   Wait for response from TLB cache or hybrid memory.
    //
    rule translate_VtoP_response (state == FUNCP_TLB_BUSY);

        // pop a request from the link
        let resp = respVtoP.first();
        respVtoP.deq();

        state <= FUNCP_TLB_IDLE;

        if (resp matches tagged Valid .pp)
        begin
            // Valid translation
            let pa = paFromPage(pp, pageOffsetFromVA(reqVA));
            response.enq(tagged Valid pa);

            debugLog.record($format("  %s VtoP response: VA 0x%x -> PA 0x%x", i_or_d, reqVA, pa));

            // Store the translation in the small cache
            tinyCache.write(pageFromVA(reqVA), pp);
        end
        else
        begin
            debugLog.record($format("  %s translation failed: VA 0x%x", i_or_d, reqVA));

            response.enq(tagged Invalid);
        end

    endrule


    // ***** Methods *****

    method Action lookupReq(TOKEN tok, ISA_ADDRESS va) if (state == FUNCP_TLB_IDLE);

        debugLog.record($format("%s req: VA 0x%x", i_or_d, va));

        FUNCP_V_PAGE vp = pageFromVA(va);

        let tc <- tinyCache.read(vp);
        if (tc matches tagged Valid .pp)
        begin
            //
            // Quick path hit.  Simply return the translation now.
            //
            MEM_ADDRESS pa = paFromPage(pp, pageOffsetFromVA(va));
            response.enq(tagged Valid pa);

            debugLog.record($format("  %s quick hit: VA 0x%x -> PA 0x%x", i_or_d, va, pa));
            statTLBHit.incr();
        end
        else
        begin
            //
            // Ask the memory service for a translation.
            //
            reqVtoP.enq(vp);

            reqVA <= va;
            state <= FUNCP_TLB_BUSY;
            statTLBMiss.incr();
        end

    endmethod

    method ActionValue#(Maybe#(MEM_ADDRESS)) lookupResp();
        let r = response.first();
        response.deq();
        return r;
    endmethod

endmodule


//
// mkVtoPInterface --
//   The interface between the main shared translation cache and the hybrid
//   memory service.
//
module [HASIM_MODULE] mkVtoPInterface
    // interface:
        (HASIM_CACHE_SOURCE_DATA#(FUNCP_V_PAGE, Maybe#(FUNCP_P_PAGE)));

    // Connection to memory translation service
    Connection_Client#(ISA_ADDRESS, MEM_ADDRESS) link_memory <- mkConnection_Client("funcp_memory_VtoP");

    method Action readReq(FUNCP_V_PAGE vp);
        link_memory.makeReq(vaFromPage(vp, 0));
    endmethod

    method ActionValue#(Maybe#(FUNCP_P_PAGE)) readResp();
        MEM_ADDRESS pa = link_memory.getResp();
        link_memory.deq();

        // Sending a bit over RRR is difficult.  Instead, use the low bit
        // to signal whether the translation is valid.
        Maybe#(FUNCP_P_PAGE) resp;
        if (pa[0] == 0)
            resp = tagged Valid pageFromPA(pa);
        else
            resp = tagged Invalid;

        return resp;
    endmethod

    // No writes can happen
    method Action write(FUNCP_V_PAGE vp, Maybe#(FUNCP_P_PAGE) pp);
        noAction;
    endmethod

    method Action writeSyncReq(FUNCP_V_PAGE vp, Maybe#(FUNCP_P_PAGE) pp);
        noAction;
    endmethod

    method Action writeSyncWait();
        noAction;
    endmethod

endmodule


//
// mkTLBCacheStats --
//   Statistics for the main, shared, translation cache.
//
module [HASIM_MODULE] mkTLBCacheStats
    // interface:
        (HASIM_CACHE_STATS);
    
    // ***** Statistics *****

    Stat statTLBMiss <- mkStatCounter(`STATS_FUNCP_TLB_L2_MISS);

    method Action readHit();
    endmethod

    method Action readMiss();
        statTLBMiss.incr();
    endmethod

    method Action writeHit();
    endmethod

    method Action writeMiss();
    endmethod

    method Action invalLine();
    endmethod

    method Action dirtyLineFlush();
    endmethod

    method Action forceInvalLine();
    endmethod

endmodule


//
// mkFUNCP_CPU_TLBS --
//   Allocates a pair of TLB interfaces:  one instruction and one data.
//   The module allocates soft connections for ITLB and DTLB connections
//   with the functional register state manager.
//
//   Requests are routed through a unified large cache here.  On a miss in the
//   translation cache, requests are automatically routed by the cache through
//   the mkVtoPInterface module above to the hybrid memory service.
//
module [HASIM_MODULE] mkFUNCP_CPU_TLBS
    // interface:
        ();

    DEBUG_FILE debugLog <- mkDebugFile(`FUNCP_TLB_LOGFILE_NAME);

    // Connections to functional register state manager
    Connection_Server#(FUNCP_TLB_QUERY, Maybe#(MEM_ADDRESS)) link_funcp_itlb <- mkConnection_Server("funcp_itlb");
    Connection_Server#(FUNCP_TLB_QUERY, Maybe#(MEM_ADDRESS)) link_funcp_dtlb <- mkConnection_Server("funcp_dtlb");

    // ITLB
    VTOP_REQ_FIFO  itlb_vtop_req <- mkFIFO();
    VTOP_RESP_FIFO itlb_vtop_resp <- mkFIFO();
    FUNCP_TLB#(2) itlb <- mkFUNCP_TLB(FUNCP_ITLB, itlb_vtop_req, itlb_vtop_resp, debugLog);

    // DTLB
    VTOP_REQ_FIFO  dtlb_vtop_req <- mkFIFO();
    VTOP_RESP_FIFO dtlb_vtop_resp <- mkFIFO();
    FUNCP_TLB#(4) dtlb <- mkFUNCP_TLB(FUNCP_DTLB, dtlb_vtop_req, dtlb_vtop_resp, debugLog);

    HASIM_CACHE_SOURCE_DATA#(FUNCP_V_PAGE, Maybe#(FUNCP_P_PAGE)) vtopIfc <- mkVtoPInterface();
    HASIM_CACHE_STATS statIfc <- mkTLBCacheStats();

    // Translation cache
    HASIM_CACHE#(FUNCP_V_PAGE,
                 Maybe#(FUNCP_P_PAGE),
                 `FUNCP_TLB_CACHE_SETS,
                 `FUNCP_TLB_CACHE_WAYS,
                 `FUNCP_ISA_PAGE_SHIFT) cache <- mkCacheSetAssoc(vtopIfc, statIfc, debugLog);

    FIFO#(FUNCP_TLB_TYPE) pendingTLBQ <- mkFIFO();


    // ***** Rules for communcation with functional register state manager *****
    
    rule itlb_req (True);
        // pop a request from the link
        match { .tok, .va } = link_funcp_itlb.getReq();
        link_funcp_itlb.deq();

        itlb.lookupReq(tok, va);
    endrule

    rule itlb_resp (True);
        let resp <- itlb.lookupResp();
        link_funcp_itlb.makeResp(resp);
    endrule

    rule dtlb_req (True);
        // pop a request from the link
        match { .tok, .va } = link_funcp_dtlb.getReq();
        link_funcp_dtlb.deq();

        dtlb.lookupReq(tok, va);
    endrule

    rule dtlb_resp (True);
        let resp <- dtlb.lookupResp();
        link_funcp_dtlb.makeResp(resp);
    endrule



    // ***** Managing translation requests from the child ITLB and DTLB *****
    
    rule translate_VtoP_I_request (True);
        FUNCP_V_PAGE vp = itlb_vtop_req.first();
        itlb_vtop_req.deq();

        debugLog.record($format("  I hybrid req: VA 0x%x", vaFromPage(vp, 0)));

        pendingTLBQ.enq(FUNCP_ITLB);
        cache.readReq(vp);
    endrule

    rule translate_VtoP_D_request (True);
        FUNCP_V_PAGE vp = dtlb_vtop_req.first();
        dtlb_vtop_req.deq();

        debugLog.record($format("  D hybrid req: VA 0x%x", vaFromPage(vp, 0)));

        pendingTLBQ.enq(FUNCP_DTLB);
        cache.readReq(vp);
    endrule

    (* descending_urgency= "translate_VtoP_D_request, translate_VtoP_I_request" *)

    rule translate_VtoP_response (True);

        let resp <- cache.readResp();

        let t = pendingTLBQ.first();
        pendingTLBQ.deq();

        if (t == FUNCP_ITLB)
            itlb_vtop_resp.enq(resp);
        else
            dtlb_vtop_resp.enq(resp);

    endrule

endmodule
