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
// Query passed to TLB service.  Normal queries and faults have the same
// payload, but different semantics.  Normal queries may fail is the page
// is not mapped.  Fault requests allocate the page and return no response.
//
typedef struct
{
    ISA_ADDRESS va;
    TOKEN tok;
}
FUNCP_TLB_QUERY
    deriving (Eq, Bits);

typedef FUNCP_TLB_QUERY FUNCP_TLB_FAULT;

//
// Helper functions for constructing FUNCP_TLB_QUERY
//
function FUNCP_TLB_QUERY normalTLBQuery(TOKEN tok, ISA_ADDRESS va);
    return FUNCP_TLB_QUERY { va: va, tok: tok };
endfunction

function FUNCP_TLB_FAULT handleTLBPageFault(TOKEN tok, ISA_ADDRESS va);
    return FUNCP_TLB_FAULT { va: va, tok: tok };
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
    method Action lookupReq(TOKEN tok, ISA_ADDRESS va, Bool alloc_on_fault);
    method ActionValue#(FUNCP_TLB_RESP) lookupResp();
endinterface: FUNCP_TLB


typedef enum
{
    FUNCP_TLB_IDLE,
    FUNCP_TLB_BUSY
}
FUNCP_TLB_STATE
    deriving (Eq, Bits);


//
// Translation request
//
typedef struct
{
    Bool alloc_on_fault;
    FUNCP_V_PAGE vp;
}
FUNCP_TRANSLATION_REQ
    deriving (Eq, Bits);

//
// Details about a translation
//
typedef struct
{
    Bool page_fault;       // Translation failed.  Raised a page fault.
    FUNCP_P_PAGE page;
}
FUNCP_TRANSLATION_RESP
    deriving (Eq, Bits);

//
// Request/response FIFOs from individual TLBs to the containing module.
// The containing module has a unified I/D TLB cache and manages misses
// to hybrid memory translations.
//
typedef FIFO#(FUNCP_TRANSLATION_REQ) VTOP_REQ_FIFO;
typedef FIFO#(FUNCP_TRANSLATION_RESP) VTOP_RESP_FIFO;


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

    let no_translation_stat_id = (tlbType == FUNCP_ITLB) ? `STATS_FUNCP_TLB_ITLB_NOT_MAPPED : `STATS_FUNCP_TLB_DTLB_NOT_MAPPED;
    Stat statTLBNoTranslation <- mkStatCounter(no_translation_stat_id);

    let page_fault_stat_id = (tlbType == FUNCP_ITLB) ? `STATS_FUNCP_TLB_ITLB_PAGE_FAULT : `STATS_FUNCP_TLB_DTLB_PAGE_FAULT;
    Stat statTLBPageFault <- mkStatCounter(page_fault_stat_id);

    // ***** Local State *****
    
    // State for an active request
    Reg#(ISA_ADDRESS) reqVA <- mkRegU();
    Reg#(FUNCP_TLB_STATE) state <- mkReg(FUNCP_TLB_IDLE);

    // Lookup response FIFO
    FIFO#(FUNCP_TLB_RESP) response <- mkFIFO();

    // Small local cache of recent translations
    HASIM_TINY_CACHE#(FUNCP_V_PAGE,
                      FUNCP_P_PAGE,
                      n_CACHE_ENTRIES,
                      `FUNCP_ISA_PAGE_SHIFT) tinyCache <- mkTinyCache(debugLog);

    // Constructors
    
    function FUNCP_TLB_RESP validTranslation(MEM_ADDRESS pa);
        return FUNCP_TLB_RESP { page_fault: False, pa: pa };
    endfunction
    
    function FUNCP_TLB_RESP invalidTranslation(MEM_ADDRESS pa);
        return FUNCP_TLB_RESP { page_fault: True, pa: pa };
    endfunction


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

        let pa = paFromPage(resp.page, pageOffsetFromVA(reqVA));

        if (! resp.page_fault)
        begin
            debugLog.record($format("  %s VtoP response: VA 0x%x -> PA 0x%x", i_or_d, reqVA, pa));
            response.enq(validTranslation(pa));

            // Store the translation in the small cache
            tinyCache.write(pageFromVA(reqVA), resp.page);
        end
        else
        begin
            debugLog.record($format("  %s VtoP response: VA 0x%x -> PA 0x%x [PAGE FAULT]", i_or_d, reqVA, pa));
            response.enq(invalidTranslation(pa));

            statTLBNoTranslation.incr();
        end

    endrule


    // ***** Methods *****

    method Action lookupReq(TOKEN tok, ISA_ADDRESS va, Bool alloc_on_fault) if (state == FUNCP_TLB_IDLE);

        debugLog.record($format("%s req: VA 0x%x", i_or_d, va));

        FUNCP_V_PAGE vp = pageFromVA(va);

        if (alloc_on_fault)
        begin
            // Page fault handler is allocating a new page.
            statTLBPageFault.incr();
        end

        let tc <- tinyCache.read(vp);
        if (tc matches tagged Valid .pp)
        begin
            //
            // Quick path hit.  Simply return the translation now.
            //
            MEM_ADDRESS pa = paFromPage(pp, pageOffsetFromVA(va));
            response.enq(validTranslation(pa));

            debugLog.record($format("  %s quick hit: VA 0x%x -> PA 0x%x", i_or_d, va, pa));
            statTLBHit.incr();
        end
        else
        begin
            //
            // Ask the memory service for a translation.
            //
            reqVtoP.enq(FUNCP_TRANSLATION_REQ { alloc_on_fault: alloc_on_fault, vp: vp });

            reqVA <= va;
            state <= FUNCP_TLB_BUSY;
            statTLBMiss.incr();
        end

    endmethod

    method ActionValue#(FUNCP_TLB_RESP) lookupResp();
        let r = response.first();
        response.deq();
        return r;
    endmethod

endmodule


// ===================================================================
//
// System interface (connection between L2 cache and host)
//
// ===================================================================

interface FUNCP_TLB_CACHE_INTERFACE;

    interface HASIM_CACHE_SOURCE_DATA#(FUNCP_V_PAGE, FUNCP_TRANSLATION_RESP) cacheIfc;

    method Action refMetaData(Bool alloc_on_fault);

endinterface: FUNCP_TLB_CACHE_INTERFACE

//
// mkVtoPInterface --
//   The interface between the main shared translation cache and the hybrid
//   memory service.
//
module [HASIM_MODULE] mkVtoPInterface
    // interface:
        (FUNCP_TLB_CACHE_INTERFACE);

    // Connection to memory translation service
    Connection_Client#(ISA_ADDRESS, MEM_ADDRESS) link_memory <- mkConnection_Client("funcp_memory_VtoP");

    // Is the cache in allocate on fault mode?  This works as a register
    // because the cache can process only one request at a time.
    Reg#(Bool) allocOnFault <- mkRegU();

    // This is the standard interface passed to the cache...
    interface HASIM_CACHE_SOURCE_DATA cacheIfc;

        method Action readReq(FUNCP_V_PAGE vp);
            // Sending a bit over RRR is difficult.  Instead, use the low bit
            // to signal whether allocate on fault is set.
            link_memory.makeReq(vaFromPage(vp, zeroExtend(pack(allocOnFault))));
        endmethod

        method ActionValue#(FUNCP_TRANSLATION_RESP) readResp();
            MEM_ADDRESS pa = link_memory.getResp();
            link_memory.deq();

            // The low bit signals whether the translation is valid.
            return FUNCP_TRANSLATION_RESP { page_fault: pa[0] == 1, page: pageFromPA(pa) };
        endmethod

        // No writes can happen
        method Action write(FUNCP_V_PAGE vp, FUNCP_TRANSLATION_RESP pp);
            noAction;
        endmethod

        method Action writeSyncReq(FUNCP_V_PAGE vp, FUNCP_TRANSLATION_RESP pp);
            noAction;
        endmethod

        method Action writeSyncWait();
            noAction;
        endmethod
    
    endinterface: cacheIfc

    method Action refMetaData(Bool alloc_on_fault);
        allocOnFault <= alloc_on_fault;
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


// ===================================================================
//
// Main module.  Connection to register state manager and TLB.
//
// ===================================================================

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

    // Connections to functional register state manager translation pipelines
    Connection_Server#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP) link_funcp_itlb_trans <- mkConnection_Server("funcp_itlb_translate");
    Connection_Server#(FUNCP_TLB_QUERY, FUNCP_TLB_RESP) link_funcp_dtlb_trans <- mkConnection_Server("funcp_dtlb_translate");

    // Connections to functional fault handler
    Connection_Receive#(FUNCP_TLB_FAULT) link_funcp_itlb_fault <- mkConnection_Receive("funcp_itlb_pagefault");
    Connection_Receive#(FUNCP_TLB_FAULT) link_funcp_dtlb_fault <- mkConnection_Receive("funcp_dtlb_pagefault");

    // ITLB
    VTOP_REQ_FIFO  itlb_vtop_req <- mkFIFO();
    VTOP_RESP_FIFO itlb_vtop_resp <- mkFIFO();
    FUNCP_TLB#(2) itlb <- mkFUNCP_TLB(FUNCP_ITLB, itlb_vtop_req, itlb_vtop_resp, debugLog);

    // DTLB
    VTOP_REQ_FIFO  dtlb_vtop_req <- mkFIFO();
    VTOP_RESP_FIFO dtlb_vtop_resp <- mkFIFO();
    FUNCP_TLB#(4) dtlb <- mkFUNCP_TLB(FUNCP_DTLB, dtlb_vtop_req, dtlb_vtop_resp, debugLog);

    FUNCP_TLB_CACHE_INTERFACE vtopIfc <- mkVtoPInterface();
    HASIM_CACHE_STATS statIfc <- mkTLBCacheStats();

    // Translation cache
    HASIM_CACHE#(FUNCP_V_PAGE,
                 FUNCP_TRANSLATION_RESP,
                 `FUNCP_TLB_CACHE_SETS,
                 `FUNCP_TLB_CACHE_WAYS,
                 `FUNCP_ISA_PAGE_SHIFT) cache <- mkCacheSetAssoc(vtopIfc.cacheIfc, statIfc, debugLog);

    FIFO#(Tuple2#(FUNCP_V_PAGE, FUNCP_TLB_TYPE)) pendingTLBQ <- mkFIFO1();
    FIFO#(Bool) itlbQ <- mkFIFO();
    FIFO#(Bool) dtlbQ <- mkFIFO();


    // ***** Rules for communcation with functional register state manager *****
    
    rule itlb_req (True);
        //
        // Get either a fault request or a normal translation request.  Give
        // priority to page faults.
        //
        if (link_funcp_itlb_fault.notEmpty())
        begin
            let r = link_funcp_itlb_fault.receive();
            link_funcp_itlb_fault.deq();

            itlb.lookupReq(r.tok, r.va, True);
            itlbQ.enq(True);
        end
        else
        begin
            let r = link_funcp_itlb_trans.getReq();
            link_funcp_itlb_trans.deq();

            itlb.lookupReq(r.tok, r.va, False);
            itlbQ.enq(False);
        end
    endrule

    rule itlb_resp (True);
        let resp <- itlb.lookupResp();

        let page_fault = itlbQ.first();
        itlbQ.deq();

        // Respond only for normal lookup.  Page fault gets no response.
        if (! page_fault)
            link_funcp_itlb_trans.makeResp(resp);
    endrule

    rule dtlb_req (True);
        //
        // Get either a fault request or a normal translation request.  Give
        // priority to page faults.
        //
        if (link_funcp_dtlb_fault.notEmpty())
        begin
            let r = link_funcp_dtlb_fault.receive();
            link_funcp_dtlb_fault.deq();

            dtlb.lookupReq(r.tok, r.va, True);
            dtlbQ.enq(True);
        end
        else
        begin
            let r = link_funcp_dtlb_trans.getReq();
            link_funcp_dtlb_trans.deq();

            dtlb.lookupReq(r.tok, r.va, False);
            dtlbQ.enq(False);
        end
    endrule

    rule dtlb_resp (True);
        let resp <- dtlb.lookupResp();

        let page_fault = dtlbQ.first();
        dtlbQ.deq();

        // Respond only for normal lookup.  Page fault gets no response.
        if (! page_fault)
            link_funcp_dtlb_trans.makeResp(resp);
    endrule



    // ***** Managing translation requests from the child ITLB and DTLB *****
    
    rule translate_VtoP_I_request (True);
        let req = itlb_vtop_req.first();
        itlb_vtop_req.deq();

        debugLog.record($format("  I hybrid req: VA 0x%x", vaFromPage(req.vp, 0)));

        vtopIfc.refMetaData(req.alloc_on_fault);
        pendingTLBQ.enq(tuple2(req.vp, FUNCP_ITLB));
        cache.readReq(req.vp);
    endrule

    rule translate_VtoP_D_request (True);
        let req = dtlb_vtop_req.first();
        dtlb_vtop_req.deq();

        debugLog.record($format("  D hybrid req: VA 0x%x", vaFromPage(req.vp, 0)));

        vtopIfc.refMetaData(req.alloc_on_fault);
        pendingTLBQ.enq(tuple2(req.vp, FUNCP_DTLB));
        cache.readReq(req.vp);
    endrule

    (* descending_urgency= "translate_VtoP_D_request, translate_VtoP_I_request" *)

    rule translate_VtoP_response (True);

        let resp <- cache.readResp();

        match { .vp, .which_tlb } = pendingTLBQ.first();
        pendingTLBQ.deq();

        // Don't cache invalid or uncacheable translations.
        if (resp.page_fault)
        begin
            debugLog.record($format("    Uncacheable: VA 0x%x -> PA 0x%x", vaFromPage(vp, 0), paFromPage(resp.page, 0)));
            cache.invalReq(vp, False);
        end

        if (which_tlb == FUNCP_ITLB)
            itlb_vtop_resp.enq(resp);
        else
            dtlb_vtop_resp.enq(resp);

    endrule

endmodule
