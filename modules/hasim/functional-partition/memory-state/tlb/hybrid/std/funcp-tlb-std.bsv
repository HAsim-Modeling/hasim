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
// Standard hybrid TLB.  Translations are stored in the central cache.  A
// private L1 cache is also allocated.
//

// Library includes.

import FIFO::*;
import Vector::*;
import FShow::*;
import DefaultValue::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_services_lib.bsh"
`include "asim/provides/soft_services_deps.bsh"
`include "asim/provides/common_services.bsh"
`include "asim/provides/mem_services.bsh"
`include "asim/provides/central_cache.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/dict/VDEV_CACHE.bsh"
`include "asim/dict/PARAMS_FUNCP_MEMSTATE_TLB.bsh"
`include "asim/rrr/remote_client_stub_FUNCP_TLB.bsh"


// ===================================================================
//
// PRIVATE DATA STRUCTURES
//
// ===================================================================

//
// Translation cache index
//
typedef struct
{
    CONTEXT_ID context_id;
    FUNCP_V_PAGE vp;
}
FUNCP_TLB_IDX
    deriving (Eq, Bits);

function FUNCP_TLB_IDX tlbCacheIdx(CONTEXT_ID ctx_id, FUNCP_V_PAGE vp);
    FUNCP_TLB_IDX idx;
    idx.context_id = ctx_id;
    idx.vp = vp;

    return idx;
endfunction


//
// Translation cache value
//
typedef struct
{
    Bool ioSpace;          // Memory mapped I/O.
    Bool pageFault;        // Translation failed.  Raised a page fault.
    FUNCP_P_PAGE page;
}
FUNCP_TLB_ENTRY
    deriving (Eq, Bits);


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
// refInfo describing a request
//
typedef 4 FUNCP_TLB_REFS;
typedef struct
{
    // Instruction or data?
    Bool isInstrReq;

    // Allocate an untranslated page?
    Bool allocOnFault;

    // Page to which the allocOnFault bit applies.  Fill requests from the central
    // cache are grouped in lines.  With this field the simulator can determine
    // which page to allocate.
    CENTRAL_CACHE_WORD_IDX allocWordIdx;

    // refIdx will be used to sort responses so they are returned in order
    SCOREBOARD_FIFO_ENTRY_ID#(FUNCP_TLB_REFS) refIdx;
}
FUNCP_TLB_REFINFO
    deriving (Eq, Bits);


//
// State machine for ordering requests.
//
typedef enum
{
    FUNCP_TLB_STATE_ITRANS,
    FUNCP_TLB_STATE_DTRANS
}
FUNCP_TLB_STATE
    deriving (Eq, Bits);


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
//   the mkVtoPInterface module below to the hybrid memory service.
//
module [HASIM_MODULE] mkFUNCP_CPU_TLBS
    // interface:
    ();

    DEBUG_FILE debugLog <- mkDebugFile(`FUNCP_TLB_LOGFILE_NAME);

    // Debugging output stream, useful for getting a stream of status messages
    // when running on an FPGA.
    STDIO#(Bit#(64)) stdio <- mkStdIO_Debug();
    let msgIReq <- getGlobalStringUID("FUNCP TLB: ITranslate REQ ctx=%d, va=0x%016llx\n");
    let msgIRsp <- getGlobalStringUID("FUNCP TLB: ITranslate RSP pa=0x%016llx, io=%d, fault=%d\n");
    let msgDReq <- getGlobalStringUID("FUNCP TLB: DTranslate REQ ctx=%d, va=0x%016llx\n");
    let msgDRsp <- getGlobalStringUID("FUNCP TLB: DTranslate RSP pa=0x%016llx, io=%d, fault=%d\n");

    // Connections to functional register state manager translation pipelines
    Connection_Server#(Maybe#(FUNCP_TLB_QUERY), FUNCP_TLB_RESP) link_funcp_itlb_trans <- mkConnection_Server("funcp_itlb_translate");
    Connection_Server#(Maybe#(FUNCP_TLB_QUERY), FUNCP_TLB_RESP) link_funcp_dtlb_trans <- mkConnection_Server("funcp_dtlb_translate");

    // Connections to functional fault handler
    Connection_Receive#(FUNCP_TLB_FAULT) link_funcp_itlb_fault <- mkConnection_Receive("funcp_itlb_pagefault");
    Connection_Receive#(FUNCP_TLB_FAULT) link_funcp_dtlb_fault <- mkConnection_Receive("funcp_dtlb_pagefault");

    // State enforces order of I and D translation.  Side effects of translation
    // requests to the host may cause run-to-run mapping changes, which are
    // hard to debug.  We treat TLB queries like A-Ports.  The model must request
    // a translation or send no-message once for instructions and then once
    // for data.  The next context's instruction translation will not fire until
    // the data translation request is received for the current context.
    // This is why requests are protected by Maybe#(), to allow for no-message.
    Reg#(FUNCP_TLB_STATE) state <- mkReg(FUNCP_TLB_STATE_ITRANS);

    // Connection between central cache and translation RRR service
    ClientStub_FUNCP_TLB clientTranslationStub <- mkClientStub_FUNCP_TLB();
    let vtopIfc <- mkVtoPInterface(clientTranslationStub, debugLog);

    // Reorder buffer for cache responses.  Cache doesn't guarantee ordered return.
    SCOREBOARD_FIFOF#(FUNCP_TLB_REFS, FUNCP_TLB_ENTRY) itlbTransRespQ <- mkScoreboardFIFOF();
    SCOREBOARD_FIFOF#(FUNCP_TLB_REFS, FUNCP_TLB_ENTRY) dtlbTransRespQ <- mkScoreboardFIFOF();

    // Page offset used in final step for returning true PA
    FIFO#(FUNCP_PAGE_OFFSET) itlbReqInfoQ <- mkSizedFIFO(valueOf(FUNCP_TLB_REFS));
    FIFO#(FUNCP_PAGE_OFFSET) dtlbReqInfoQ <- mkSizedFIFO(valueOf(FUNCP_TLB_REFS));

    // Translation cache and cache prefetcher
    NumTypeParam#(`FUNCP_TLB_PVT_ENTRIES) num_pvt_entries = ?;
    NumTypeParam#(`FUNCP_TLB_PVT_CACHE_PREFETCH_LEARNER_NUM) num_prefetch_learners = ?;
    
    let prefetcher <- (`FUNCP_TLB_PVT_PREFETCH_ENABLE == 1)?
                      mkCachePrefetcher(num_prefetch_learners, False, True, debugLog):
                      mkNullCachePrefetcher();

    CENTRAL_CACHE_CLIENT#(FUNCP_TLB_IDX,      // Cache address type
                          FUNCP_TLB_ENTRY,    // Cache word
                          FUNCP_TLB_REFINFO)
        cache <- mkCentralCacheClient(`VDEV_CACHE_FUNCP_TLB,
                                      num_pvt_entries,
                                      prefetcher,
                                      True,
                                      vtopIfc);

    // Cache read metadata that forces requests to the host to be ordered.
    // Calling the host for a translation may have memory allocation side
    // effects.  Order prevents run to run variation.
    RL_CACHE_GLOBAL_READ_META orderedReqs = defaultValue();
    orderedReqs.orderedSourceDataReqs = True;

    let statIfc <- mkTLBCacheStats(cache.stats);
    let prefetchStatIfc <- (`FUNCP_TLB_PVT_PREFETCH_ENABLE == 1)?
                           mkTLBPrefetchStats(num_prefetch_learners, prefetcher.stats):
                           mkNullTLBPrefetchStats(num_prefetch_learners, prefetcher.stats);

    // Dynamic parameters
    PARAMETER_NODE paramNode         <- mkDynamicParameterNode();
    Param#(3) cacheMode              <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_TLB_FUNCP_TLB_PVT_CACHE_MODE, paramNode);
    Param#(6) prefetchMechanism      <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_TLB_FUNCP_TLB_PREFETCHER_MECHANISM, paramNode);
    Param#(4) prefetchLearnerSizeLog <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_TLB_FUNCP_TLB_PREFETCHER_LEARNER_SIZE_LOG, paramNode);
    Param#(2) prefetchPrioritySpec   <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_TLB_FUNCP_TLB_PREFETCHER_PRIORITY_SPEC, paramNode);
    //
    // Function to enforce order of instruction vs. data translations.
    // Order may be important for virtual to physical translation in
    // models that map pages at run time, since changes in order
    // lead to different run-to-run mappings.
    //
    function Bool orderIsValid(FUNCP_TLB_STATE expectedState);
        return ((`FUNCP_TLB_ENFORCE_ORDER == 0) ||
                (state == expectedState));
    endfunction


    // ====================================================================
    //
    // Initialization
    //
    // ====================================================================

    Reg#(Bool) initialized <- mkReg(False);
    rule doInit (! initialized);
        cache.setCacheMode(unpack(cacheMode[1:0]), unpack(cacheMode[2]));
        prefetcher.setPrefetchMode(unpack(prefetchMechanism), unpack(prefetchLearnerSizeLog), unpack(prefetchPrioritySpec));
        initialized <= True;
    endrule

    //
    // itlbFaultReq --
    //     Consume request to allocate an instruction page.  There will be no
    //     response.
    //
    //     This rule is broken into two passes.  The first pass drops any
    //     current translations from the TLB, since the translation most
    //     likely indicates an invalid page.  The second pass allocates a
    //     page mapping.
    //
    (* conservative_implicit_conditions *)
    rule itlbFaultReq (True);
        let r = link_funcp_itlb_fault.receive();
        link_funcp_itlb_fault.deq();

        let vp = pageFromVA(r.va);
        let idx = tlbCacheIdx(r.contextId, vp);

        // Drop current TLB entry since the failed translation is cached.
        cache.invalReq(idx, True);

        // Force allocation
        clientTranslationStub.makeRequest_ActivateVAddr(contextIdToRRR(r.contextId),
                                                        vaFromPage(vp, 0),
                                                        1);

        debugLog.record($format("I Alloc: ctx=%0d, va=0x%x, vp=0x%x", r.contextId, r.va, vp));
    endrule

    //
    // itlbTransReq --
    //     Standard instruction page translation request.
    //

    let stdioIReq <- mkStdIO_CondPrintf(ioMask_FUNCP_MEMSTATE, stdio);

    rule itlbTransReq (link_funcp_itlb_trans.getReq() matches tagged Valid .r &&&
                       orderIsValid(FUNCP_TLB_STATE_ITRANS) &&&
                       ! link_funcp_itlb_fault.notEmpty &&&
                       ! link_funcp_dtlb_fault.notEmpty);
        link_funcp_itlb_trans.deq();

        // Enforce order of queries
        if (! r.notLastQuery)
        begin
            state <= FUNCP_TLB_STATE_DTRANS;
        end

        debugLog.record($format("I Req: ctx=%0d, va=0x%x", r.contextId, r.va));
        // Pack state into 64 bits as best we can
        stdioIReq.printf(msgIReq, list2(zeroExtend(r.contextId), resize(r.va)));

        let vp = pageFromVA(r.va);

        let idx <- itlbTransRespQ.enq();
        cache.readReq(tlbCacheIdx(r.contextId, vp),
                      FUNCP_TLB_REFINFO { isInstrReq: True,
                                          allocOnFault: False,
                                          allocWordIdx: truncate(vp),
                                         refIdx: idx },
                      orderedReqs);

        itlbReqInfoQ.enq(pageOffsetFromVA(r.va));
    endrule

    rule itlbTransReqNoMsg (! isValid(link_funcp_itlb_trans.getReq) &&
                            orderIsValid(FUNCP_TLB_STATE_ITRANS));
        link_funcp_itlb_trans.deq();
        state <= FUNCP_TLB_STATE_DTRANS;
    endrule

    //
    // itlbResp --
    //     Response for instruction page translation.
    //

    let stdioIResp <- mkStdIO_CondPrintf(ioMask_FUNCP_MEMSTATE, stdio);

    rule itlbResp (True);
        // Translation cache response
        let v = itlbTransRespQ.first();
        itlbTransRespQ.deq();

        // Request details
        let offset = itlbReqInfoQ.first();
        itlbReqInfoQ.deq();

        FUNCP_TLB_RESP resp;
        resp.ioSpace = v.ioSpace;
        resp.pageFault = v.pageFault;
        resp.pa = paFromPage(v.page, offset);

        debugLog.record($format("I Resp: pa=0x%x, io=%0d, fault=%0d", resp.pa, resp.ioSpace, resp.pageFault));
        stdioIResp.printf(msgIRsp, list3(zeroExtend(resp.pa),
                                         zeroExtend(pack(resp.ioSpace)),
                                         zeroExtend(pack(resp.pageFault))));

        link_funcp_itlb_trans.makeResp(resp);
    endrule



    //
    // dtlbFaultReq --
    //     Consume request to allocate a data page.  There will be no
    //     response.
    //
    (* conservative_implicit_conditions *)
    rule dtlbFaultReq (True);
        let r = link_funcp_dtlb_fault.receive();
        link_funcp_dtlb_fault.deq();

        let vp = pageFromVA(r.va);
        let idx = tlbCacheIdx(r.contextId, vp);

        // Drop current TLB entry since the failed translation is cached.
        cache.invalReq(idx, True);

        // Force allocation
        clientTranslationStub.makeRequest_ActivateVAddr(contextIdToRRR(r.contextId),
                                                        vaFromPage(vp, 0),
                                                        0);

        debugLog.record($format("D Alloc: ctx=%0d, va=0x%x, vp=0x%x", r.contextId, r.va, vp));
    endrule

    //
    // dtlbTransReq --
    //     Standard data page translation request.
    //

    let stdioDReq <- mkStdIO_CondPrintf(ioMask_FUNCP_MEMSTATE, stdio);

    (* descending_urgency = "itlbFaultReq, itlbTransReq, dtlbFaultReq, dtlbTransReq" *)
    rule dtlbTransReq (link_funcp_dtlb_trans.getReq() matches tagged Valid .r &&&
                       orderIsValid(FUNCP_TLB_STATE_DTRANS) &&&
                       ! link_funcp_itlb_fault.notEmpty &&&
                       ! link_funcp_dtlb_fault.notEmpty);
        link_funcp_dtlb_trans.deq();

        // Enforce order of queries
        if (! r.notLastQuery)
        begin
            state <= FUNCP_TLB_STATE_ITRANS;
        end

        debugLog.record($format("D Req: ctx=%0d, va=0x%x", r.contextId, r.va));
        stdioDReq.printf(msgDReq, list2(zeroExtend(r.contextId), resize(r.va)));

        let vp = pageFromVA(r.va);

        let idx <- dtlbTransRespQ.enq();
        cache.readReq(tlbCacheIdx(r.contextId, vp),
                      FUNCP_TLB_REFINFO { isInstrReq: False,
                                          allocOnFault: False,
                                          allocWordIdx: truncate(vp),
                                          refIdx: idx },
                      orderedReqs);

        dtlbReqInfoQ.enq(pageOffsetFromVA(r.va));
    endrule

    rule dtlbTransReqNoMsg (! isValid(link_funcp_dtlb_trans.getReq) &&
                            orderIsValid(FUNCP_TLB_STATE_DTRANS));
        link_funcp_dtlb_trans.deq();
        state <= FUNCP_TLB_STATE_ITRANS;
    endrule

    //
    // dtlbResp --
    //     Response for data page translation.
    //

    let stdioDResp <- mkStdIO_CondPrintf(ioMask_FUNCP_MEMSTATE, stdio);

    rule dtlbResp (True);
        // Translation cache response
        let v = dtlbTransRespQ.first();
        dtlbTransRespQ.deq();

        // Request details
        let offset = dtlbReqInfoQ.first();
        dtlbReqInfoQ.deq();

        FUNCP_TLB_RESP resp;
        resp.ioSpace = v.ioSpace;
        resp.pageFault = v.pageFault;
        resp.pa = paFromPage(v.page, offset);

        debugLog.record($format("D Resp: pa=0x%x, io=%0d, fault=%0d", resp.pa, resp.ioSpace, resp.pageFault));
        stdioDResp.printf(msgIRsp, list3(zeroExtend(resp.pa),
                                         zeroExtend(pack(resp.ioSpace)),
                                         zeroExtend(pack(resp.pageFault))));

        link_funcp_dtlb_trans.makeResp(resp);
    endrule


    //
    // optTransReqNoMsg --
    //     An optimization for the case that both an ITLB and a DTLB no-message
    //     request are available.  In this case the state doesn't change.
    //     Just drop both.
    //
    //     This rule should be given higher static priority than
    //     itlbTransReqNoMsg and dtlbTransReqNoMsg.
    //
    (* descending_urgency = "optTransReqNoMsg, itlbTransReqNoMsg" *)
    (* descending_urgency = "optTransReqNoMsg, dtlbTransReqNoMsg" *)
    rule optTransReqNoMsg (! isValid(link_funcp_itlb_trans.getReq) &&
                           ! isValid(link_funcp_dtlb_trans.getReq));
        link_funcp_itlb_trans.deq();
        link_funcp_dtlb_trans.deq();
    endrule


    //
    // receiveCacheResponse --
    //     Forward cache response to the reorder buffer.  The TLB requires
    //     in-order responses from the cache.
    //
    rule receiveCacheResponse (True);
        let cache_resp <- cache.readResp();

        // If allocOnFault is set the request came from the allocation path.
        // There is no response for allocation.  All other requests get
        // a response.
        if (! cache_resp.readMeta.allocOnFault)
        begin
            if (cache_resp.readMeta.isInstrReq)
            begin
                itlbTransRespQ.setValue(cache_resp.readMeta.refIdx, cache_resp.val);
            end
            else
            begin
                dtlbTransRespQ.setValue(cache_resp.readMeta.refIdx, cache_resp.val);
            end
        end

    endrule


    //
    // consumeInvalACK --
    //     We don't care about the ACK for invalReq.  Just consume them.
    //
    rule consumeInvalACK (True);
        cache.invalOrFlushWait();
    endrule
endmodule



// ===================================================================
//
// System interface (connection between central cache and software)
//
// ===================================================================

//
// mkVtoPInterface --
//   The interface between the main shared translation cache and the hybrid
//   memory service.
//
module [HASIM_MODULE] mkVtoPInterface#(ClientStub_FUNCP_TLB clientTranslationStub,
                                       DEBUG_FILE debugLog)
    // interface:
    (CENTRAL_CACHE_CLIENT_BACKING#(FUNCP_TLB_IDX, FUNCP_TLB_ENTRY, t_READ_META))
    provisos (Bits#(t_READ_META, t_READ_META_SZ));

    STAT statTLBCentralMiss <-
        mkStatCounter(statName("FUNCP_TLB_CENTRAL_CACHE_MISS",
                               "FUNCP TLB: Central cache misses"));

    Reg#(CENTRAL_CACHE_WORD_IDX) rdWordIdx <- mkReg(0);
    FIFO#(Bool) writeAck <- mkFIFO();


    method Action readLineReq(FUNCP_TLB_IDX idx,
                              t_READ_META readMeta,
                              RL_CACHE_GLOBAL_READ_META globalReadMeta);
        FUNCP_TLB_REFINFO refInfo = unpack(truncateNP(pack(readMeta)));
        ISA_ADDRESS va = vaFromPage(idx.vp, 0);

        // RRR doesn't support single bit arguments and we know the low bit of
        // the VA is 0.  Pass allocOnFault in bit 0.
        va[0] = pack(refInfo.allocOnFault);

        debugLog.record($format("RRR Req: ctx=%0d, va=0x%x, reqWord=%0d", idx.context_id, va, refInfo.allocWordIdx));
        statTLBCentralMiss.incr();

        // Request translation from software server
        clientTranslationStub.makeRequest_VtoP(contextIdToRRR(idx.context_id),
                                               va,
                                               zeroExtend(refInfo.allocWordIdx));
    endmethod

    method ActionValue#(Tuple2#(FUNCP_TLB_ENTRY, Bool)) readResp();
        // Pick a word from the current incoming value.  Pop the entry if on
        // the last word.
        OUT_TYPE_VtoP r;
        if (rdWordIdx == maxBound)
            r <- clientTranslationStub.getResponse_VtoP();
        else
            r = clientTranslationStub.peekResponse_VtoP();

        // RRR types aren't flexible enough to define an array of compile-time
        // size.  For now this code requires 4 words per line.
        Vector#(CENTRAL_CACHE_WORDS_PER_LINE, FUNCP_PADDR_RRR) translations;
        translations[0] = r.pa0;
        translations[1] = r.pa1;
        translations[2] = r.pa2;
        translations[3] = r.pa3;

        let pa = translations[rdWordIdx];
        rdWordIdx <= rdWordIdx + 1;

        // RRR doesn't support single bit arguments.  We would otherwise ignore
        // the low bits of the PA, since they are an index into the page.
        // Use the low two bits for flags.

        FUNCP_TLB_ENTRY entry;
        entry.ioSpace = unpack(pa[1]);
        entry.pageFault = unpack(pa[0]);
        entry.page = pageFromPA(truncate(pa));

        debugLog.record($format("RRR Resp: pa=0x%x, fault=%0d, io=%0d",
                                paFromPage(entry.page, 0), entry.pageFault, entry.ioSpace));

        return tuple2(entry, True);
    endmethod


    //
    // No writes can happen
    //

    method Action writeLineReq(FUNCP_TLB_IDX idx,
                               Vector#(CENTRAL_CACHE_WORDS_PER_LINE, Bool) wordValidMask,
                               Bool sendAck);
        if (sendAck)
            writeAck.enq(?);
    endmethod

    method Action writeData(FUNCP_TLB_ENTRY val);
        noAction;
    endmethod

    method Action writeAckWait();
        writeAck.deq();
    endmethod
endmodule



// ===================================================================
//
// Statistics
//
// ===================================================================

//
// mkTLBCacheStats --
//   Statistics for the main, shared, translation cache.
//
module [HASIM_MODULE] mkTLBCacheStats#(RL_CACHE_STATS#(t_READ_META) stats)
    // interface:
    ();
    
    // ***** Statistics *****

    STAT_ID statIDs[2] = {
        statName("FUNCP_TLB_PVT_CACHE_HIT",
                 "FUNCP TLB: Private cache hits"),
        statName("FUNCP_TLB_PVT_CACHE_MISS",
                 "FUNCP TLB: Private cache misses")
    };

    STAT_VECTOR#(2) sv <- mkStatCounter_Vector(statIDs);

    rule readHit (stats.readHit() matches tagged Valid .readMeta);
        sv.incr(0);
    endrule

    rule readMiss (stats.readMiss()  matches tagged Valid .readMeta);
        sv.incr(1);
    endrule

endmodule


//
// mkTLBPrefetchStats --
//   Statistics for the main, shared, translation cache's prefetcher.
//
module [HASIM_MODULE] mkTLBPrefetchStats#(NumTypeParam#(n_LEARNERS) dummy, RL_PREFETCH_STATS stats)
    // interface:
    ()
    provisos( NumAlias#(TMul#(n_LEARNERS, 4), n_STATS),
              Add#(TMax#(TLog#(n_STATS),1), extraBits, TLog#(`STATS_MAX_VECTOR_LEN)));
    
    STAT_ID prefetchStatIDs[9];
//  STAT_ID learnerStatIDs[ valueOf(n_STATS) ];

    prefetchStatIDs[0] = statName("FUNCP_TLB_PVT_PREFETCH_HIT", 
                                  "FUNCP TLB: Prefetch hits");
    prefetchStatIDs[1] = statName("FUNCP_TLB_PVT_PREFETCH_DROP_BUSY", 
                                  "FUNCP TLB: Prefetch reqs dropped by busy");
    prefetchStatIDs[2] = statName("FUNCP_TLB_PVT_PREFETCH_DROP_HIT", 
                                  "FUNCP TLB: Prefetch reqs dropped by hit");
    prefetchStatIDs[3] = statName("FUNCP_TLB_PVT_PREFETCH_LATE", 
                                  "FUNCP TLB: Late prefetch reqs");
    prefetchStatIDs[4] = statName("FUNCP_TLB_PVT_PREFETCH_USELESS", 
                                  "FUNCP TLB: Useless prefetch reqs");
    prefetchStatIDs[5] = statName("FUNCP_TLB_PVT_PREFETCH_ISSUE", 
                                  "FUNCP TLB: Prefetch reqs issued");
    prefetchStatIDs[6] = statName("FUNCP_TLB_PVT_PREFETCH_LEARN", 
                                  "FUNCP TLB: Prefetcher learns");
    prefetchStatIDs[7] = statName("FUNCP_TLB_PVT_PREFETCH_CONFLICT", 
                                  "FUNCP TLB: Prefetch learner conflicts");
    prefetchStatIDs[8] = statName("FUNCP_TLB_PVT_PREFETCH_ILLEGAL", 
                                  "FUNCP TLB: Uncacheable prefetch reqs");
    
//  for (Integer i = 0; i < valueOf(n_LEARNERS); i = i+1)
//  begin
//      learnerStatIDs[0+4*i] = statName("FUNCP_TLB_PVT_PREFETCH_L"+integerToString(i)+"_HIT",
//                                       "FUNCP TLB: Prefetch learner "+integerToString(i)+" hits");
//      learnerStatIDs[1+4*i] = statName("FUNCP_TLB_PVT_PREFETCH_L"+integerToString(i)+"_ISSUE", 
//                                       "FUNCP TLB: Prefetch reqs from learner "+integerToString(i));
//      learnerStatIDs[2+4*i] = statName("FUNCP_TLB_PVT_PREFETCH_L"+integerToString(i)+"_STRIDE", 
//                                       "FUNCP TLB: Prefetch stride from learner "+integerToString(i));
//      learnerStatIDs[3+4*i] = statName("FUNCP_TLB_PVT_PREFETCH_L"+integerToString(i)+"_LA_DIST", 
//                                       "FUNCP TLB: Prefetch lookahead dist from learner "+integerToString(i));
//  end
    
    STAT_VECTOR#(9)       prefetchSv <- mkStatCounter_Vector(prefetchStatIDs);
//  STAT_VECTOR#(n_STATS) learnerSv  <- mkStatCounter_Vector(learnerStatIDs);

    Reg#(Bool) prefetchHitR              <- mkReg(False);
    Reg#(Bool) prefetchDroppedByBusyR    <- mkReg(False);
    Reg#(Bool) prefetchDroppedByHitR     <- mkReg(False);
    Reg#(Bool) prefetchLateR             <- mkReg(False);
    Reg#(Bool) prefetchUselessR          <- mkReg(False);
    Reg#(Bool) prefetchIssuedR           <- mkReg(False);
    Reg#(Bool) prefetchLearnR            <- mkReg(False);
    Reg#(Bool) prefetchLearnerConflictR  <- mkReg(False);
    Reg#(Bool) prefetchIllegalReqR       <- mkReg(False);
//  Reg#(Maybe#(PREFETCH_LEARNER_STATS)) hitLearnerInfoR <- mkReg(tagged Invalid);
    
    rule addPipeline (True);
        prefetchHitR             <= stats.prefetchHit();
        prefetchDroppedByHitR    <= stats.prefetchDroppedByHit();
        prefetchDroppedByBusyR   <= stats.prefetchDroppedByBusy();
        prefetchLateR            <= stats.prefetchLate();
        prefetchUselessR         <= stats.prefetchUseless();
        prefetchIssuedR          <= stats.prefetchIssued();
        prefetchLearnR           <= stats.prefetchLearn();
        prefetchLearnerConflictR <= stats.prefetchLearnerConflict();
        prefetchIllegalReqR      <= stats.prefetchIllegalReq();
//      hitLearnerInfoR        <= stats.hitLearnerInfo;
    endrule

    rule prefetchHit (prefetchHitR);
        prefetchSv.incr(0);
    endrule

    rule prefetchDroppedByBusy (prefetchDroppedByBusyR);
        prefetchSv.incr(1);
    endrule

    rule prefetchDroppedByHit (prefetchDroppedByHitR);
        prefetchSv.incr(2);
    endrule

    rule prefetchLate (prefetchLateR);
        prefetchSv.incr(3);
    endrule

    rule prefetchUseless (prefetchUselessR);
        prefetchSv.incr(4);
    endrule
    
    rule prefetchIssued (prefetchIssuedR);
        prefetchSv.incr(5);
    endrule
    
    rule prefetchLearn (prefetchLearnR);
        prefetchSv.incr(6);
    endrule
    
    rule prefetchLearnerConflict (prefetchLearnerConflictR);
        prefetchSv.incr(7);
    endrule

    rule prefetchIllegalReq (prefetchIllegalReqR);
        prefetchSv.incr(8);
    endrule

//  rule hitLearnerUpdate (hitLearnerInfoR matches tagged Valid .s);
//      learnerSv.incr(0+resize(s.idx)*4); //hitLeanerIdx
//      if (s.isActive)                    //the learner is issuing prefetch request
//      begin
//          learnerSv.incr(1+resize(s.idx)*4);                         //activeLearnerIdx
//          learnerSv.incrBy(2+resize(s.idx)*4, signExtend(s.stride)); //activeLearnerStride
//          learnerSv.incrBy(3+resize(s.idx)*4, zeroExtend(s.laDist)); //activeLearnerLaDist
//      end
//  endrule

endmodule

module [HASIM_MODULE] mkNullTLBPrefetchStats#(NumTypeParam#(n_LEARNERS) dummy, RL_PREFETCH_STATS stats)
    // interface:
    ();
endmodule
