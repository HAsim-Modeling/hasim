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
// Set-associative write back cache
//

//
// Run-time parameters controlling behavior:
//
// FUNCP_MEM_CACHE_ENABLE:
//   0: Cache disabled.  All requests bypass cache and go directly to memory.
//   1: Cache enabled.
//
// FUNCP_MEM_CACHE_WRITE_BACK:
//   0: Write back.  Stores flushed on victimization.
//   1: Write through.  Stores write to memory directly and cache dirty bits
//      are never set.
//
//

// Library imports.

import FIFO::*;
import Vector::*;
import RWire::*;
import FShow::*;

// Project foundation imports.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_cache.bsh"

// The memory virtual device

`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_regstate_base_types.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/dict/PARAMS_FUNCP_MEMSTATE_CACHE.bsh"
`include "asim/dict/ASSERTIONS_FUNCP_MEMSTATE_CACHE.bsh"
`include "asim/dict/STATS_FUNCP_MEMSTATE_CACHE.bsh"


typedef TExp#(`FUNCP_MEMCACHE_SET_INDEX_BITS) FUNCP_MEMCACHE_SETS;

// Low address bits indexing base ISA data size
typedef TLog#(TDiv#(`FUNCP_ISA_INT_REG_SIZE,8)) FUNCP_MEM_ISA_WORD_OFFSET_BITS;
typedef Bit#(FUNCP_MEM_ISA_WORD_OFFSET_BITS)    FUNCP_MEM_ISA_WORD_OFFSET;

// Address bits indexing ISA data sized objects in a cache line
typedef TLog#(CACHELINE_WORDS)                  FUNCP_MEM_CACHELINE_OFFSET_BITS;
typedef Bit#(FUNCP_MEM_CACHELINE_OFFSET_BITS)   FUNCP_MEM_CACHELINE_OFFSET;

// All non-cache tag bits
typedef TAdd#(FUNCP_MEM_ISA_WORD_OFFSET_BITS, FUNCP_MEM_CACHELINE_OFFSET_BITS) FUNCP_MEM_ADDR_NONTAG_BITS;

// Address bits for cache tag (excludes ISA_WORD_OFFSET and CACHELINE_OFFSET)
typedef Bit#(TSub#(`FUNCP_ISA_P_ADDR_SIZE,TLog#(TDiv#(`FUNCP_CACHELINE_BITS,8)))) FUNCP_MEM_CACHE_TAG;

// Cache line data size
typedef Bit#(`FUNCP_CACHELINE_BITS) FUNCP_MEM_CACHELINE;

// Equivalent to cache line size but as a vector of ISA-sized objects
typedef Vector#(CACHELINE_WORDS, MEM_VALUE) FUNCP_MEM_CACHELINE_VEC;


// ===================================================================
//
// FUNCP_MEM_INTERFACE
//
//    Interface for talking to main memory.  In addition to the
//    HASIM_CACHE_SORUCE_DATA subinterface, FUNCP_MEM_INTERFACE has bypass
//    methods for reading and writing word-sized data from/to main memory.
//    The bypass methods are used when the cache is disabled.
//
// ===================================================================

interface FUNCP_MEM_INTERFACE;
    interface HASIM_CACHE_SOURCE_DATA#(FUNCP_MEM_CACHE_TAG, FUNCP_MEM_CACHELINE, FUNCP_MEM_CACHE_REF_INFO) cacheIfc;

    method Action readWordReq(MEM_LOAD_INFO ldInfo);
    method ActionValue#(MEMSTATE_RESP) readWordResp();

    method Action writeWord(CONTEXT_ID ctxId, MEM_ADDRESS addr, MEM_VALUE val);

endinterface: FUNCP_MEM_INTERFACE


//
// FUNCP_MEM_CACHE_REQ
//     Intermediate state in the local cache request queue.
//
typedef union tagged
{
    MEM_LOAD_INFO   MEM_LOAD;
    MEM_STORE_INFO  MEM_STORE;
    MEM_INVAL_INFO  MEM_INVALIDATE_CACHELINE;
    MEM_INVAL_INFO  MEM_FLUSH_CACHELINE;
}
FUNCP_MEM_CACHE_REQ
    deriving (Eq, Bits);


//
// Reference handle passed to cache read request and returned with the data.
// Associating this data with the request makes handling OOO responses from
// the cache easy.
//
typedef struct
{
    CONTEXT_ID contextId;
    FUNCP_MEM_CACHELINE_OFFSET offset;
    Bool iStream;
    FUNCP_MEMREF_TOKEN memRefToken;
}
FUNCP_MEM_CACHE_REF_INFO
    deriving (Eq, Bits);



module [HASIM_MODULE] mkFuncpMemInterface
    // interface:
        (FUNCP_MEM_INTERFACE);

    // Connection to main memory
    Connection_Client#(MEM_REQUEST, MEM_REPLY) link_funcp_memory <- mkConnection_Client("funcp_memory");

    function MEM_ADDRESS memAddrFromCacheTag(FUNCP_MEM_CACHE_TAG tag);
        FUNCP_MEM_CACHELINE_OFFSET cloff = 0;
        FUNCP_MEM_ISA_WORD_OFFSET woff = 0;
        return { tag, cloff, woff };
    endfunction

    //
    // This is the standard interface passed to the cache.  All the methods
    // operate on cache-line sized data.  They also translate cache addresses
    // that lack the low bits for addressing within a line to system addresses
    // that include the low bits.
    //
    interface HASIM_CACHE_SOURCE_DATA cacheIfc;

        method Action readReq(FUNCP_MEM_CACHE_TAG addr, FUNCP_MEM_CACHE_REF_INFO refInfo);
            link_funcp_memory.makeReq(funcpMemLoadCacheLineReq(refInfo.contextId, memAddrFromCacheTag(addr)));
        endmethod

        method ActionValue#(FUNCP_MEM_CACHELINE) readResp() if (link_funcp_memory.getResp() matches tagged MEM_REPLY_LOAD_CACHELINE .v);
            link_funcp_memory.deq();
            return pack(v);
        endmethod
    
        // Asynchronous write (no response)
        method Action write(FUNCP_MEM_CACHE_TAG addr, FUNCP_MEM_CACHELINE val, FUNCP_MEM_CACHE_REF_INFO refInfo);
            link_funcp_memory.makeReq(tagged MEM_STORE_CACHELINE MEM_STORE_CACHELINE_INFO { contextId: refInfo.contextId, addr: memAddrFromCacheTag(addr), val: unpack(val) });
        endmethod
    
        // Synchronous write.  writeSyncWait() blocks until the response arrives.
        method Action writeSyncReq(FUNCP_MEM_CACHE_TAG addr, FUNCP_MEM_CACHELINE val, FUNCP_MEM_CACHE_REF_INFO refInfo);
            link_funcp_memory.makeReq(tagged MEM_STORE_CACHELINE_SYNC MEM_STORE_CACHELINE_INFO { contextId: refInfo.contextId, addr: memAddrFromCacheTag(addr), val: unpack(val) });
        endmethod

        method Action writeSyncWait() if (link_funcp_memory.getResp() matches tagged MEM_REPLY_STORE_CACHELINE_ACK .v);
            link_funcp_memory.deq();
        endmethod

    endinterface: cacheIfc

    //
    // This is the private interface used when bypassing the cache
    //
    method Action readWordReq(MEM_LOAD_INFO ldInfo);
        link_funcp_memory.makeReq(funcpMemLoadReq(ldInfo.contextId,
                                                  ldInfo.addr,
                                                  ldInfo.iStream,
                                                  ldInfo.memRefToken));
    endmethod

    method ActionValue#(MEMSTATE_RESP) readWordResp() if (link_funcp_memory.getResp() matches tagged MEM_REPLY_LOAD .v);
        link_funcp_memory.deq();
        return v;
    endmethod

    method Action writeWord(CONTEXT_ID ctxId, MEM_ADDRESS addr, MEM_VALUE val);
        link_funcp_memory.makeReq(funcpMemStoreReq(ctxId, addr, val));
    endmethod

endmodule


// ===================================================================
//
// STATISTICS INTERFACE
//
// mkFuncpMemoryCacheStats --
//     Statistics callbacks from main cache class.
//
// ===================================================================

module [HASIM_MODULE] mkFuncpMemoryCacheStats
    // interface:
        (HASIM_CACHE_STATS);
    
    Stat statLoadHit   <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_LOAD_HIT);
    Stat statLoadMiss  <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_LOAD_MISS);

    Stat statStoreHit  <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_STORE_HIT);
    Stat statStoreMiss <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_STORE_MISS);

    Stat statInvalLine
                       <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_INVAL_LINE);
    Stat statDirtyLineFlush
                       <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_DIRTY_LINE_FLUSH);
    Stat statForceInvalLine
                       <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_FORCE_INVAL_LINE);

    method Action readHit();
        statLoadHit.incr();
    endmethod

    method Action readMiss();
        statLoadMiss.incr();
    endmethod

    method Action writeHit();
        statStoreHit.incr();
    endmethod

    method Action writeMiss();
        statStoreMiss.incr();
    endmethod

    method Action invalLine();
        statInvalLine.incr();
    endmethod

    method Action dirtyLineFlush();
        statDirtyLineFlush.incr();
    endmethod

    method Action forceInvalLine();
        statForceInvalLine.incr();
    endmethod

endmodule



// ===================================================================
//
// L1 cache.  One line per context.
//
// ===================================================================

interface FUNCP_MEM_L1_MULTICTX;
    // Two levels of Maybe#() in the read result to support the reserve method.
    method ActionValue#(Maybe#(Maybe#(FUNCP_MEM_CACHELINE))) read(CONTEXT_ID ctxId, FUNCP_MEM_CACHE_TAG tag);

    // Hold a line with the tag in preparation for filling it with a read from
    // the main cache.  If a store comes along later while the read is still in
    // flight the reservation will be broken to avoid caching stale data.
    method Action reserve(CONTEXT_ID ctxId, FUNCP_MEM_CACHE_TAG tag);

    // Update a line if the tag matches.
    method Action update(CONTEXT_ID ctxId, FUNCP_MEM_CACHE_TAG tag, FUNCP_MEM_CACHELINE val);

    // Invalidate caches matching tag in all contexts.
    method Action inval(FUNCP_MEM_CACHE_TAG tag);

    method Action invalAll();
endinterface: FUNCP_MEM_L1_MULTICTX


module [HASIM_MODULE] mkFUNCP_L1Cache_MultiCtx#(DEBUG_FILE debugLog)
    // interface:
    (FUNCP_MEM_L1_MULTICTX);
    
    //
    // Allocate a single cache line entry for each context.
    //
    Vector#(NUM_CONTEXTS,
            HASIM_TINY_CACHE#(FUNCP_MEM_CACHE_TAG,
                              Maybe#(FUNCP_MEM_CACHELINE),
                              1,
                              FUNCP_MEM_ADDR_NONTAG_BITS)) l1cache = newVector();

    for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
    begin
        l1cache[c] <- mkTinyCache1(debugLog);
    end


    //
    // Methods
    //

    method ActionValue#(Maybe#(Maybe#(FUNCP_MEM_CACHELINE))) read(CONTEXT_ID ctxId, FUNCP_MEM_CACHE_TAG tag);
        let r <- l1cache[ctxId].read(tag);
        return r;
    endmethod

    method Action reserve(CONTEXT_ID ctxId, FUNCP_MEM_CACHE_TAG tag);
        l1cache[ctxId].write(tag, tagged Invalid);
    endmethod

    method Action update(CONTEXT_ID ctxId, FUNCP_MEM_CACHE_TAG tag, FUNCP_MEM_CACHELINE val);
        l1cache[ctxId].update(tag, tagged Valid val);
    endmethod

    method Action inval(FUNCP_MEM_CACHE_TAG tag);
        for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
        begin
            l1cache[c].inval(tag);
        end
    endmethod

    method Action invalAll();
        for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
        begin
            l1cache[c].invalAll();
        end
    endmethod

endmodule


// ===================================================================
//
// MAIN CACHE MODULE with soft connections to other functional
// components.
//
// ===================================================================

module [HASIM_MODULE] mkFUNCP_Cache
    // interface:
        ();

    DEBUG_FILE debugLog <- mkDebugFile(`FUNCP_MEMCACHE_LOGFILE_NAME);

    // ***** Dynamic parameters *****
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();

    Param#(1) enableCacheParam <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_CACHE_FUNCP_MEMCACHE_ENABLE, paramNode);
    function Bool enableCache() = (enableCacheParam == 1);

    Param#(1) writeBackParam <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_CACHE_FUNCP_MEMCACHE_WRITE_BACK, paramNode);
    function Bool writeBackCache() = (writeBackParam == 1);


    // ***** Soft Connections *****
    Connection_Server#(MEM_REQUEST, MEMSTATE_RESP) link_memstate <- mkConnection_Server("mem_cache");

    Connection_Server#(MEM_INVAL_FUNCP_CACHE_SERVICE_INFO, Bool) link_funcp_memory_inval <- mkConnection_Server("funcp_memory_cache_invalidate");
    Connection_Server#(CONTEXT_ID, Bool) link_funcp_memory_inval_all <- mkConnection_Server("funcp_memory_cache_invalidate_all");


    // ***** Statistics *****
    Stat statLoadL1DHit <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_LOAD_HIT_D_L1);
    Stat statLoadL1IHit <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_LOAD_HIT_I_L1);

    // Interfaces required by the base cache module
    FUNCP_MEM_INTERFACE funcpMemIfc <- mkFuncpMemInterface();
    HASIM_CACHE_STATS statIfc <- mkFuncpMemoryCacheStats();

    // The cache
    HASIM_CACHE#(FUNCP_MEM_CACHE_TAG,      // Cache address type
                 FUNCP_MEM_CACHELINE,      // Cache line
                 MEM_VALUE,                // Cache word
                 CACHELINE_WORDS,          // Words per cache line
                 FUNCP_MEM_CACHE_REF_INFO, // Reference meta-data (passed to RRR)
                 FUNCP_MEMCACHE_SETS,      // Sets in the cache
                 `FUNCP_MEMCACHE_WAYS,     // Ways per set
                 FUNCP_MEM_ADDR_NONTAG_BITS) cache <- mkCacheSetAssoc(funcpMemIfc.cacheIfc, statIfc, True, debugLog);

    // Single entry L1 caches
    FUNCP_MEM_L1_MULTICTX cacheL1D <- mkFUNCP_L1Cache_MultiCtx(debugLog);
    FUNCP_MEM_L1_MULTICTX cacheL1I <- mkFUNCP_L1Cache_MultiCtx(debugLog);

    FIFO#(FUNCP_MEM_CACHE_REQ) handleReqQ <- mkFIFO();
    FIFO#(Tuple2#(MEM_LOAD_INFO, MEM_VALUE)) loadFromL1CacheQ <- mkFIFO();

    // Loop state for invalidating multiple lines with one message from the host
    Reg#(UInt#(8)) invalLoopNLines <- mkReg(0);
    Reg#(FUNCP_MEM_CACHE_TAG) invalLoopCacheTag <- mkRegU();
    Reg#(Bool) invalLoopOnlyFlush <- mkRegU();
    Reg#(CONTEXT_ID) invalLoopContextId <- mkRegU();


    // ***** Assertion Checkers *****

    ASSERTION_NODE assertNode    <- mkAssertionNode(`ASSERTIONS_FUNCP_MEMSTATE_CACHE__BASE);
    ASSERTION assertValidRequest <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_CACHE_INVALID_REQUEST, ASSERT_ERROR, assertNode);


    function FUNCP_MEM_CACHE_TAG cacheTagFromAddr(MEM_ADDRESS addr);

        Tuple3#(FUNCP_MEM_CACHE_TAG, FUNCP_MEM_CACHELINE_OFFSET, FUNCP_MEM_ISA_WORD_OFFSET) tup = unpack(addr);
        match { .tag, .cloff, .woff } = tup;
        return tag;

    endfunction


    function FUNCP_MEM_CACHELINE_OFFSET cacheLineOffsetFromAddr(MEM_ADDRESS addr);

        Tuple3#(FUNCP_MEM_CACHE_TAG, FUNCP_MEM_CACHELINE_OFFSET, FUNCP_MEM_ISA_WORD_OFFSET) tup = unpack(addr);
        match { .tag, .cloff, .woff } = tup;
        return cloff;

    endfunction


    function FUNCP_MEM_CACHE_REF_INFO initCacheRefInfo(CONTEXT_ID ctxId,
                                                       MEM_ADDRESS addr,
                                                       Bool iStream,
                                                       FUNCP_MEMREF_TOKEN memRefToken);
        FUNCP_MEM_CACHE_REF_INFO r;
        r.contextId = ctxId;
        r.offset = cacheLineOffsetFromAddr(addr);
        r.iStream = iStream;
        r.memRefToken = memRefToken;
        return r;

    endfunction


    // ***** Cache client-side rules ***** //

    //
    // handleReq --
    //    Incoming client request.  Only start a new request if no invalidate
    //    is in pending.
    //
    (* conservative_implicit_conditions *)
    rule handleReq_LOAD (enableCache && (invalLoopNLines == 0) &&&
                         ! link_funcp_memory_inval_all.reqNotEmpty() &&&
                         link_memstate.getReq() matches tagged MEM_LOAD .ld);
        link_memstate.deq();
        
        debugLog.record($format("LOAD: ctx=%d, ", ld.contextId) + fshow(ld.memRefToken) + $format(", addr=0x%x", ld.addr));

        let tag = cacheTagFromAddr(ld.addr);

        let l1_d <- cacheL1D.read(ld.contextId, tag);
        let l1_i <- cacheL1I.read(ld.contextId, tag);

        if (l1_d matches tagged Valid .l1_d_line &&&
            l1_d_line matches tagged Valid .v)
        begin
            // L1 D cache hit
            debugLog.record($format("  LOAD: addr=0x%x, L1 D Hit", ld.addr));
            statLoadL1DHit.incr();

            FUNCP_MEM_CACHELINE_VEC val_vec = unpack(v);
            loadFromL1CacheQ.enq(tuple2(ld, val_vec[cacheLineOffsetFromAddr(ld.addr)]));
        end
        else if (l1_i matches tagged Valid .l1_i_line &&&
                 l1_i_line matches tagged Valid .v)
        begin
            // L1 I cache hit
            debugLog.record($format("  LOAD: addr=0x%x, L1 I Hit", ld.addr));
            statLoadL1IHit.incr();

            FUNCP_MEM_CACHELINE_VEC val_vec = unpack(v);
            loadFromL1CacheQ.enq(tuple2(ld, val_vec[cacheLineOffsetFromAddr(ld.addr)]));
        end
        else
        begin
            // Look in main cache.
            handleReqQ.enq(tagged MEM_LOAD ld);

            //
            // Store an entry with the tag for this line in the L1 cache.
            // If the tag is still there when the load data is available 
            // then we know it is safe to store the value in the L1 cache.
            //
            if (ld.iStream)
                cacheL1I.reserve(ld.contextId, tag);
            else
                cacheL1D.reserve(ld.contextId, tag);
        end
    endrule


    rule handleReq_STORE (enableCache && (invalLoopNLines == 0) &&&
                          ! link_funcp_memory_inval_all.reqNotEmpty() &&&
                          link_memstate.getReq() matches tagged MEM_STORE .s);
        link_memstate.deq();

        // The value of writeBackCache must be stable for an entire run since
        // setting write-back to false does not flush existing dirty lines.
        cache.setModeWriteBack(writeBackCache);

        debugLog.record($format("STORE: ctx=%0d, addr=0x%x, data=0x%x", s.contextId, s.addr, s.val));

        handleReqQ.enq(tagged MEM_STORE s);
                
        // Invalidate address in L1 caches
        let tag = cacheTagFromAddr(s.addr);
        cacheL1D.inval(tag);
        cacheL1I.inval(tag);

        // Write-through now?
        if (! writeBackCache)
            funcpMemIfc.writeWord(s.contextId, s.addr, s.val);
    endrule


    rule handleReq_INVAL (enableCache && (invalLoopNLines == 0) &&&
                          ! link_funcp_memory_inval_all.reqNotEmpty() &&&
                          link_memstate.getReq() matches tagged MEM_INVALIDATE_CACHELINE .inval);
        link_memstate.deq();

        debugLog.record($format("INVAL: ctx=%0d, addr=0x%x", inval.contextId, inval.addr));
        handleReqQ.enq(tagged MEM_INVALIDATE_CACHELINE inval);

        // Invalidate address in L1 caches
        let tag = cacheTagFromAddr(inval.addr);
        cacheL1D.inval(tag);
        cacheL1I.inval(tag);
    endrule


    rule handleReq_FLUSH (enableCache && (invalLoopNLines == 0) &&&
                         ! link_funcp_memory_inval_all.reqNotEmpty() &&&
                         link_memstate.getReq() matches tagged MEM_FLUSH_CACHELINE .flush);
        link_memstate.deq();

        debugLog.record($format("FLUSH: ctx=%0d, addr=0x%x", flush.contextId, flush.addr));
        handleReqQ.enq(tagged MEM_FLUSH_CACHELINE flush);
    endrule


    //
    // handleReqCache --
    //     This stage is here for timing since L1 lookup and starting the L2
    //     cache request can't fit in a cycle.
    //
    (* conservative_implicit_conditions *)
    rule handleReqCache (enableCache);
        let req = handleReqQ.first();
        handleReqQ.deq();
        
        case (req) matches
            tagged MEM_LOAD .ld:
            begin
                cache.readReq(cacheTagFromAddr(ld.addr),
                              initCacheRefInfo(ld.contextId, ld.addr, ld.iStream, ld.memRefToken));
            end

            tagged MEM_STORE .st:
            begin
                let tag = cacheTagFromAddr(st.addr);
                let word = cacheLineOffsetFromAddr(st.addr);

                cache.write(tag, st.val, word,
                            initCacheRefInfo(st.contextId, st.addr, ?, ?));
            end

            tagged MEM_INVALIDATE_CACHELINE .inval:
            begin
                cache.invalReq(cacheTagFromAddr(inval.addr), False,
                               initCacheRefInfo(inval.contextId, inval.addr, ?, ?));
            end

            tagged MEM_FLUSH_CACHELINE .flush:
            begin
                cache.flushReq(cacheTagFromAddr(flush.addr), False,
                               initCacheRefInfo(flush.contextId, flush.addr, ?, ?));
            end
        endcase
    endrule


    //
    // handleRespL1 --
    //     Response from read request initiated by handleReq that hit in L1 cache
    //
    rule handleRespL1 (enableCache);
        match { .load_info, .val } = loadFromL1CacheQ.first();
        loadFromL1CacheQ.deq();

        link_memstate.makeResp(memStateResp(load_info.memRefToken, val));

        let offset = cacheLineOffsetFromAddr(load_info.addr);
        debugLog.record($format("  LOAD L1 done: idx=%d, data=0x%x", offset, val));
    endrule
    

    //
    // handleResp --
    //     Response from read request initiated by handleReq from main cache
    //
    rule handleResp (enableCache);
        let r <- cache.readResp();

        let load_info = r.refInfo;
        let tag = r.addr;
        let offset = load_info.offset;

        FUNCP_MEM_CACHELINE_VEC v = unpack(r.value);
        link_memstate.makeResp(memStateResp(load_info.memRefToken, v[offset]));

        // Store load in L1 cache
        if (load_info.iStream)
            cacheL1I.update(load_info.contextId, tag, r.value);
        else
            cacheL1D.update(load_info.contextId, tag, r.value);

        debugLog.record($format("  LOAD done: ") + fshow(load_info.memRefToken) + $format(", idx=%d, data=0x%x", offset, v[offset]));
    endrule
    

    //
    // handleBypassReq --
    //     Handle incoming requests when the cache is bypassed
    //
    rule handleBypassReq (! enableCache);
        let mem_req = link_memstate.getReq();
        link_memstate.deq();
        
        case (mem_req) matches
            tagged MEM_LOAD .ld:
            begin
                debugLog.record($format("LOAD: ctx=%d, ", ld.contextId) + fshow(ld.memRefToken) + $format(", addr=0x%x", ld.addr));
                funcpMemIfc.readWordReq(ld);
            end

            tagged MEM_STORE .s:
            begin
                debugLog.record($format("STORE: ctx=%d, addr=0x%x, data=0x%x", s.contextId, s.addr, s.val));
                funcpMemIfc.writeWord(s.contextId, s.addr, s.val);
            end

            tagged MEM_INVALIDATE_CACHELINE .a:
            begin
                noAction;
            end

            tagged MEM_FLUSH_CACHELINE .a:
            begin
                noAction;
            end

            default: assertValidRequest(False);
        endcase
    endrule


    //
    // handleBypassResp --
    //     Response from request initiated by handleBypassReq
    //
    rule handleBypassResp (! enableCache);
        let v <- funcpMemIfc.readWordResp();
        link_memstate.makeResp(v);
        debugLog.record($format("  LOAD done: ") + fshow(v.memRefToken) + $format(", data=0x%x", v));
    endrule


    // ***** Cache system-side rules ***** //

    //
    // handleInvalReq --
    //    Incoming invalidate line request.  Only process a request when
    //    not currently invalidating and when no global invalidation request
    //    is pending.
    //
    rule handleInvalReq ((invalLoopNLines == 0) &&
                         ! link_funcp_memory_inval_all.reqNotEmpty());
        let line_info = link_funcp_memory_inval.getReq();
        link_funcp_memory_inval.deq();

        invalLoopNLines <= line_info.nLines;
        invalLoopCacheTag <= cacheTagFromAddr(line_info.addr);
        invalLoopOnlyFlush <= line_info.onlyFlush;
        invalLoopContextId <= line_info.contextId;

        debugLog.record($format("%s: ctx=%d, addr=0x%x, nLines=%d", (line_info.onlyFlush ? "FLUSH" : "INVAL"), line_info.contextId, line_info.addr, line_info.nLines));
    endrule

    rule doInvals (invalLoopNLines != 0);
        //
        // Invalidate request specifies address, number of lines and whether
        // to invalidate or just flush dirty lines.  Loop here over the
        // number of requested lines.
        //

        if (invalLoopOnlyFlush)
        begin
            cache.flushReq(invalLoopCacheTag,
                           invalLoopNLines == 1,
                           initCacheRefInfo(invalLoopContextId, ?, ?, ?));
        end
        else
        begin
            cache.invalReq(invalLoopCacheTag,
                           invalLoopNLines == 1,
                           initCacheRefInfo(invalLoopContextId, ?, ?, ?));

            // Invalidate address in L1 caches
            cacheL1D.inval(invalLoopCacheTag);
            cacheL1I.inval(invalLoopCacheTag);
        end

        invalLoopCacheTag <= invalLoopCacheTag + 1;
        invalLoopNLines <= invalLoopNLines - 1;
    endrule

    //
    // handleInvalResp --
    //     Send a message back when inval is done
    //
    rule handleInvalResp (True);
        cache.invalOrFlushWait();
        link_funcp_memory_inval.makeResp(?);
        debugLog.record($format("  FLUSH/INVAL done"));
    endrule

    //
    // handleInvalAllReq --
    //    Incoming invalidate line request
    //
    (* descending_urgency = "handleInvalAllReq, doInvals, handleResp, handleRespL1, handleReqCache, handleReq_FLUSH, handleReq_INVAL, handleReq_STORE, handleReq_LOAD" *)
    rule handleInvalAllReq (invalLoopNLines == 0);
        let ctx_id = link_funcp_memory_inval_all.getReq();
        link_funcp_memory_inval_all.deq();

        cache.invalAllReq(initCacheRefInfo(ctx_id, ?, ?, ?));

        // Invalidate address in L1 caches
        cacheL1D.invalAll();
        cacheL1I.invalAll();

        debugLog.record($format("INVAL all"));
    endrule

    //
    // handleInvalAllResp --
    //     Send a message back when inval is done
    //
    rule handleInvalAllResp (True);
        cache.invalAllWait();
        link_funcp_memory_inval_all.makeResp(?);
        debugLog.record($format("  INVAL all done"));
    endrule

endmodule: mkFUNCP_Cache
