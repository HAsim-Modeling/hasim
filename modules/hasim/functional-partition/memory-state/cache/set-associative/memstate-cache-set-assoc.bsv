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

// Project foundation imports.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_cache.bsh"

// The memory virtual device

`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/dict/PARAMS_FUNCP_MEMSTATE_CACHE.bsh"
`include "asim/dict/ASSERTIONS_FUNCP_MEMSTATE_CACHE.bsh"
`include "asim/dict/STATS_FUNCP_MEMSTATE_CACHE.bsh"


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
    interface HASIM_CACHE_SOURCE_DATA#(FUNCP_MEM_CACHE_TAG, FUNCP_MEM_CACHELINE) cacheIfc;

    method Action readWordReq(MEM_ADDRESS addr);
    method ActionValue#(MEM_VALUE) readWordResp();

    method Action writeWord(MEM_ADDRESS addr, MEM_VALUE val);

endinterface: FUNCP_MEM_INTERFACE


//
// FUNCP_MEM_CACHE_LOAD_INFO --
//     Queue describing load path for cache read request or L1 cache hit.
//     If l1Value is valid then load is serviced by an L1 hit and the main
//     cache is bypassed.  If the main cache services the request then return
//     the portion of the line indexed by offset.
//
typedef struct
{
    Maybe#(MEM_VALUE) l1Value;
    Bool iStream;
    FUNCP_MEM_CACHE_TAG tag;
    FUNCP_MEM_CACHELINE_OFFSET offset;
}
FUNCP_MEM_CACHE_LOAD_INFO
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

        method Action readReq(FUNCP_MEM_CACHE_TAG addr);
            link_funcp_memory.makeReq(tagged MEM_LOAD_CACHELINE memAddrFromCacheTag(addr));
        endmethod

        method ActionValue#(FUNCP_MEM_CACHELINE) readResp() if (link_funcp_memory.getResp() matches tagged MEM_REPLY_LOAD_CACHELINE .v);
            link_funcp_memory.deq();
            return pack(v);
        endmethod
    
        // Asynchronous write (no response)
        method Action write(FUNCP_MEM_CACHE_TAG addr, FUNCP_MEM_CACHELINE val);
            link_funcp_memory.makeReq(tagged MEM_STORE_CACHELINE MEM_STORE_CACHELINE_INFO { addr: memAddrFromCacheTag(addr), val: unpack(val) });
        endmethod
    
        // Synchronous write.  writeSyncWait() blocks until the response arrives.
        method Action writeSyncReq(FUNCP_MEM_CACHE_TAG addr, FUNCP_MEM_CACHELINE val);
            link_funcp_memory.makeReq(tagged MEM_STORE_CACHELINE_SYNC MEM_STORE_CACHELINE_INFO { addr: memAddrFromCacheTag(addr), val: unpack(val) });
        endmethod

        method Action writeSyncWait() if (link_funcp_memory.getResp() matches tagged MEM_REPLY_STORE_CACHELINE_ACK .v);
            link_funcp_memory.deq();
        endmethod

    endinterface: cacheIfc

    //
    // This is the private interface used when bypassing the cache
    //
    method Action readWordReq(MEM_ADDRESS addr);
        link_funcp_memory.makeReq(funcpMemLoadReq(addr, False));
    endmethod

    method ActionValue#(MEM_VALUE) readWordResp() if (link_funcp_memory.getResp() matches tagged MEM_REPLY_LOAD .v);
        link_funcp_memory.deq();
        return v;
    endmethod

    method Action writeWord(MEM_ADDRESS addr, MEM_VALUE val);
        link_funcp_memory.makeReq(funcpMemStoreReq(addr, val));
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
    Connection_Server#(MEM_REQUEST, MEM_VALUE) link_memstate <- mkConnection_Server("mem_cache");

    Connection_Server#(MEM_INVAL_CACHELINE_INFO, Bool) link_funcp_memory_inval <- mkConnection_Server("funcp_memory_cache_invalidate");
    Connection_Server#(Bool, Bool)                 link_funcp_memory_inval_all <- mkConnection_Server("funcp_memory_cache_invalidate_all");


    // ***** Statistics *****
    Stat statLoadL1DHit <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_LOAD_HIT_D_L1);
    Stat statLoadL1IHit <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_LOAD_HIT_I_L1);

    // Interfaces required by the base cache module
    FUNCP_MEM_INTERFACE funcpMemIfc <- mkFuncpMemInterface();
    HASIM_CACHE_STATS statIfc <- mkFuncpMemoryCacheStats();

    // The cache
    HASIM_CACHE#(FUNCP_MEM_CACHE_TAG,
                 FUNCP_MEM_CACHELINE,
                 `FUNCP_MEMCACHE_SETS,
                 `FUNCP_MEMCACHE_WAYS,
                 FUNCP_MEM_ADDR_NONTAG_BITS) cache <- mkCacheSetAssoc(funcpMemIfc.cacheIfc, statIfc, debugLog);

    // Single entry L1 caches
    HASIM_TINY_CACHE#(FUNCP_MEM_CACHE_TAG, FUNCP_MEM_CACHELINE, 1) cacheL1D <- mkTinyCache1();
    HASIM_TINY_CACHE#(FUNCP_MEM_CACHE_TAG, FUNCP_MEM_CACHELINE, 1) cacheL1I <- mkTinyCache1();


    FIFO#(FUNCP_MEM_CACHE_LOAD_INFO) loadFromCacheQ <- mkFIFO();

    // Loop state for invalidating multiple lines with one message from the host
    Reg#(UInt#(8)) invalLoopNLines <- mkReg(0);
    Reg#(FUNCP_MEM_CACHE_TAG) invalLoopCacheTag <- mkRegU();
    Reg#(Bool) invalLoopOnlyFlush <- mkRegU();

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


    function FUNCP_MEM_CACHE_LOAD_INFO funcpMemCacheLoadInfo(Maybe#(MEM_VALUE) mv, Bool iStream, MEM_ADDRESS addr);

        return FUNCP_MEM_CACHE_LOAD_INFO { l1Value: mv,
                                           iStream: iStream,
                                           tag: cacheTagFromAddr(addr),
                                           offset: cacheLineOffsetFromAddr(addr) };

    endfunction


    // ***** Cache client-side rules ***** //

    //
    // handleReq --
    //    Incoming client request
    //
    rule handleReq (enableCache);
        let mem_req = link_memstate.getReq();
        link_memstate.deq();
        
        // The value of writeBackCache must be stable for an entire run since
        // setting write-back to false does not flush existing dirty lines.
        cache.setModeWriteBack(writeBackCache);

        case (mem_req) matches
            tagged MEM_LOAD .ld:
            begin
                let tag = cacheTagFromAddr(ld.addr);

                let l1_d <- cacheL1D.read(tag);
                let l1_i <- cacheL1I.read(tag);

                if (l1_d matches tagged Valid .v)
                begin
                    // L1 D cache hit
                    debugLog.record($format("LOAD: addr=0x%x, L1 D Hit", ld.addr));
                    statLoadL1DHit.incr();

                    FUNCP_MEM_CACHELINE_VEC val_vec = unpack(v);
                    loadFromCacheQ.enq(funcpMemCacheLoadInfo(tagged Valid val_vec[cacheLineOffsetFromAddr(ld.addr)], ld.iStream, ld.addr));
                end
                else if (l1_i matches tagged Valid .v)
                begin
                    // L1 I cache hit
                    debugLog.record($format("LOAD: addr=0x%x, L1 I Hit", ld.addr));
                    statLoadL1IHit.incr();

                    FUNCP_MEM_CACHELINE_VEC val_vec = unpack(v);
                    loadFromCacheQ.enq(funcpMemCacheLoadInfo(tagged Valid val_vec[cacheLineOffsetFromAddr(ld.addr)], ld.iStream, ld.addr));
                end
                else
                begin
                    // Look in main cache
                    debugLog.record($format("LOAD: addr=0x%x", ld.addr));
                    cache.readReq(tag);
                    loadFromCacheQ.enq(funcpMemCacheLoadInfo(tagged Invalid, ld.iStream, ld.addr));
                end
            end

            tagged MEM_STORE .s:
            begin
                debugLog.record($format("STORE: addr=0x%x, data=0x%x", s.addr, s.val));

                //
                // Build a mask so only the target word is updated within
                // the line.
                //
                MEM_VALUE ones = signExtend(1'b1);

                FUNCP_MEM_CACHELINE_VEC mask = unpack(0);
                FUNCP_MEM_CACHELINE_VEC val = unpack(0);
                
                let word = cacheLineOffsetFromAddr(s.addr);
                mask[word] = -1;
                val[word] = s.val;

                let tag = cacheTagFromAddr(s.addr);
                cache.writeMaskedReq(tag, pack(val), pack(mask));
                
                // Invalidate address in L1 caches
                cacheL1D.inval(tag);
                cacheL1I.inval(tag);

                // Write-through now?
                if (! writeBackCache)
                    funcpMemIfc.writeWord(s.addr, s.val);
            end

            tagged MEM_INVALIDATE_CACHELINE .a:
            begin
                debugLog.record($format("INVAL: addr=0x%x", a));
                let tag = cacheTagFromAddr(a);
                cache.invalReq(tag, False);

                // Invalidate address in L1 caches
                cacheL1D.inval(tag);
                cacheL1I.inval(tag);
            end

            tagged MEM_FLUSH_CACHELINE .a:
            begin
                debugLog.record($format("FLUSH: addr=0x%x", a));
                cache.flushReq(cacheTagFromAddr(a), False);
            end

            default: assertValidRequest(False);
        endcase
    endrule


    //
    // handleRespL1 --
    //     Response from read request initiated by handleReq that hit in L1 cache
    //
    rule handleRespL1 (enableCache &&& loadFromCacheQ.first().l1Value matches tagged Valid .val);
        let load_info = loadFromCacheQ.first();
        loadFromCacheQ.deq();

        link_memstate.makeResp(val);

        debugLog.record($format("  LOAD L1 done: idx=%d, data=0x%x", load_info.offset, val));
    endrule
    

    //
    // handleResp --
    //     Response from read request initiated by handleReq from main cache
    //
    rule handleResp (enableCache &&& loadFromCacheQ.first().l1Value matches tagged Invalid);
        let r <- cache.readResp();
        let load_info = loadFromCacheQ.first();
        loadFromCacheQ.deq();

        FUNCP_MEM_CACHELINE_VEC v = unpack(r);
        link_memstate.makeResp(v[load_info.offset]);

        // Store load in L1 cache
        if (load_info.iStream)
            cacheL1I.write(load_info.tag, r);
        else
            cacheL1D.write(load_info.tag, r);

        debugLog.record($format("  LOAD done: idx=%d, data=0x%x", load_info.offset, v[load_info.offset]));
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
                debugLog.record($format("LOAD: addr=0x%x", ld.addr));
                funcpMemIfc.readWordReq(ld.addr);
            end

            tagged MEM_STORE .s:
            begin
                debugLog.record($format("STORE: addr=0x%x, data=0x%x", s.addr, s.val));
                funcpMemIfc.writeWord(s.addr, s.val);
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
        debugLog.record($format("  LOAD done: data=0x%x", v));
    endrule


    // ***** Cache system-side rules ***** //

    //
    // handleInvalReq --
    //    Incoming invalidate line request
    //
    rule handleInvalReq (invalLoopNLines == 0);
        let line_info = link_funcp_memory_inval.getReq();
        link_funcp_memory_inval.deq();

        invalLoopNLines <= line_info.nLines;
        invalLoopCacheTag <= cacheTagFromAddr(line_info.addr);
        invalLoopOnlyFlush <= line_info.onlyFlush;

        debugLog.record($format("%s: addr=0x%x, nLines=%d", (line_info.onlyFlush ? "FLUSH" : "INVAL"), line_info.addr, line_info.nLines));
    endrule

    (* descending_urgency = "doInvals, handleResp" *)
    rule doInvals (invalLoopNLines != 0);
        //
        // Invalidate request specifies address, number of lines and whether
        // to invalidate or just flush dirty lines.  Loop here over the
        // number of requested lines.
        //

        if (invalLoopOnlyFlush)
        begin
            cache.flushReq(invalLoopCacheTag, invalLoopNLines == 1);
        end
        else
        begin
            cache.invalReq(invalLoopCacheTag, invalLoopNLines == 1);

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
    rule handleInvalResp(True);
        cache.invalOrFlushWait();
        link_funcp_memory_inval.makeResp(?);
        debugLog.record($format("  FLUSH/INVAL done"));
    endrule

    //
    // handleInvalAllReq --
    //    Incoming invalidate line request
    //
    (* descending_urgency = "handleInvalAllReq, handleReq" *)
    rule handleInvalAllReq(True);
        link_funcp_memory_inval_all.deq();
        cache.invalAllReq();

        // Invalidate address in L1 caches
        cacheL1D.invalAll();
        cacheL1I.invalAll();

        debugLog.record($format("INVAL all"));
    endrule

    //
    // handleInvalAllResp --
    //     Send a message back when inval is done
    //
    rule handleInvalAllResp(True);
        cache.invalAllWait();
        link_funcp_memory_inval_all.makeResp(?);
        debugLog.record($format("  INVAL all done"));
    endrule

endmodule: mkFUNCP_Cache
