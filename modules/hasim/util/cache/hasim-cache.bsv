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
// Author: Michael Adler
//
// A generic cache class (n-way set associative) for caching data in BRAM.
// Classes building a cache must provide an interface class to the source
// data of type HASIM_CACHE_SOURCE_DATA (defined below).  The cache
// takes a number of parameters: the address and data types, the number of
// sets and the number of ways within each set.
//
// The cache may either be write-back (the default) or write-through.  For
// write through caches it is the callers responsibility to do the write
// to backing storage.  This cache class merely skips setting of the dirty
// bit on writes in write-through mode.
//

// Library imports.

import FIFO::*;
import FIFOF::*;
import Vector::*;

// Project foundation imports.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/fpga_components.bsh"


// ===================================================================
//
// PUBLIC DATA STRUCTURES
//
// ===================================================================

//
// Load response
//
typedef struct
{
    t_CACHE_DATA value;
    t_CACHE_ADDR addr;
    t_CACHE_REF_INFO refInfo;
}
HASIM_CACHE_LOAD_RESP#(type t_CACHE_ADDR, type t_CACHE_DATA, type t_CACHE_REF_INFO)
    deriving (Eq, Bits);


//
// HAsim cache interface.  nTagExtraLowBits is used just for debugging.
// This specified number of low bits are prepanded to cache tags so
// addresses match those seen in other modules.
//
// t_CACHE_REF_INFO is metadata associated with a reference.  Metadata is
// passed to the backing store for fills.  The metadata is not stored in
// the cache.
//
interface HASIM_CACHE#(type t_CACHE_ADDR,
                       type t_CACHE_DATA,
                       type t_CACHE_REF_INFO,
                       numeric type nSets,
                       numeric type nWays,
                       numeric type nTagExtraLowBits);

    // Read a full line.  Read from backing store if not already cached.
    method Action readReq(t_CACHE_ADDR addr, t_CACHE_REF_INFO refInfo);
    method ActionValue#(HASIM_CACHE_LOAD_RESP#(t_CACHE_ADDR, t_CACHE_DATA, t_CACHE_REF_INFO)) readResp();
    // Predicate to test whether a read response is ready this cycle.
    method Bool readRespReady();
    
    // Write a full line.  No response.
    method Action writeReq(t_CACHE_ADDR addr,
                           t_CACHE_DATA val,
                           t_CACHE_REF_INFO refInfo);
    
    // Write a partial line.  Writes only where mask is 1.  Read line from backing
    // store before write if not already cached.  No response.
    method Action writeMaskedReq(t_CACHE_ADDR addr,
                                 t_CACHE_DATA val,
                                 t_CACHE_DATA mask,
                                 t_CACHE_REF_INFO refInfo);

    // Invalidate & flush requests.  Both write dirty lines back.  Invalidate drops
    // the line from the cache.  Flush keeps the line in the cache.  A response
    // is returned for invalOrFlushWait iff sendAck is true.
    method Action invalReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo);
    method Action flushReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo);
    method Action invalOrFlushWait();

    // Invalidate entire cache.
    method Action invalAllReq(t_CACHE_REF_INFO refInfo);
    method Action invalAllWait();
    
    // Write back or write through cache?  Default is write back.
    // NOTE:  Turning off writeback does NOT cause the cache to write through
    //        as stores arrive.  All it does is keep the dirty bit from being
    //        set on writes.  If a caller turns off write back it becomes the
    //        responsibility of the caller to write data to the storage device
    //        outside of this interface.
    method Action setModeWriteBack(Bool isWriteBack);

endinterface: HASIM_CACHE


//
// The caller must provide an instance of the HASIM_CACHE_SOURCE_DATA interface
// so the cache can read and write data from the next level in the hierarchy.
//
// See HASIM_CACHE interface for description of refInfo.
//
interface HASIM_CACHE_SOURCE_DATA#(type t_CACHE_ADDR,
                                   type t_CACHE_DATA,
                                   type t_CACHE_REF_INFO);

    // Read request and response with data
    method Action readReq(t_CACHE_ADDR addr, t_CACHE_REF_INFO refInfo);
    method ActionValue#(t_CACHE_DATA) readResp();
    
    // Asynchronous write (no response)
    method Action write(t_CACHE_ADDR addr,
                        t_CACHE_DATA val,
                        t_CACHE_REF_INFO refInfo);
    
    // Synchronous write.  writeSyncWait() blocks until the response arrives.
    method Action writeSyncReq(t_CACHE_ADDR addr,
                               t_CACHE_DATA val,
                               t_CACHE_REF_INFO refInfo);
    method Action writeSyncWait();

endinterface: HASIM_CACHE_SOURCE_DATA


//
// The caller must provide an instance of the HASIM_CACHE_STATS interface to
// the cache code.  This allows each instance of the cache to have its own
// statistics.  mkNullHAsimCacheStats is provided in this package for callers
// not interested in statistics.
//
interface HASIM_CACHE_STATS;
    method Action readHit();
    method Action readMiss();
    method Action writeHit();
    method Action writeMiss();
    method Action invalLine();             // Invalidate due to capacity
    method Action dirtyLineFlush();
    method Action forceInvalLine();        // Invalidate forced by external request
endinterface: HASIM_CACHE_STATS


// ===================================================================
//
// PRIVATE DATA STRUCTURES
//
// ===================================================================

//
// States in the cache manager FSM
//
typedef enum
{
    HCST_IDLE,

    // Initial processing of new request.  Two stages
    HCST_START1_REQ,
    HCST_START2_REQ,

    // Waiting for read from cache BRAM to service a READ request
    HCST_CACHE_READ_PENDING,

    // Waiting for read for partial line write
    HCST_CACHE_RMW_PENDING,

    // Read or write missed
    HCST_MISS,

    // Waiting for read from cache BRAM.  Line will be flushed to backing store.
    HCST_FLUSH_DIRTY,

    // Data for a read is coming from backing store
    HCST_FILL_FOR_READ_PENDING,

    // Data for a partial write is coming from backing store
    HCST_FILL_FOR_RMW_PENDING,

    // Waiting for backing store response for a flush for an invalidate/flush request.
    HCST_FLUSH_SYNC_PENDING,
 
    // Invalidating all sets
    HCST_INVAL_ALL,
    HCST_INVAL_ALL_DONE
}
HASIM_CACHE_STATE
    deriving (Eq, Bits);

//
// Data for a write to the cache.
//
typedef struct
{
    t_CACHE_DATA val;
    t_CACHE_DATA mask;
    Bool isMasked;
}
HASIM_CACHE_WRITE_INFO#(type t_CACHE_ADDR, type t_CACHE_DATA)
    deriving (Eq, Bits);

//
// Cache request type
// 
typedef enum
{
    HCOP_READ,
    HCOP_WRITE,
    HCOP_INVAL,
    HCOP_FLUSH_DIRTY
}
HASIM_CACHE_OPERATION
    deriving (Eq, Bits);

typedef UInt#(TLog#(nSets)) HASIM_CACHE_SET_IDX#(numeric type nSets);
typedef UInt#(TLog#(nWays)) HASIM_CACHE_WAY_IDX#(numeric type nWays);


//
// Cache metadata (tag and a dirty bit).  The tag is the full address.
// It is the responsibility of the package using this cache to drop insignificant
// low bits from the address size before addresses reach here.
//
typedef struct
{
    t_CACHE_ADDR addr;
    Bool dirty;
}
HASIM_CACHE_METADATA#(type t_CACHE_ADDR)
    deriving(Bits, Eq);

//
// The cache data is indexed by the set and the way within the set.
// Declaring the cache data as multiply indexed vectors results in a large
// amount of extra LUT usage to control the BRAMs.  Instead, we allocate a
// single large cache data BRAM and index it with a packed version of this
// structure:
//
typedef struct
{
    t_CACHE_SET_IDX set;
    HASIM_CACHE_WAY_IDX#(nWays) way;
}
HASIM_CACHE_DATA_IDX#(numeric type nWays, type t_CACHE_SET_IDX)
    deriving(Bits, Eq);


typedef 2 STORE_DATA_HEAP_IDX_SZ;


//
// Set associative cache
//

module mkCacheSetAssoc#(HASIM_CACHE_SOURCE_DATA#(Bit#(t_CACHE_ADDR_SZ), t_CACHE_DATA, t_CACHE_REF_INFO) sourceData,
                        HASIM_CACHE_STATS stats,
                        DEBUG_FILE debugLog)
    // interface:
        (HASIM_CACHE#(Bit#(t_CACHE_ADDR_SZ), t_CACHE_DATA, t_CACHE_REF_INFO, nSets, nWays, nTagExtraLowBits))
    provisos (Bits#(t_CACHE_DATA, t_CACHE_DATA_SZ),
              Bits#(t_CACHE_REF_INFO, t_CACHE_REF_INFO_SZ),

              // The interface allows for numbers of sets and ways that aren't
              // powers of 2, but the implementation currently does not.  Enforce
              // powers of 2 here.
              Add#(nSets, 0, TExp#(TLog#(nSets))),
              Add#(nWays, 0, TExp#(TLog#(nWays))),

              // Cache address size must be no larger than 64 bits
              Add#(t_CACHE_ADDR_SZ, a__, 64),

              Alias#(HASIM_CACHE_SET_IDX#(nSets), t_CACHE_SET_IDX),
              Bits#(t_CACHE_SET_IDX, t_CACHE_SET_IDX_SZ),

              // Set size must be no longer than 32 bits (for set filter)
              Add#(t_CACHE_SET_IDX_SZ, b__, 32),

              // Tag size is anything left over after the set index size
              Add#(t_CACHE_SET_IDX_SZ, t_CACHE_TAG_SZ, t_CACHE_ADDR_SZ),

              Alias#(Bit#(t_CACHE_ADDR_SZ), t_CACHE_ADDR),
              Alias#(HASIM_CACHE_WAY_IDX#(nWays), t_CACHE_WAY_IDX),
              Alias#(HASIM_CACHE_WRITE_INFO#(t_CACHE_ADDR, t_CACHE_DATA), t_CACHE_WRITE_INFO),
              Alias#(Vector#(nWays, HASIM_CACHE_WAY_IDX#(nWays)), t_LRU_LIST),
              Alias#(HASIM_CACHE_DATA_IDX#(nWays, t_CACHE_SET_IDX), t_CACHE_DATA_IDX),
              Alias#(HASIM_CACHE_METADATA#(t_CACHE_ADDR), t_METADATA),
              Alias#(HASIM_CACHE_LOAD_RESP#(t_CACHE_ADDR, t_CACHE_DATA, t_CACHE_REF_INFO), t_CACHE_LOAD_RESP),
              Alias#(Vector#(nWays, Maybe#(HASIM_CACHE_METADATA#(t_CACHE_ADDR))), t_METADATA_VECTOR),
       
              // Unbelievably ugly tautologies required by the compiler:
              Add#(t_CACHE_ADDR_SZ, nTagExtraLowBits, TAdd#(t_CACHE_ADDR_SZ, nTagExtraLowBits)),
              Log#(nWays, TLog#(nWays)),
              Add#(TLog#(TExp#(TLog#(nSets))), 0, TLog#(nSets)),
              Add#(TLog#(TDiv#(TExp#(TLog#(nSets)), 2)), x__, TLog#(nSets)));


    // ***** Cache data *****

    // Tags & dirty bits
    BRAM#(t_CACHE_SET_IDX, t_METADATA_VECTOR) cacheMeta <- mkBRAMInitialized(Vector::replicate(tagged Invalid));
    // Values
    BRAM#(t_CACHE_DATA_IDX, t_CACHE_DATA) cacheData <- mkBRAM();
    // LRU hint
    BRAM#(t_CACHE_SET_IDX, t_LRU_LIST) cacheLRU <- mkBRAMInitialized(Vector::genWith(fromInteger));

    // ***** Internal state *****

    Reg#(HASIM_CACHE_STATE) curState <- mkReg(HCST_IDLE);

    Reg#(Bool) cacheIsEmpty <- mkReg(True);

    // Store data is kept in a heap to avoid passing it around through FIFOs.
    // The heap size limits the number of stores in flight.  Store data is large,
    // so don't allocate too many.
    MEMORY_HEAP_IMM#(Bit#(STORE_DATA_HEAP_IDX_SZ), t_CACHE_WRITE_INFO) reqInfo_writeData <- mkMemoryHeapUnionLUTRAM();

    //
    // These registers hold the state for the current request
    //
    Reg#(HASIM_CACHE_OPERATION) reqInfo_oper <- mkRegU();
    Reg#(t_CACHE_ADDR)          reqInfo_addr <- mkRegU();
    Reg#(t_CACHE_SET_IDX)       reqInfo_set <- mkRegU();
    Reg#(Bool)                  reqInfo_ack <- mkRegU();    // Inval request sends response?
    Reg#(t_CACHE_REF_INFO)      reqInfo_refInfo <- mkRegU();

    // Pointer to value in store data heap (writes only)
    Reg#(Bit#(STORE_DATA_HEAP_IDX_SZ)) reqInfo_wInfoPtr <- mkRegU();

    // Meta data becomes part of the state at the start of read and invalidate processing.
    Reg#(t_METADATA_VECTOR)     reqInfo_metaData <- mkRegU();

    // The way becomes part of the state for the request, computed either
    // as the way for a hit or a victim.
    Reg#(t_CACHE_WAY_IDX)       reqInfo_way <- mkRegU();

    // The flush address is either set as a request comes in for flush/inval or
    // when a line must be evicted in the middle of processing a request.
    Reg#(t_CACHE_ADDR)          reqInfo_flushAddr <- mkRegU();

    // Is the cache write back?  If not, never set a dirty bit.  It is then the
    // responsibility of the caller to write values to backing storage.
    Reg#(Bool) writeBackCache <- mkReg(True);

    // Filter for allowing one live operation per cache set.
    COUNTING_FILTER#(t_CACHE_SET_IDX) setFilter <- mkCountingFilter(debugLog);


    // ***** Queues between internal pipeline stages *****

    // Response to client, returned 
    FIFOF#(t_CACHE_LOAD_RESP) respToClientQ <- mkFIFOF();
    FIFO#(Bool) invalReqDoneQ <- mkFIFO1();

    FIFO#(Tuple2#(t_LRU_LIST, t_METADATA_VECTOR)) missQ <- mkFIFO();

    // ***** Indexing functions *****

    function t_CACHE_DATA_IDX getDataIdx (t_CACHE_SET_IDX set, t_CACHE_WAY_IDX way);

        t_CACHE_DATA_IDX idx;
        idx.set = set;
        idx.way = way;
        return idx;

    endfunction


    function t_CACHE_SET_IDX cacheSet(t_CACHE_ADDR addr);
        return unpack(truncate(hashBits(addr)));
    endfunction

    //
    // debugAddr --
    //     Pretty printer for converting cache addresses to system addresses.
    //     Adds trailing 0's that were dropped from cache addresses because they
    //     are inside a cache line.
    //
    function Bit#(TAdd#(t_CACHE_ADDR_SZ, nTagExtraLowBits)) debugAddr(t_CACHE_ADDR addr);
        
        Bit#(nTagExtraLowBits) zero = 0;
        return { addr, zero };

    endfunction

    // ***** Meta data searches *****

    function t_METADATA metaData(t_CACHE_ADDR addr, Bool dirty);
    
        t_METADATA meta;
        meta.addr = addr;
        meta.dirty = dirty;
    
        return meta;
    
    endfunction


    function Maybe#(t_CACHE_WAY_IDX) findWayMatch(t_CACHE_ADDR addr, t_METADATA_VECTOR meta);

        Vector#(nWays, Bool) way_match = replicate(False);

        for (Integer w = 0; w < valueOf(nWays); w = w + 1)
        begin
            way_match[w] = case (meta[w]) matches
                               tagged Valid .m: (m.addr == addr);
                               default: False;
                           endcase;
        end

        return findElem(True, way_match);

    endfunction

    function Bool isInvalid(Maybe#(t) m) = ! isValid(m);

    function Maybe#(t_CACHE_WAY_IDX) findFirstInvalid(t_METADATA_VECTOR meta);

        return findIndex(isInvalid, meta);

    endfunction


    // ***** LRU Management ***** //

    //
    // getLRU --
    //   Least recently used way in a set.
    //
    function t_CACHE_WAY_IDX getLRU(t_LRU_LIST list);

        return list[valueOf(nWays) - 1];

    endfunction


    //
    // getMRU --
    //   Most recently used way in a set.
    //

    function t_CACHE_WAY_IDX getMRU(t_LRU_LIST list);

        return list[0];

    endfunction


    //
    // pushMRU --
    //   Update MRU list, moving a way to the head of the list.
    //
    function t_LRU_LIST pushMRU(t_LRU_LIST curLRU, t_CACHE_WAY_IDX mru);

        t_LRU_LIST new_list = curLRU;
    
        //
        // Find the new MRU value in the current list
        //
        if (findElem(mru, curLRU) matches tagged Valid .mru_pos)
        begin
            //
            // Shift older references out of the MRU slot
            //
            for (t_CACHE_WAY_IDX w = 0; w < mru_pos; w = w + 1)
            begin
                new_list[w + 1] = curLRU[w];
            end

            // MRU is slot 0
            new_list[0] = mru;
        end

        return new_list;

    endfunction



    function Action cacheDebugLRUUpdate(t_CACHE_SET_IDX set,
                                        t_CACHE_WAY_IDX way,
                                        t_LRU_LIST cur_lru,
                                        t_LRU_LIST new_lru);
    action

        if ((getMRU(cur_lru) != way) || (cur_lru != new_lru))
        begin
            debugLog.record($format("    Update LRU (set=0x%x): %b -> %b", set, cur_lru, new_lru));
        end
        if (getMRU(new_lru) != way)
        begin
            debugLog.record($format("    ***ERROR*** expected MRU to be 0x%x but it is 0x%x", way, getMRU(new_lru)));
        end

    endaction
    endfunction


    // ***** Rules ***** //

    //
    // receiveMeta --
    //     Access requests start by receiving meta data for a line.  The meta
    //     data is large enough and the search for a way within the cache can
    //     be expensive with four ways.  Rules consuming the meta data wind
    //     up on the critical path.  Just receive the meta data in the
    //     first cycle.
    (* conservative_implicit_conditions *)
    rule receiveMeta (curState == HCST_START1_REQ);
        let set = reqInfo_set;
        let success <- setFilter.insert(set);

        if (success)
        begin
            let meta <- cacheMeta.readRsp();

            reqInfo_metaData <= meta;
            curState <= HCST_START2_REQ;
        end
    endrule


    //
    // handleInvalOrFlush --
    //     Invalidate and flush requests have similar handling.  Both write
    //     back a dirty matching line.  Flush preserves the now clean line
    //     in the cache.
    //
    (* conservative_implicit_conditions *)
    rule handleInvalOrFlush (curState == HCST_START2_REQ &&&
                             (reqInfo_oper == HCOP_INVAL) || (reqInfo_oper == HCOP_FLUSH_DIRTY));

        let cur_lru <- cacheLRU.readRsp();

        let meta = reqInfo_metaData;
        let addr = reqInfo_addr;
        let set = reqInfo_set;

        Bool found_dirty_line = False;

        if (findWayMatch(addr, meta) matches tagged Valid .way)
        begin
            if (meta[way] matches tagged Valid .m &&& m.dirty)
            begin
                // Found dirty line.  Prepare for write back.
                reqInfo_way <= way;
                cacheData.readReq(getDataIdx(set, way));
                found_dirty_line = True;

                if (reqInfo_oper == HCOP_FLUSH_DIRTY)
                begin
                    // Line no longer dirty.  Update meta data.
                    let new_meta = m;
                    new_meta.dirty = False;
                    meta[way] = tagged Valid new_meta;
                end
            end

            if (reqInfo_oper == HCOP_INVAL)
            begin
                // Invalidate line
                meta[way] = tagged Invalid;
                stats.forceInvalLine();
            end

            cacheMeta.write(set, meta);

            debugLog.record($format("  FLUSH/INVAL HIT %s: addr=0x%x, set=0x%x, way=0x%x", (found_dirty_line ? "dirty" : "clean"), debugAddr(addr), set, way));
        end
        
        if (found_dirty_line)
            curState <= HCST_FLUSH_DIRTY;
        else
        begin
            // Done with this request
            setFilter.remove(set);
            curState <= HCST_IDLE;
            if (reqInfo_ack)
                invalReqDoneQ.enq(?);
        end
    endrule


    //
    // handleReadOrWrite --
    //     Cache read or write request.
    //
    (* conservative_implicit_conditions *)
    rule handleReadOrWrite (curState == HCST_START2_REQ &&&
                            (reqInfo_oper == HCOP_READ) || (reqInfo_oper == HCOP_WRITE));

        let cur_lru <- cacheLRU.readRsp();

        let meta = reqInfo_metaData;
        let addr = reqInfo_addr;
        let set = reqInfo_set;

        cacheIsEmpty <= False;

        if (findWayMatch(addr, meta) matches tagged Valid .way)
        begin
            //
            // Hit!
            //
            reqInfo_way <= way;

            // Update LRU
            let new_lru = pushMRU(cur_lru, way);
            cacheLRU.write(set, new_lru);
            cacheDebugLRUUpdate(set, way, cur_lru, new_lru);

            let idx = getDataIdx(set, way);
            if (reqInfo_oper == HCOP_READ)
            begin
                // Read
                stats.readHit();
                cacheData.readReq(idx);
                curState <= HCST_CACHE_READ_PENDING;
            end
            else
            begin
                // Write
                stats.writeHit();

                // Mark line dirty
                let meta_upd = meta;
                meta_upd[way] = tagged Valid metaData(addr, writeBackCache);
                cacheMeta.write(set, meta_upd);

                let w_info = reqInfo_writeData.sub(reqInfo_wInfoPtr);

                if (w_info.isMasked)
                begin
                    // Partial write.  Start by reading current value.
                    cacheData.readReq(idx);
                    curState <= HCST_CACHE_RMW_PENDING;
                end
                else
                begin
                    // Full line write.  Now we're done.
                    debugLog.record($format("  Write HIT: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, w_info.val));
                    cacheData.write(idx, w_info.val);

                    reqInfo_writeData.free(reqInfo_wInfoPtr);
                    setFilter.remove(set);
                    curState <= HCST_IDLE;
                end
            end
        end
        else
        begin
            // Miss.
            missQ.enq(tuple2(cur_lru, meta));
            curState <= HCST_MISS;
        end
    endrule


    //
    // handleReadCacheHit --
    //   Forward data coming from cache BRAM from handleRead to back to the requester.
    //
    rule handleReadCacheHit (curState == HCST_CACHE_READ_PENDING);
        let v <- cacheData.readRsp();

        let addr = reqInfo_addr;
        let set = reqInfo_set;
        let way = reqInfo_way;

        t_CACHE_LOAD_RESP rsp;
        rsp.value = v;
        rsp.addr = addr;
        rsp.refInfo = reqInfo_refInfo;
        respToClientQ.enq(rsp);

        // Done with this read request
        setFilter.remove(set);
        curState <= HCST_IDLE;

        debugLog.record($format("  Read HIT: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, v));
    endrule


    //
    // handleRmwCacheHit --
    //   Partial line update.  Read the current value and update masked bits.
    //
    rule handleRmwCacheHit (curState == HCST_CACHE_RMW_PENDING);
        let v <- cacheData.readRsp();

        let addr = reqInfo_addr;
        let set = reqInfo_set;
        let way = reqInfo_way;

        debugLog.record($format("  Hit for WRITE: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, v));

        let w_info = reqInfo_writeData.sub(reqInfo_wInfoPtr);
        v = unpack((pack(v) & ~pack(w_info.mask)) |
                   (pack(w_info.val) & pack(w_info.mask)));

        cacheData.write(getDataIdx(set, way), v);

        // Done with this write request.
        reqInfo_writeData.free(reqInfo_wInfoPtr);
        setFilter.remove(set);
        curState <= HCST_IDLE;

        debugLog.record($format("  WRITE masked: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, v));
    endrule


    //
    // handleMiss --
    //     Miss handler for read and write requests.
    //
    (* conservative_implicit_conditions *)
    rule handleMiss (curState == HCST_MISS);
        match {.cur_lru, .meta} = missQ.first();
        missQ.deq();

        let addr = reqInfo_addr;
        let set = reqInfo_set;

        //
        // Miss.  First pick a victim.
        //
        debugLog.record($format("  %s MISS: addr=0x%x, set=0x%x", (reqInfo_oper == HCOP_READ ? "READ" : "WRITE"), debugAddr(addr), set));

        Bool is_read = (reqInfo_oper == HCOP_READ);

        //
        // Pick a fill victim:  either the first invalid or the LRU entry.
        // 
        t_CACHE_WAY_IDX fill_way = getLRU(cur_lru);
        if (findFirstInvalid(meta) matches tagged Valid .inval_way)
        begin
            fill_way = inval_way;
        end

        reqInfo_way <= fill_way;

        // Update LRU
        let new_lru = pushMRU(cur_lru, fill_way);
        cacheLRU.write(set, new_lru);
        cacheDebugLRUUpdate(set, fill_way, cur_lru, new_lru);

        // Update tag here for the filled line since we have the details
        let meta_upd = meta;
        meta_upd[fill_way] = tagged Valid metaData(addr, writeBackCache && ! is_read);
        cacheMeta.write(set, meta_upd);

        //
        // Now must figure out the next state...
        //

        let idx = getDataIdx(set, fill_way);

        // Dirty victim?  Flush.
        Bool flushed_dirty = False;
        if (meta[fill_way] matches tagged Valid .m)
        begin
            stats.invalLine();
            if (m.dirty)
            begin
                flushed_dirty = True;
                reqInfo_flushAddr <= m.addr;
                cacheData.readReq(idx);
            end
        end

        // Request the filled value from backing store unless this is a
        // write to the whole line.
        let w_info = reqInfo_writeData.sub(reqInfo_wInfoPtr);
        Bool need_backing_data = (is_read || w_info.isMasked);
        if (need_backing_data)
        begin
            sourceData.readReq(addr, reqInfo_refInfo);
        end

        if (is_read)
            stats.readMiss();
        else
            stats.writeMiss();

        if (flushed_dirty)
        begin
            //
            // Flushing an old line.  The HCST_FLUSH_DIRTY state handles
            // both read and write requests.
            //
            curState <= HCST_FLUSH_DIRTY;
        end
        else if (need_backing_data)
        begin
            if (is_read)
            begin
                curState <= HCST_FILL_FOR_READ_PENDING;
            end
            else
            begin
                curState <= HCST_FILL_FOR_RMW_PENDING;
            end
        end
        else
        begin
            // Writing the full line and no flush of an old line.  We're
            // ready now.
            debugLog.record($format("  Write to INVAL: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, fill_way, w_info.val));
            cacheData.write(idx, w_info.val);

            // Done with this write request.
            reqInfo_writeData.free(reqInfo_wInfoPtr);
            setFilter.remove(set);
            curState <= HCST_IDLE;
        end
    endrule


    //
    // handleFlushDirtyLine --
    //   Flush a dirty line and continue on to fill, if appropriate.
    //
    rule handleFlushDirtyLine (curState == HCST_FLUSH_DIRTY);
        let flushData <- cacheData.readRsp();

        let addr = reqInfo_flushAddr;
        let set = reqInfo_set;
        let way = reqInfo_way;

        stats.dirtyLineFlush();
        debugLog.record($format("  Write back DIRTY: addr=0x%x, set=0x%x, data=0x%x", debugAddr(addr), set, flushData));

        if (reqInfo_oper == HCOP_READ || reqInfo_oper == HCOP_WRITE)
        begin
            // Normal flush before a fill
            sourceData.write(addr, flushData, reqInfo_refInfo);

            // Pass the request on to the fill stage
            if (reqInfo_oper == HCOP_READ)
                curState <= HCST_FILL_FOR_READ_PENDING;
            else
                curState <= HCST_FILL_FOR_RMW_PENDING;
        end
        else
        begin
            // Flush for invalidate request.  Use sync method to know the
            // data arrived.
            sourceData.writeSyncReq(addr, flushData, reqInfo_refInfo);
            curState <= HCST_FLUSH_SYNC_PENDING;
        end
        
    endrule


    //
    // handleFillForRead --
    //    Update the cache with requested data coming back from memory.
    //
    rule handleFillForRead (curState == HCST_FILL_FOR_READ_PENDING);

        let v <- sourceData.readResp();

        let addr = reqInfo_addr;
        let set = reqInfo_set;
        let way = reqInfo_way;

        // Cache the value
        cacheData.write(getDataIdx(set, way), v);

        t_CACHE_LOAD_RESP rsp;
        rsp.value = v;
        rsp.addr = addr;
        rsp.refInfo = reqInfo_refInfo;
        respToClientQ.enq(rsp);

        debugLog.record($format("  Read FILL: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, v));

        setFilter.remove(set);
        curState <= HCST_IDLE;

    endrule
    

    //
    // handleFillForWrite --
    //    Update the cache with requested data coming back from memory merged
    //    with the write data.
    //
    rule handleFillForWrite (curState == HCST_FILL_FOR_RMW_PENDING);

        let v <- sourceData.readResp();

        let addr = reqInfo_addr;
        let set = reqInfo_set;
        let way = reqInfo_way;

        debugLog.record($format("  Fill for WRITE: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, v));

        // Merge with full line and update cache
        let w_info = reqInfo_writeData.sub(reqInfo_wInfoPtr);
        v = unpack((pack(v) & ~pack(w_info.mask)) |
                   (pack(w_info.val) & pack(w_info.mask)));
        cacheData.write(getDataIdx(set, way), v);

        debugLog.record($format("  WRITE masked: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, v));

        reqInfo_writeData.free(reqInfo_wInfoPtr);
        setFilter.remove(set);
        curState <= HCST_IDLE;

    endrule


    //
    // handleFlushACK --
    //   Wait for the response to a flush from back storage for synchronous
    //   flushes.
    //
    rule handleFlushACK (curState == HCST_FLUSH_SYNC_PENDING);

        sourceData.writeSyncWait();

        let set = reqInfo_set;

        // Done with this flush request.
        setFilter.remove(set);
        curState <= HCST_IDLE;

        if (reqInfo_ack)
            invalReqDoneQ.enq(?);

        debugLog.record($format("  FLUSH or INVAL done"));

    endrule


    // ====================================================================
    //
    //   Invalidate ALL support (clear entire cache).
    //
    // ====================================================================

    FIFO#(Tuple3#(t_CACHE_SET_IDX, t_METADATA_VECTOR, Bool)) invalAllFlushSetQ <- mkFIFO();
    FIFO#(Tuple2#(t_CACHE_ADDR, t_CACHE_SET_IDX)) invalAllFlushLineQ <- mkFIFO();

    // State for invalidate all
    Reg#(Bool)             invalidateAllReq <- mkReg(False);
    Reg#(Bool)             invalidatingAllLastSet  <- mkReg(False);
    Reg#(t_CACHE_SET_IDX)  invalidateAllSet <- mkReg(0);
    Reg#(t_CACHE_WAY_IDX)  invalidateFlushWay <- mkReg(0);

    Reg#(t_CACHE_REF_INFO) invalidateAll_refInfo <- mkRegU();

    //
    // invalAllWait --
    //     Wait for cache to be idle before starting an invalidate all.
    //
    rule handleInvalAllReq (invalidateAllReq && curState == HCST_IDLE);
        debugLog.record($format("  Start INVAL ALL"));

        invalidateAllReq <= False;

        if (cacheIsEmpty)
        begin
            curState <= HCST_INVAL_ALL_DONE;
        end
        else
        begin
            curState <= HCST_INVAL_ALL;
            cacheMeta.readReq(0);
            cacheIsEmpty <= True;
        end
    endrule


    //
    // handleInvalidateAll --
    //     Memory system may request invalidation of the entire cache if it
    //     doesn't know which lines may need to be flushed.
    //
    rule handleInvalidateAll ((curState == HCST_INVAL_ALL) && ! invalidatingAllLastSet);
        cacheLRU.write(invalidateAllSet, Vector::genWith(fromInteger));
        cacheMeta.write(invalidateAllSet, Vector::replicate(tagged Invalid));

        // Flush dirty lines
        let meta <- cacheMeta.readRsp();
        let done = (invalidateAllSet == maxBound);
        invalAllFlushSetQ.enq(tuple3(invalidateAllSet, meta, done));

        if (done)
        begin
            invalidatingAllLastSet <= True;
        end
        else
        begin
            cacheMeta.readReq(invalidateAllSet);
        end

        invalidateAllSet <= invalidateAllSet + 1;
    endrule


    // Not required for correctness: get rid of a couple warning messages...
    (* descending_urgency= "handleReadCacheHit, flushDirtyLineForInvalAll" *)
    (* descending_urgency= "handleRmwCacheHit, flushDirtyLineForInvalAll" *)
    (* descending_urgency= "handleFlushDirtyLine, flushDirtyLineForInvalAll" *)

    (* descending_urgency= "handleReadOrWrite, handleInvalidateAll" *)
    (* descending_urgency= "handleInvalOrFlush, handleInvalidateAll, invalSetForInvalAll" *)


    //
    // invalSetForInvalAll --
    //   Invalidate an entire set (requested by handleInvalidateAll).
    //
    (* conservative_implicit_conditions *)
    rule invalSetForInvalAll (curState == HCST_INVAL_ALL);
        match {.set, .meta, .last_set} = invalAllFlushSetQ.first();
        
        if (meta[invalidateFlushWay] matches tagged Valid .m &&& m.dirty)
        begin
            invalAllFlushLineQ.enq(tuple2(m.addr, set));
            cacheData.readReq(getDataIdx(set, invalidateFlushWay));
        end

        // Done with the set?
        if (invalidateFlushWay == maxBound)
        begin
            // Done.  Pass the request on to the fill stage, if appropriate.
            invalAllFlushSetQ.deq();
            
            // Done with invalidateingAll?
            if (last_set)
            begin
                curState <= HCST_INVAL_ALL_DONE;
                invalidatingAllLastSet <= False;
                debugLog.record($format("  Request done: INVAL ALL"));
            end
        end

        invalidateFlushWay <= invalidateFlushWay + 1;
    endrule


    //
    // flushDirtyLineForInvalAll --
    //     Flush one dirty line, requested by invalSetForInvalAll.
    //
    rule flushDirtyLineForInvalAll (True);
        match { .addr, .set } = invalAllFlushLineQ.first();
        invalAllFlushLineQ.deq();

        let flushData <- cacheData.readRsp();

        sourceData.write(addr, flushData, invalidateAll_refInfo);

        stats.dirtyLineFlush();
        debugLog.record($format("  Write back DIRTY: addr=0x%x, set=0x%x, data=0x%x", debugAddr(addr), set, flushData));
    endrule


    // ====================================================================
    //
    //   Incoming cache requests (methods)
    //
    // ====================================================================

    //
    // genRequest --
    //     This function is used by most of the request methods to generate
    //     the internal data structure for managing a request.  It also starts
    //     the first step:  reading metadata from BRAM.
    //
    function ActionValue#(t_CACHE_SET_IDX) genRequest(HASIM_CACHE_OPERATION oper,
                                                      t_CACHE_ADDR addr,
                                                      t_CACHE_REF_INFO refInfo);
    actionvalue

        curState <= HCST_START1_REQ;

        let set = cacheSet(addr);

        reqInfo_oper <= oper;
        reqInfo_addr <= addr;
        reqInfo_set <= set;

        reqInfo_refInfo <= refInfo;

        // This field is meaningful only for flush/inval requests but it is
        // harmless to set it unconditionally.  Capacity evictions will update
        // the field with the tag of the line being evicted.
        reqInfo_flushAddr <= addr;

        // Read meta data and LRU hints
        cacheMeta.readReq(set);
        cacheLRU.readReq(set);

        return set;

    endactionvalue
    endfunction


    //
    // readReq -- Read a full line.  Fetch from backing store if not in the cache.
    //
    method Action readReq(t_CACHE_ADDR addr, t_CACHE_REF_INFO refInfo) if (curState == HCST_IDLE);
        let set <- genRequest(HCOP_READ, addr, refInfo);
        debugLog.record($format("  New request: READ addr=0x%x, set=0x%x", debugAddr(addr), set));
    endmethod

    method ActionValue#(t_CACHE_LOAD_RESP) readResp();
        let r = respToClientQ.first();
        respToClientQ.deq();
        return r;
    endmethod

    method Bool readRespReady();
        return respToClientQ.notEmpty();
    endmethod

    //
    // writeReq -- Write a full line.
    //
    method Action writeReq(t_CACHE_ADDR addr, t_CACHE_DATA val, t_CACHE_REF_INFO refInfo) if (curState == HCST_IDLE);
        let set <- genRequest(HCOP_WRITE, addr, refInfo);

        t_CACHE_WRITE_INFO wInfo;
        wInfo.val = val;
        wInfo.mask = ?;
        wInfo.isMasked = False;

        let h <- reqInfo_writeData.malloc();
        reqInfo_writeData.upd(h, wInfo);
        reqInfo_wInfoPtr <= h;

        debugLog.record($format("  New request: WRITE addr=0x%x, set=0x%x, data=0x%x, wData heap=%0d", debugAddr(addr), set, val, h));
    endmethod


    //
    // writeMaskedReq --
    //     Write a partial line.  Fetch from backing store if not in the cache.
    //
    method Action writeMaskedReq(t_CACHE_ADDR addr, t_CACHE_DATA val, t_CACHE_DATA mask, t_CACHE_REF_INFO refInfo) if (curState == HCST_IDLE);
        let set <- genRequest(HCOP_WRITE, addr, refInfo);

        t_CACHE_WRITE_INFO wInfo;
        wInfo.val = val;
        wInfo.mask = mask;
        wInfo.isMasked = True;

        let h <- reqInfo_writeData.malloc();
        reqInfo_writeData.upd(h, wInfo);
        reqInfo_wInfoPtr <= h;

        debugLog.record($format("  New request: WRITE MASKED addr=0x%x, set=0x%x, data=0x%x, mask=0x%x, wData heap=%0d", debugAddr(addr), set, val, mask, h));
    endmethod
    

    //
    // invalReq -- Invalidate (remove) a line from the cache
    //
    method Action invalReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo) if (curState == HCST_IDLE);
        let set <- genRequest(HCOP_INVAL, addr, refInfo);
        reqInfo_ack <= sendAck;
        debugLog.record($format("  New request: INVAL addr=0x%x, set=0x%x", debugAddr(addr), set));
    endmethod
    

    //
    // flushReq --
    //     Flush (write back) a line from the cache but keep the line cached.
    //
    method Action flushReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo) if (curState == HCST_IDLE);
        let set <- genRequest(HCOP_FLUSH_DIRTY, addr, refInfo);
        reqInfo_ack <= sendAck;
        debugLog.record($format("  New request: FLUSH addr=0x%x, set=0x%x", debugAddr(addr), set));
    endmethod


    //
    // invalOrFlushWait -- Block until an inval or flush request completes.
    //
    method Action invalOrFlushWait();
        invalReqDoneQ.deq();
    endmethod


    //
    // invalAllReq -- Invalidate entire cache.  Write back dirty lines.
    //
    method Action invalAllReq(t_CACHE_REF_INFO refInfo) if (! invalidateAllReq);
        debugLog.record($format("  New request: INVAL ALL"));
        invalidateAll_refInfo <= refInfo;
        invalidateAllReq <= True;
    endmethod

    method Action invalAllWait() if (curState == HCST_INVAL_ALL_DONE);
        curState <= HCST_IDLE;
    endmethod

    //
    // setModeWriteBack -- Write back or write through cache config.
    //
    method Action setModeWriteBack(Bool isWriteBack);
        if (writeBackCache != isWriteBack)
            debugLog.record($format("Cache mode: WRITE %s", (isWriteBack ? "BACK" : "THROUGH")));

        writeBackCache <= isWriteBack;
    endmethod

endmodule



// ===================================================================
//
// Null version of HASIM_CACHE_STATS interface for clients not interested in
// statistics.
//
// ===================================================================

module mkNullHAsimCacheStats
    // interface:
        (HASIM_CACHE_STATS);
    
    method Action readHit();
    endmethod

    method Action readMiss();
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
