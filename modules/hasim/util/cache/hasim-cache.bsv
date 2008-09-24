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
import Vector::*;

// Project foundation imports.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"


// ===================================================================
//
// PUBLIC DATA STRUCTURES
//
// ===================================================================

//
// HAsim cache interface.  nTagExtraLowBits is used just for debugging.
// This specified number of low bits are prepanded to cache tags so
// addresses match those seen in other modules.

interface HASIM_CACHE#(type t_CACHE_ADDR,
                       type t_CACHE_DATA,
                       numeric type nSets,
                       numeric type nWays,
                       numeric type nTagExtraLowBits);

    // Read a full line.  Read from backing store if not already cached.
    method Action readReq(t_CACHE_ADDR addr);
    method ActionValue#(t_CACHE_DATA) readResp();
    
    // Write a full line.  No response.
    method Action writeReq(t_CACHE_ADDR addr, t_CACHE_DATA val);
    
    // Write a partial line.  Writes only where mask is 1.  Read line from backing
    // store before write if not already cached.  No response.
    method Action writeMaskedReq(t_CACHE_ADDR addr, t_CACHE_DATA val, t_CACHE_DATA mask);

    // Invalidate & flush requests.  Both write dirty lines back.  Invalidate drops
    // the line from the cache.  Flush keeps the line in the cache.  A response
    // is returned for invalOrFlushWait iff sendAck is true.
    method Action invalReq(t_CACHE_ADDR addr, Bool sendAck);
    method Action flushReq(t_CACHE_ADDR addr, Bool sendAck);
    method Action invalOrFlushWait();

    // Invalidate entire cache.
    method Action invalAllReq();
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
interface HASIM_CACHE_SOURCE_DATA#(type t_CACHE_ADDR,
                                   type t_CACHE_DATA);

    // Read request and response with data
    method Action readReq(t_CACHE_ADDR addr);
    method ActionValue#(t_CACHE_DATA) readResp();
    
    // Asynchronous write (no response)
    method Action write(t_CACHE_ADDR addr, t_CACHE_DATA val);
    
    // Synchronous write.  writeSyncWait() blocks until the response arrives.
    method Action writeSyncReq(t_CACHE_ADDR addr, t_CACHE_DATA val);
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

    // Initial processing of new request
    HCST_START_REQ,

    // Waiting for read from cache BRAM to service a READ request
    HCST_CACHE_READ_PENDING,

    // Waiting for read for partial line write
    HCST_CACHE_RMW_PENDING,

    // Waiting for read from cache BRAM.  Line will be flushed to backing store.
    HCST_FLUSH_DIRTY,

    // Data for a read is coming from backing store
    HCST_FILL_FOR_READ_PENDING,

    // Data for a partial write is coming from backing store
    HCST_FILL_FOR_RMW_PENDING,

    // Waiting for backing store response for a flush for an invalidate/flush request.
    HCST_FLUSH_SYNC_PENDING
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


//
// Hacks for defining type aliases within a module to work around lack of
// "typedef" inside module scope.
//
typeclass Alias#(type a, type b);
endtypeclass

instance Alias#(a,a);
endinstance


//
// Set associative cache
//

module [HASIM_MODULE] mkCacheSetAssoc#(HASIM_CACHE_SOURCE_DATA#(Bit#(t_CACHE_ADDR_SZ), t_CACHE_DATA) sourceData,
                                       HASIM_CACHE_STATS stats,
                                       DEBUG_FILE debugLog)
    // interface:
        (HASIM_CACHE#(Bit#(t_CACHE_ADDR_SZ), t_CACHE_DATA, nSets, nWays, nTagExtraLowBits))
    provisos (Bits#(t_CACHE_DATA, t_CACHE_DATA_SZ),
              Log#(nWays, TLog#(nWays)),
              // Silly, but required by compiler...
              Add#(t_CACHE_ADDR_SZ, nTagExtraLowBits, TAdd#(t_CACHE_ADDR_SZ, nTagExtraLowBits)),

              Alias#(HASIM_CACHE_SET_IDX#(nSets), t_CACHE_SET_IDX),
              Bits#(t_CACHE_SET_IDX, t_CACHE_SET_IDX_SZ),
              Add#(t_CACHE_SET_IDX_SZ, b__, 32),

              Alias#(Bit#(t_CACHE_ADDR_SZ), t_CACHE_ADDR),
              Alias#(HASIM_CACHE_WAY_IDX#(nWays), t_CACHE_WAY_IDX),
              Alias#(HASIM_CACHE_WRITE_INFO#(t_CACHE_ADDR, t_CACHE_DATA), t_CACHE_WRITE_INFO),
              Alias#(Vector#(nWays, HASIM_CACHE_WAY_IDX#(nWays)), t_LRU_LIST),
              Alias#(HASIM_CACHE_DATA_IDX#(nWays, t_CACHE_SET_IDX), t_CACHE_DATA_IDX),
              Alias#(HASIM_CACHE_METADATA#(t_CACHE_ADDR), t_METADATA),
              Alias#(Vector#(nWays, Maybe#(HASIM_CACHE_METADATA#(t_CACHE_ADDR))), t_METADATA_VECTOR));


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

    //
    // These registers hold the state for the current request
    //
    Reg#(HASIM_CACHE_OPERATION) reqInfo_oper <- mkRegU();
    Reg#(t_CACHE_ADDR)          reqInfo_addr <- mkRegU();
    Reg#(t_CACHE_SET_IDX)       reqInfo_set <- mkRegU();
    Reg#(t_CACHE_WRITE_INFO)    reqInfo_wInfo <- mkRegU();  // Valid only for writes
    Reg#(Bool)                  reqInfo_ack <- mkRegU();    // Inval request sends response?

    // The way becomes part of the state for the request, computed either
    // as the way for a hit or a victim.
    Reg#(t_CACHE_WAY_IDX)       reqInfo_way <- mkRegU();

    // The flush address is either set as a request comes in for flush/inval or
    // when a line must be evicted in the middle of processing a request.
    Reg#(t_CACHE_ADDR)          reqInfo_flushAddr <- mkRegU();

    // Is the cache write back?  If not, never set a dirty bit.  It is then the
    // responsibility of the caller to write values to backing storage.
    Reg#(Bool) writeBackCache <- mkReg(True);

    // State for invalidate all
    Reg#(Bool)             invalidatingAll  <- mkReg(False);
    Reg#(Bool)             invalidatingAllDone  <- mkReg(False);
    Reg#(t_CACHE_SET_IDX)  invalidateAllSet <- mkReg(0);
    Reg#(t_CACHE_WAY_IDX)  invalidateFlushWay <- mkReg(0);

    // ***** Queues between internal pipeline stages *****

    // Response to client, returned 
    FIFO#(t_CACHE_DATA) respToClient <- mkFIFO();
    FIFO#(Bool) invalReqDone <- mkFIFO1();
    FIFO#(Bool) invalAllReqDone <- mkFIFO1();

    FIFO#(Tuple2#(t_CACHE_SET_IDX, t_METADATA_VECTOR)) flushDirtySet <- mkFIFO();


    // ***** Indexing functions *****

    function t_CACHE_DATA_IDX getDataIdx (t_CACHE_SET_IDX set, t_CACHE_WAY_IDX way);

        t_CACHE_DATA_IDX idx;
        idx.set = set;
        idx.way = way;
        return idx;

    endfunction


    function t_CACHE_SET_IDX cacheSet(t_CACHE_ADDR addr);
    
        // Silly statement to keep the compiler happy about sizes.  Using
        // truncate or extend forces an assumption about sizes < or > 32
        // bits in spite of there already being a proviso that the set
        // size is <= 32.
        //
        // Get up to 32 bits of the address into addr32...
        Bit#(32) addr32 = 0 | addr[min(32, valueOf(t_CACHE_SET_IDX_SZ)) : 0];

        // Compute the hash...
        return unpack(truncate(hashTo32(addr32)));

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

        Maybe#(t_CACHE_WAY_IDX) wayMatch = tagged Invalid;

        for (Integer w = 0; w < valueOf(nWays); w = w + 1)
        begin
            if (meta[w] matches tagged Valid .m &&& m.addr == addr)
            begin
                wayMatch = tagged Valid fromInteger(w);
            end
        end
    
        return wayMatch;

    endfunction


    function Maybe#(t_CACHE_WAY_IDX) findFirstInvalid(t_METADATA_VECTOR meta);
    
        Maybe#(t_CACHE_WAY_IDX) wayMatch = tagged Invalid;

        for (Integer w = 0; w < valueOf(nWays); w = w + 1)
        begin
            if (meta[w] matches tagged Invalid)
            begin
                wayMatch = tagged Valid fromInteger(w);
            end
        end
    
        return wayMatch;

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
    // handleInvalOrFlush --
    //     Invalidate and flush requests have similar handling.  Both write
    //     back a dirty matching line.  Flush preserves the now clean line
    //     in the cache.
    //
    rule handleInvalOrFlush (curState == HCST_START_REQ &&&
                             (reqInfo_oper == HCOP_INVAL) || (reqInfo_oper == HCOP_FLUSH_DIRTY));

        let cur_lru <- cacheLRU.readRsp();
        let meta <- cacheMeta.readRsp();

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
            curState <= HCST_IDLE;
            if (reqInfo_ack)
                invalReqDone.enq(?);
        end
    endrule


    //
    // handleReadOrWrite --
    //     Cache read or write request.
    //
    rule handleReadOrWrite (curState == HCST_START_REQ &&&
                            (reqInfo_oper == HCOP_READ) || (reqInfo_oper == HCOP_WRITE));

        let cur_lru <- cacheLRU.readRsp();
        let meta <- cacheMeta.readRsp();

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

                if (reqInfo_wInfo.isMasked)
                begin
                    // Partial write.  Start by reading current value.
                    cacheData.readReq(idx);
                    curState <= HCST_CACHE_RMW_PENDING;
                end
                else
                begin
                    // Full line write.  Now we're done.
                    debugLog.record($format("  Write HIT: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, reqInfo_wInfo.val));
                    cacheData.write(idx, reqInfo_wInfo.val);
                    curState <= HCST_IDLE;
                end
            end
        end
        else
        begin
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
            Bool need_backing_data = (is_read || reqInfo_wInfo.isMasked);
            if (need_backing_data)
            begin
                sourceData.readReq(addr);
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
                debugLog.record($format("  Write to INVAL: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, fill_way, reqInfo_wInfo.val));
                cacheData.write(idx, reqInfo_wInfo.val);
                curState <= HCST_IDLE;
            end

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

        respToClient.enq(v);
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

        v = unpack((pack(v) & ~pack(reqInfo_wInfo.mask)) |
                   (pack(reqInfo_wInfo.val) & pack(reqInfo_wInfo.mask)));

        cacheData.write(getDataIdx(set, way), v);
        curState <= HCST_IDLE;

        debugLog.record($format("  WRITE masked: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, v));
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

        if (invalidatingAll)
        begin
            // Invalidating whole cache.  Just write out the line without
            // a state change.
            sourceData.write(addr, flushData);
        end
        else if (reqInfo_oper == HCOP_READ || reqInfo_oper == HCOP_WRITE)
        begin
            // Normal flush before a fill
            sourceData.write(addr, flushData);

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
            sourceData.writeSyncReq(addr, flushData);
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

        respToClient.enq(v);
        debugLog.record($format("  Read FILL: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, v));
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
        v = unpack((pack(v) & ~pack(reqInfo_wInfo.mask)) |
                   (pack(reqInfo_wInfo.val) & pack(reqInfo_wInfo.mask)));
        cacheData.write(getDataIdx(set, way), v);

        debugLog.record($format("  WRITE masked: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", debugAddr(addr), set, way, v));
        curState <= HCST_IDLE;

    endrule


    //
    // handleFlushACK --
    //   Wait for the response to a flush from back storage for synchronous
    //   flushes.
    //
    rule handleFlushACK (curState == HCST_FLUSH_SYNC_PENDING);

        sourceData.writeSyncWait();
        curState <= HCST_IDLE;

        if (reqInfo_ack)
            invalReqDone.enq(?);

        debugLog.record($format("  FLUSH or INVAL done"));

    endrule


    //
    // handleInvalidateAll --
    //     Memory system may request invalidation of the entire cache if it
    //     doesn't know which lines may need to be flushed.
    //
    rule handleInvalidateAll (invalidatingAll && ! invalidatingAllDone);

        cacheLRU.write(invalidateAllSet, Vector::genWith(fromInteger));
        cacheMeta.write(invalidateAllSet, Vector::replicate(tagged Invalid));

        // Flush dirty lines
        let meta <- cacheMeta.readRsp();
        flushDirtySet.enq(tuple2(invalidateAllSet, meta));

        if (invalidateAllSet == maxBound)
        begin
            invalidatingAllDone <= True;
        end
        else
        begin
            cacheMeta.readReq(invalidateAllSet);
        end

        invalidateAllSet <= invalidateAllSet + 1;

    endrule


    // Not required for correctness: get rid of a couple warning messages...
    (* descending_urgency= "handleReadOrWrite, handleInvalidateAll" *)
    (* descending_urgency= "handleInvalOrFlush, handleInvalidateAll" *)

    //
    // handleInvalSet --
    //   Invalidate an entire set (requested by handleInvalidateAll).
    //
    rule handleInvalSet (invalidatingAll && curState == HCST_IDLE);
        
        match {.set, .meta} = flushDirtySet.first();
        
        if (meta[invalidateFlushWay] matches tagged Valid .m &&& m.dirty)
        begin
            reqInfo_flushAddr <= m.addr;
            reqInfo_set <= set;
            reqInfo_way <= invalidateFlushWay;

            cacheData.readReq(getDataIdx(set, invalidateFlushWay));
            curState <= HCST_FLUSH_DIRTY;
        end

        // Done with the set?
        if (invalidateFlushWay == maxBound)
        begin
            // Done.  Pass the request on to the fill stage, if appropriate.
            flushDirtySet.deq();
            
            // Done with invalidateingAll?
            if (invalidatingAllDone)
            begin
                invalidatingAll <= False;
                invalidatingAllDone <= False;
                invalAllReqDone.enq(?);
                debugLog.record($format("  Request done: INVAL ALL"));
            end
        end

        invalidateFlushWay <= invalidateFlushWay + 1;

    endrule


    //
    // genRequest --
    //     This function is used by most of the request methods to generate
    //     the internal data structure for managing a request.  It also starts
    //     the first step:  reading metadata from BRAM.
    //
    function ActionValue#(t_CACHE_SET_IDX) genRequest(HASIM_CACHE_OPERATION oper, t_CACHE_ADDR addr);
    actionvalue

        curState <= HCST_START_REQ;

        let set = cacheSet(addr);

        reqInfo_oper <= oper;
        reqInfo_addr <= addr;
        reqInfo_set <= set;

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


    // ***** Methods ***** //

    //
    // readReq -- Read a full line.  Fetch from backing store if not in the cache.
    //
    method Action readReq(t_CACHE_ADDR addr) if (curState == HCST_IDLE && !invalidatingAll);
        let set <- genRequest(HCOP_READ, addr);
        debugLog.record($format("  New request: READ addr=0x%x, set=0x%x", debugAddr(addr), set));
    endmethod

    method ActionValue#(t_CACHE_DATA) readResp();
        let v = respToClient.first();
        respToClient.deq();
        return v;
    endmethod
    
    //
    // writeReq -- Write a full line.
    //
    method Action writeReq(t_CACHE_ADDR addr, t_CACHE_DATA val) if (curState == HCST_IDLE && !invalidatingAll);
        let set <- genRequest(HCOP_WRITE, addr);

        t_CACHE_WRITE_INFO wInfo;
        wInfo.val = val;
        wInfo.mask = ?;
        wInfo.isMasked = False;
        reqInfo_wInfo <= wInfo;

        debugLog.record($format("  New request: WRITE addr=0x%x, set=0x%x, data=0x%x", debugAddr(addr), set, val));
    endmethod


    //
    // writeMaskedReq --
    //     Write a partial line.  Fetch from backing store if not in the cache.
    //
    method Action writeMaskedReq(t_CACHE_ADDR addr, t_CACHE_DATA val, t_CACHE_DATA mask) if (curState == HCST_IDLE && !invalidatingAll);
        let set <- genRequest(HCOP_WRITE, addr);

        t_CACHE_WRITE_INFO wInfo;
        wInfo.val = val;
        wInfo.mask = mask;
        wInfo.isMasked = True;
        reqInfo_wInfo <= wInfo;

        debugLog.record($format("  New request: WRITE MASKED addr=0x%x, set=0x%x, data=0x%x, mask=0x%x", debugAddr(addr), set, val, mask));
    endmethod
    

    //
    // invalReq -- Invalidate (remove) a line from the cache
    //
    method Action invalReq(t_CACHE_ADDR addr, Bool sendAck) if (curState == HCST_IDLE && !invalidatingAll);
        let set <- genRequest(HCOP_INVAL, addr);
        reqInfo_ack <= sendAck;
        debugLog.record($format("  New request: INVAL addr=0x%x, set=0x%x", debugAddr(addr), set));
    endmethod
    

    //
    // flushReq --
    //     Flush (write back) a line from the cache but keep the line cached.
    //
    method Action flushReq(t_CACHE_ADDR addr, Bool sendAck) if (curState == HCST_IDLE && !invalidatingAll);
        let set <- genRequest(HCOP_FLUSH_DIRTY, addr);
        reqInfo_ack <= sendAck;
        debugLog.record($format("  New request: FLUSH addr=0x%x, set=0x%x", debugAddr(addr), set));
    endmethod


    //
    // invalOrFlushWait -- Block until an inval or flush request completes.
    //
    method Action invalOrFlushWait();
        invalReqDone.deq();
    endmethod


    //
    // invalAllReq -- Invalidate entire cache.  Write back dirty lines.
    //
    method Action invalAllReq() if (curState == HCST_IDLE && !invalidatingAll);
        debugLog.record($format("  New request: INVAL ALL"));

        if (cacheIsEmpty)
        begin
            invalAllReqDone.enq(?);
        end
        else
        begin
            cacheMeta.readReq(0);
            invalidatingAll <= True;
            cacheIsEmpty <= True;
        end
    endmethod

    method Action invalAllWait();
        invalAllReqDone.deq();
    endmethod

    //
    // setModeWriteBack -- Write back or write through cache config.
    //
    method Action setModeWriteBack(Bool isWriteBack);
        debugLog.record($format("  Cache mode: WRITE %s", (isWriteBack ? "BACK" : "THROUGH")));
        writeBackCache <= isWriteBack;
    endmethod

endmodule



// ===================================================================
//
// Null version of HASIM_CACHE_STATS interface for clients not interested in
// statistics.
//
// ===================================================================

module [HASIM_MODULE] mkNullHAsimCacheStats
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
