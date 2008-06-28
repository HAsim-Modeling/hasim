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
// Set-associative write back cache
//

//
// Run-time parameters controlling behavior:
//
// ENABLE_FUNCP_MEM_CACHE:
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

// The memory virtual device

`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/dict/PARAMS_FUNCP_MEMSTATE_CACHE.bsh"
`include "asim/dict/ASSERTIONS_FUNCP_MEMSTATE_CACHE.bsh"
`include "asim/dict/STATS_FUNCP_MEMSTATE_CACHE.bsh"


typedef Bit#(TLog#(TDiv#(`FUNCP_ISA_INT_REG_SIZE,8)))  WORD_OFFSET;
typedef Bit#(TLog#(CACHELINE_WORDS))                   CACHELINE_OFFSET;
typedef Bit#(TSub#(`FUNCP_ISA_ADDR_SIZE,TLog#(TDiv#(`FUNCP_CACHELINE_BITS,8)))) CACHE_TAG;

typedef Bit#(`FUNCP_CACHE_IDX_BITS) CACHE_SET;
typedef UInt#(TLog#(`FUNCP_CACHE_WAYS)) CACHE_WAY;

typedef Vector#(`FUNCP_CACHE_WAYS, CACHE_WAY) CACHE_LRU_LIST;


//
// Cache metadata (tag and a dirty bit)
//
typedef struct
{
    CACHE_TAG tag;
    Bool dirty;
}
  CACHE_METADATA
    deriving(Eq, Bits);

typedef Vector#(`FUNCP_CACHE_WAYS, Maybe#(CACHE_METADATA)) CACHE_METADATA_VECTOR;

//
// Request structure passed along the cache.  Index computation uses a hash and
// is expensive, so it is done once and passed along.
//
typedef struct
{
    MEM_REQUEST req;
    CACHE_SET   set;
}
  CACHE_ACCESS
    deriving(Eq, Bits);

//
// The cache data is indexed by three things:  the set, the way within the set
// and the offset in the line.  Declaring the cache data as multiply indexed
// vectors results in a large amount of extra LUT usage to control the
// BRAMs.  Instead, we allocate a single large cache data BRAM and index it
// with a packed version of this structure:
//
typedef struct
{
    CACHE_SET set;
    CACHE_WAY way;
    CACHELINE_OFFSET offset;
}
  CACHE_DATA_IDX
    deriving(Eq, Bits);

//
// cacheSet --
//     The cache index hash function here is not the traditional direct copying
//     of low bits from the address.  The goal is an even distribution of index
//     values independent of the memory access pattern.
//
function CACHE_SET cacheSet(MEM_ADDRESS addr);

    Tuple3#(CACHE_TAG,CACHELINE_OFFSET,WORD_OFFSET) tup = unpack(addr);
    match { .tag, .cloff, .woff } = tup;

    // Don't know whether tag is bigger or smaller than 32 bits, hence this hack...
    Bit#(32) d = truncate({ 32'b0, tag });
    Bit#(32) set;

    //
    // CRC-32 (IEEE802.3), polynomial 0 1 2 4 5 7 8 10 11 12 16 22 23 26 32
    //   Define a large CRC result so we can grow the caches without changing
    //   this code.  Bluespec optimizer will drop extra bits.
    //
    set[0] = d[31] ^ d[30] ^ d[29] ^ d[28] ^ d[26] ^ d[25] ^ d[24] ^ 
             d[16] ^ d[12] ^ d[10] ^ d[9] ^ d[6] ^ d[0];
    set[1] = d[28] ^ d[27] ^ d[24] ^ d[17] ^ d[16] ^ d[13] ^ d[12] ^ 
             d[11] ^ d[9] ^ d[7] ^ d[6] ^ d[1] ^ d[0];
    set[2] = d[31] ^ d[30] ^ d[26] ^ d[24] ^ d[18] ^ d[17] ^ d[16] ^ 
             d[14] ^ d[13] ^ d[9] ^ d[8] ^ d[7] ^ d[6] ^ d[2] ^ 
             d[1] ^ d[0];
    set[3] = d[31] ^ d[27] ^ d[25] ^ d[19] ^ d[18] ^ d[17] ^ d[15] ^ 
             d[14] ^ d[10] ^ d[9] ^ d[8] ^ d[7] ^ d[3] ^ d[2] ^ 
             d[1];
    set[4] = d[31] ^ d[30] ^ d[29] ^ d[25] ^ d[24] ^ d[20] ^ d[19] ^ 
             d[18] ^ d[15] ^ d[12] ^ d[11] ^ d[8] ^ d[6] ^ d[4] ^ 
             d[3] ^ d[2] ^ d[0];
    set[5] = d[29] ^ d[28] ^ d[24] ^ d[21] ^ d[20] ^ d[19] ^ d[13] ^ 
             d[10] ^ d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[3] ^ d[1] ^ d[0];
    set[6] = d[30] ^ d[29] ^ d[25] ^ d[22] ^ d[21] ^ d[20] ^ d[14] ^ 
             d[11] ^ d[8] ^ d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[2] ^ d[1];
    set[7] = d[29] ^ d[28] ^ d[25] ^ d[24] ^ d[23] ^ d[22] ^ d[21] ^ 
             d[16] ^ d[15] ^ d[10] ^ d[8] ^ d[7] ^ d[5] ^ d[3] ^ 
             d[2] ^ d[0];
    set[8] = d[31] ^ d[28] ^ d[23] ^ d[22] ^ d[17] ^ d[12] ^ d[11] ^ 
             d[10] ^ d[8] ^ d[4] ^ d[3] ^ d[1] ^ d[0];
    set[9] = d[29] ^ d[24] ^ d[23] ^ d[18] ^ d[13] ^ d[12] ^ d[11] ^ 
             d[9] ^ d[5] ^ d[4] ^ d[2] ^ d[1];
    set[10] = d[31] ^ d[29] ^ d[28] ^ d[26] ^ d[19] ^ d[16] ^ d[14] ^ 
              d[13] ^ d[9] ^ d[5] ^ d[3] ^ d[2] ^ d[0];
    set[11] = d[31] ^ d[28] ^ d[27] ^ d[26] ^ d[25] ^ d[24] ^ d[20] ^ 
              d[17] ^ d[16] ^ d[15] ^ d[14] ^ d[12] ^ d[9] ^ d[4] ^ 
              d[3] ^ d[1] ^ d[0];
    set[12] = d[31] ^ d[30] ^ d[27] ^ d[24] ^ d[21] ^ d[18] ^ d[17] ^ 
              d[15] ^ d[13] ^ d[12] ^ d[9] ^ d[6] ^ d[5] ^ d[4] ^ 
              d[2] ^ d[1] ^ d[0];
    set[13] = d[31] ^ d[28] ^ d[25] ^ d[22] ^ d[19] ^ d[18] ^ d[16] ^ 
              d[14] ^ d[13] ^ d[10] ^ d[7] ^ d[6] ^ d[5] ^ d[3] ^ 
              d[2] ^ d[1];
    set[14] = d[29] ^ d[26] ^ d[23] ^ d[20] ^ d[19] ^ d[17] ^ d[15] ^ 
              d[14] ^ d[11] ^ d[8] ^ d[7] ^ d[6] ^ d[4] ^ d[3] ^ 
              d[2];
    set[15] = d[30] ^ d[27] ^ d[24] ^ d[21] ^ d[20] ^ d[18] ^ d[16] ^ 
              d[15] ^ d[12] ^ d[9] ^ d[8] ^ d[7] ^ d[5] ^ d[4] ^ 
              d[3];
    set[16] = d[30] ^ d[29] ^ d[26] ^ d[24] ^ d[22] ^ d[21] ^ d[19] ^ 
              d[17] ^ d[13] ^ d[12] ^ d[8] ^ d[5] ^ d[4] ^ d[0];
    set[17] = d[31] ^ d[30] ^ d[27] ^ d[25] ^ d[23] ^ d[22] ^ d[20] ^ 
              d[18] ^ d[14] ^ d[13] ^ d[9] ^ d[6] ^ d[5] ^ d[1];
    set[18] = d[31] ^ d[28] ^ d[26] ^ d[24] ^ d[23] ^ d[21] ^ d[19] ^ 
              d[15] ^ d[14] ^ d[10] ^ d[7] ^ d[6] ^ d[2];
    set[19] = d[29] ^ d[27] ^ d[25] ^ d[24] ^ d[22] ^ d[20] ^ d[16] ^ 
              d[15] ^ d[11] ^ d[8] ^ d[7] ^ d[3];
    set[20] = d[30] ^ d[28] ^ d[26] ^ d[25] ^ d[23] ^ d[21] ^ d[17] ^ 
              d[16] ^ d[12] ^ d[9] ^ d[8] ^ d[4];
    set[21] = d[31] ^ d[29] ^ d[27] ^ d[26] ^ d[24] ^ d[22] ^ d[18] ^ 
              d[17] ^ d[13] ^ d[10] ^ d[9] ^ d[5];
    set[22] = d[31] ^ d[29] ^ d[27] ^ d[26] ^ d[24] ^ d[23] ^ d[19] ^ 
              d[18] ^ d[16] ^ d[14] ^ d[12] ^ d[11] ^ d[9] ^ d[0];
    set[23] = d[31] ^ d[29] ^ d[27] ^ d[26] ^ d[20] ^ d[19] ^ d[17] ^ 
              d[16] ^ d[15] ^ d[13] ^ d[9] ^ d[6] ^ d[1] ^ d[0];
    set[24] = d[30] ^ d[28] ^ d[27] ^ d[21] ^ d[20] ^ d[18] ^ d[17] ^ 
              d[16] ^ d[14] ^ d[10] ^ d[7] ^ d[2] ^ d[1];
    set[25] = d[31] ^ d[29] ^ d[28] ^ d[22] ^ d[21] ^ d[19] ^ d[18] ^ 
              d[17] ^ d[15] ^ d[11] ^ d[8] ^ d[3] ^ d[2];
    set[26] = d[31] ^ d[28] ^ d[26] ^ d[25] ^ d[24] ^ d[23] ^ d[22] ^ 
              d[20] ^ d[19] ^ d[18] ^ d[10] ^ d[6] ^ d[4] ^ d[3] ^ 
              d[0];
    set[27] = d[29] ^ d[27] ^ d[26] ^ d[25] ^ d[24] ^ d[23] ^ d[21] ^ 
              d[20] ^ d[19] ^ d[11] ^ d[7] ^ d[5] ^ d[4] ^ d[1];
    set[28] = d[30] ^ d[28] ^ d[27] ^ d[26] ^ d[25] ^ d[24] ^ d[22] ^ 
              d[21] ^ d[20] ^ d[12] ^ d[8] ^ d[6] ^ d[5] ^ d[2];
    set[29] = d[31] ^ d[29] ^ d[28] ^ d[27] ^ d[26] ^ d[25] ^ d[23] ^ 
              d[22] ^ d[21] ^ d[13] ^ d[9] ^ d[7] ^ d[6] ^ d[3];
    set[30] = d[30] ^ d[29] ^ d[28] ^ d[27] ^ d[26] ^ d[24] ^ d[23] ^ 
              d[22] ^ d[14] ^ d[10] ^ d[8] ^ d[7] ^ d[4];
    set[31] = d[31] ^ d[30] ^ d[29] ^ d[28] ^ d[27] ^ d[25] ^ d[24] ^ 
              d[23] ^ d[15] ^ d[11] ^ d[9] ^ d[8] ^ d[5];

    return truncate(set);

endfunction


module [HASIM_MODULE] mkFUNCP_Cache ()
    provisos(Bits#(CACHE_SET, cache_set_SZ),
             Bits#(CACHE_WAY, cache_way_SZ),
             Bits#(CACHELINE_OFFSET, cacheline_offset_SZ),
             Bits#(CACHE_DATA_IDX, cache_data_idx_SZ));

    // ***** Soft Connections *****

    Connection_Server#(MEM_REQUEST, MEM_VALUE) link_memstate               <- mkConnection_Server("mem_cache");
    Connection_Client#(MEM_REQUEST, MEM_REPLY) link_funcp_memory           <- mkConnection_Client("funcp_memory");
    Connection_Receive#(MEM_ADDRESS)           link_funcp_memory_inval     <- mkConnection_Receive("funcp_memory_invalidate");
    Connection_Receive#(Bool)                  link_funcp_memory_inval_all <- mkConnection_Receive("funcp_memory_invalidate_all");
    Connection_Send#(Bool)                link_funcp_memory_inval_all_done <- mkConnection_Send("funcp_memory_invalidate_all_done");

    // ***** Cache data *****

    // Tags & dirty bits
    BRAM#(cache_set_SZ, CACHE_METADATA_VECTOR) cacheMeta <- mkBramInitialized(Vector::replicate(tagged Invalid));
    // Values
    BRAM#(cache_data_idx_SZ, MEM_VALUE) cacheData <- mkBram();
    // LRU hint
    BRAM#(cache_set_SZ, CACHE_LRU_LIST) cacheLRU <- mkBramInitialized(Vector::genWith(fromInteger));

    // ***** Internal state *****

    FIFO#(CACHE_ACCESS) pendingQ <- mkFIFO1; // size=1 -> blocking. we'll need a searchable fifo for size >=2.
    Reg#(Bool)          waiting  <- mkReg(False);

    Reg#(Bool)      invalidatingAll  <- mkReg(False);
    Reg#(CACHE_SET) invalidateAllSet <- mkReg(0);

    Reg#(CACHELINE_OFFSET) fillLineOffset <- mkReg(0);
    Reg#(CACHELINE_OFFSET) flushLineOffset <- mkReg(0);
    Reg#(CACHE_WAY)        flushWay <- mkReg(0);

    Param#(1) enableCacheParam <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_CACHE_ENABLE_FUNCP_MEM_CACHE);
    function Bool enableCache() = (enableCacheParam == 1);

    Param#(1) writeBackParam <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_CACHE_FUNCP_MEM_CACHE_WRITE_BACK);
    function Bool writeBackCache() = (writeBackParam == 1);

    // ***** Queues between internal pipeline stages *****

    FIFO#(Tuple2#(CACHE_WAY, CACHELINE_OFFSET)) loadFromCache <- mkFIFO();
    FIFO#(Tuple2#(CACHE_SET, CACHE_METADATA_VECTOR)) flushDirtySet <- mkFIFO();
    FIFO#(Tuple2#(CACHE_SET, CACHE_WAY)) fillVictim <- mkFIFO();

    // flushDirtyLine must be a blocking FIFO1 since the writers must also request
    // a cache line and the consumer iterates, reading more lines.
    FIFO#(Tuple4#(CACHE_TAG, CACHE_SET, CACHE_WAY, Bool)) flushDirtyLine <- mkFIFO1();

    // ***** Statistics *****

    Stat statLoadHit   <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_LOAD_HIT);
    Stat statLoadMiss  <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_LOAD_MISS);
    Stat statLoadMissValidVictim
                       <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_LOAD_MISS_VALID_VICTIM);

    Stat statStoreHit  <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_STORE_HIT);
    Stat statStoreMiss <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_STORE_MISS);
    Stat statStoreMissValidVictim
                       <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_STORE_MISS_VALID_VICTIM);
    Stat statDirtyLineFlush
                       <- mkStatCounter(`STATS_FUNCP_MEMSTATE_CACHE_DIRTY_LINE_FLUSH);

    // ***** Assertion Checkers *****

    Assertion assertLegalSize    <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_CACHE_ILLEGAL_SIZE, ASSERT_ERROR);
    Assertion assertValidRequest <- mkAssertionChecker(`ASSERTIONS_FUNCP_MEMSTATE_CACHE_INVALID_REQUEST, ASSERT_ERROR);


    // ******* Debuging State *******

    // Fake register to hold our debugging file descriptor.
    let debugLog         <- mkReg(InvalidFile);
    Reg#(Bool) debugInit <- mkReg(False);

    // The current FPGA clock cycle
    Reg#(Bit#(32)) fpgaCC <- mkReg(0);

    rule currentCC (True);

        fpgaCC <= fpgaCC + 1;

    endrule

        //Open the debug logs. (First time only. Afterwards it is not InvalidFile.)
    rule debugDoInit (! debugInit);

        debugInit <= True;

        let fd <- $fopen(`FUNCP_CACHE_LOGFILE_NAME, "w");
        if (fd == InvalidFile)
        begin
            $display(strConcat("Error opening FUNCP memory cache logfile ", `FUNCP_CACHE_LOGFILE_NAME));
            $finish(1);
        end

        debugLog <= fd;

    endrule


    // ***** Indexing functions *****

    function Bit#(cache_data_idx_SZ) getDataIdx (CACHE_SET set, CACHE_WAY way, CACHELINE_OFFSET offset);

        return pack(CACHE_DATA_IDX { set: set, way: way, offset: offset });

    endfunction


    function MEM_ADDRESS cacheAddr(CACHE_TAG tag, CACHELINE_OFFSET cloff);

        WORD_OFFSET woff = 0;
        return { tag, cloff, woff};

    endfunction


    function CACHE_TAG cacheTag(MEM_ADDRESS addr);

        Tuple3#(CACHE_TAG,CACHELINE_OFFSET,WORD_OFFSET) tup = unpack(addr);
        match { .tag, .cloff, .woff } = tup;
        return tag;

    endfunction


    function CACHELINE_OFFSET cacheLineOffset(MEM_ADDRESS addr);

        Tuple3#(CACHE_TAG,CACHELINE_OFFSET,WORD_OFFSET) tup = unpack(addr);
        match { .tag, .cloff, .woff } = tup;
        return cloff;

    endfunction


    function WORD_OFFSET cacheWordOffset(MEM_ADDRESS addr);

        Tuple3#(CACHE_TAG,CACHELINE_OFFSET,WORD_OFFSET) tup = unpack(addr);
        match { .tag, .cloff, .woff } = tup;
        return woff;

    endfunction


    function MEM_ADDRESS getLineAddr (MEM_ADDRESS addr);

        CACHELINE_OFFSET cloff_0 = 0;
        WORD_OFFSET      woff_0  = 0;
        return pack(tuple3(cacheTag(addr),cloff_0,woff_0));

    endfunction


    // ***** Meta data searches *****

    function Maybe#(CACHE_WAY) findWayMatch(MEM_ADDRESS addr, CACHE_METADATA_VECTOR meta);
    
        Maybe#(CACHE_WAY) wayMatch = tagged Invalid;

        for (Integer w = 0; w < `FUNCP_CACHE_WAYS; w = w + 1)
        begin
            if (meta[w] matches tagged Valid .m &&& m.tag == cacheTag(addr))
            begin
                wayMatch = tagged Valid fromInteger(w);
            end
        end
    
        return wayMatch;

    endfunction


    function Maybe#(CACHE_WAY) findFirstInvalid(CACHE_METADATA_VECTOR meta);
    
        Maybe#(CACHE_WAY) wayMatch = tagged Invalid;

        for (Integer w = 0; w < `FUNCP_CACHE_WAYS; w = w + 1)
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
    function CACHE_WAY getLRU(CACHE_LRU_LIST list);

        return list[`FUNCP_CACHE_WAYS - 1];

    endfunction


    //
    // getMRU --
    //   Most recently used way in a set.
    //

    function CACHE_WAY getMRU(CACHE_LRU_LIST list);

        return list[0];

    endfunction


    //
    // pushMRU --
    //   Update MRU list, moving a way to the head of the list.
    //
    function CACHE_LRU_LIST pushMRU(CACHE_LRU_LIST curLRU, CACHE_WAY mru);

        CACHE_LRU_LIST new_list = curLRU;
    
        //
        // Find the new MRU value in the current list
        //
        if (findElem(mru, curLRU) matches tagged Valid .mru_pos)
        begin
            //
            // Shift older references out of the MRU slot
            //
            for (CACHE_WAY w = 0; w < mru_pos; w = w + 1)
            begin
                new_list[w + 1] = curLRU[w];
            end

            // MRU is slot 0
            new_list[0] = mru;
        end

        return new_list;

    endfunction


    //
    // Convenience functions for debugging
    //

    function Action cacheDebug(Action a);
    action

        $fwrite(debugLog, "[%d]: ", fpgaCC);
        a;
        $fwrite(debugLog, "\n");

    endaction
    endfunction

    function Action cacheDebugLRUUpdate(String access_type,
                                 CACHE_WAY way,
                                 CACHE_LRU_LIST cur_lru,
                                 CACHE_LRU_LIST new_lru);
    action
        if ((getMRU(cur_lru) != way) || (cur_lru != new_lru))
            cacheDebug($fwrite(debugLog, "  Update LRU (way=0x%x) for %s: %b -> %b", way, access_type, cur_lru, new_lru));
        if (getMRU(new_lru) != way)
            cacheDebug($fwrite(debugLog, "  ***ERROR*** expected MRU to be 0x%x but it is 0x%x", way, getMRU(new_lru)));

    endaction
    endfunction


    // ***** Rules ***** //

    //
    // handleReq --
    //     Main entry point for load/store requests.
    //
    rule handleReq (!invalidatingAll);
        let req = link_memstate.getReq();
        link_memstate.deq();

        //
        // Valid request?
        //
        case (req) matches
            tagged MEM_LOAD  .a: noAction;
            tagged MEM_STORE .s: noAction;
            tagged MEM_INVALIDATE_CACHELINE .a: noAction;
            default: assertValidRequest(False);
        endcase

        let addr = case (req) matches
                      tagged MEM_LOAD  .a: a;
                      tagged MEM_STORE .s: s.addr;
                      tagged MEM_INVALIDATE_CACHELINE .a: a;
                   endcase;

        let set = cacheSet(addr);

        let req_kind = case (req) matches
                           tagged MEM_LOAD  .a: "LOAD";
                           tagged MEM_STORE .s: "STORE";
                           tagged MEM_INVALIDATE_CACHELINE .a: "INVAL";
                       endcase;
        cacheDebug($fwrite(debugLog, "New request: %s addr=0x%x, set=0x%x", req_kind, addr, set));

        if (enableCache)
        begin
            CACHE_ACCESS access;
            access.req = req;
            access.set = set;

            pendingQ.enq(access);

            // Read meta data and LRU hints
            cacheMeta.readReq(set);
            cacheLRU.readReq(set);
        end
        else
        begin
            //
            // Bypass cache
            //
            if (req matches tagged MEM_INVALIDATE_CACHELINE .a)
                noAction;
            else
                link_funcp_memory.makeReq(req);
        end
    endrule


    //
    // handleBypass --
    //     Only enabled when cache is OFF.  Route load results back to requester.
    //
    rule handleBypass (!enableCache &&& link_funcp_memory.getResp() matches tagged MEM_REPLY_LOAD .v);
        link_funcp_memory.deq();
        link_memstate.makeResp(v);
        cacheDebug($fwrite(debugLog, "Memory response (cache off): 0x%x", v));
    endrule


    //
    // handleInval --
    //     Invalidate a single line if it matches the tag.
    //
    rule handleInval (pendingQ.first.req matches tagged MEM_INVALIDATE_CACHELINE .addr);
        let set = pendingQ.first.set;
        let cur_lru = cacheLRU.readResp();
        let meta <- cacheMeta.readResp();

        if (findWayMatch(addr, meta) matches tagged Valid .inval_way)
        begin
            if (meta[inval_way] matches tagged Valid .m &&& m.dirty)
            begin
                flushDirtyLine.enq(tuple4(m.tag, set, inval_way, False));
                cacheData.readReq(getDataIdx(set, inval_way, 0));
            end

            meta[inval_way] = tagged Invalid;
            cacheMeta.write(set, meta);
        end
    endrule


    //
    // prepareFillVictim --
    //     Common code for loads and stores on a miss.  Pick a victim, flush
    //     existing data and request the fill from memory.  Returns the way
    //     to fill and whether the victim was currently invalid.
    //
    function ActionValue#(Tuple2#(CACHE_WAY, Bool)) prepareFillVictim(
                                                      MEM_ADDRESS addr,
                                                      CACHE_METADATA_VECTOR meta,
                                                      CACHE_SET set,
                                                      CACHE_LRU_LIST cur_lru,
                                                      Bool fill_dirty);
    actionvalue

        //
        // Miss.  First pick a victim.
        //
        CACHE_WAY fill_way = getLRU(cur_lru);
        Bool fill_invalid = False;
        if (findFirstInvalid(meta) matches tagged Valid .inval_way)
        begin
            fill_way = inval_way;
            fill_invalid = True;
        end

        // Request the new value from memory
        link_funcp_memory.makeReq(tagged MEM_LOAD_CACHELINE getLineAddr(addr));
            
        // Update LRU
        let new_lru = pushMRU(cur_lru, fill_way);
        cacheLRU.write(set, new_lru);

        if (meta[fill_way] matches tagged Valid .m &&& m.dirty)
        begin
            flushDirtyLine.enq(tuple4(m.tag, set, fill_way, True));
            cacheData.readReq(getDataIdx(set, fill_way, 0));
        end
        else
            fillVictim.enq(tuple2(set, fill_way));

        //
        // Update tag here for the filled line.  We don't wait until the
        // memory returns so we can stop passing around the meta data.
        //
        meta[fill_way] = tagged Valid CACHE_METADATA { tag : cacheTag(addr), dirty : fill_dirty };
        cacheMeta.write(set, meta);

        waiting <= True;

        cacheDebugLRUUpdate(fill_invalid ? "FILL Invalid" : "FILL LRU",
                            fill_way, cur_lru, new_lru);

        return tuple2(fill_way, fill_invalid);

    endactionvalue
    endfunction


    //
    // handleLoad --
    //     Load request.  Either return valid data from the cache or request
    //     the line from memory.
    //
    rule handleLoad (!waiting &&& pendingQ.first.req matches tagged MEM_LOAD .addr);
        let set = pendingQ.first.set;
        let cur_lru <- cacheLRU.readResp();
        let meta <- cacheMeta.readResp();

        if (findWayMatch(addr, meta) matches tagged Valid .way)
        begin
            //
            // Hit!  Now load the value from the cache.
            //
            let line_offset = cacheLineOffset(addr);
            cacheData.readReq(getDataIdx(set, way, line_offset));
            loadFromCache.enq(tuple2(way, line_offset));

            // Update LRU
            let new_lru = pushMRU(cur_lru, way);
            cacheLRU.write(set, new_lru);

            pendingQ.deq();

            statLoadHit.incr();
            cacheDebug($fwrite(debugLog, "Load HIT: addr=0x%x, set=0x%x, way=0x%x", addr, set, way));
            cacheDebugLRUUpdate("LOAD", way, cur_lru, new_lru);
        end
        else
        begin
            cacheDebug($fwrite(debugLog, "Load MISS: addr=0x%x, set=0x%x", addr, set));

            match { .fill_way, .fill_invalid }
                <- prepareFillVictim(addr, meta, set, cur_lru, False);

            statLoadMiss.incr();
            if (! fill_invalid)
                statLoadMissValidVictim.incr();
        end
    endrule


    //
    // loadCacheHit --
    //   Forward data coming from cache BRAM from handleLoad to back to the requester.
    //
    rule loadCacheHit (True);
        match {.way, .offset} = loadFromCache.first();
        loadFromCache.deq();

        let v <- cacheData.readResp();
        link_memstate.makeResp(v);

        cacheDebug($fwrite(debugLog, "  Load data=0x%x", v));
    endrule


    //
    // handleStore --
    //
    rule handleStore (!waiting &&& pendingQ.first.req matches tagged MEM_STORE .st_info);
        let addr = st_info.addr;
        let set = pendingQ.first.set;
        let cur_lru <- cacheLRU.readResp();
        let meta <- cacheMeta.readResp();

        if (findWayMatch(addr, meta) matches tagged Valid .way)
        begin
            //
            // Hit!
            //
            cacheData.write(getDataIdx(set, way, cacheLineOffset(addr)), st_info.val);
            if (writeBackCache())
            begin
                // Mark the line dirty
                let meta_way = fromMaybe(CACHE_METADATA{ tag: 0, dirty: False }, meta[way]);
                meta_way.dirty = True;
                meta[way] = tagged Valid meta_way;
                cacheMeta.write(set, meta);
            end
            else
            begin
                // Write through
                link_funcp_memory.makeReq(tagged MEM_STORE st_info);
            end

            // Update LRU
            let new_lru = pushMRU(cur_lru, way);
            cacheLRU.write(set, new_lru);

            pendingQ.deq();

            statStoreHit.incr();
            cacheDebug($fwrite(debugLog, "Store HIT: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", addr, set, way, st_info.val));
            cacheDebugLRUUpdate("STORE", way, cur_lru, new_lru);
        end
        else
        begin
            cacheDebug($fwrite(debugLog, "Store MISS: addr=0x%x, set=0x%x, data=0x%x", addr, set, st_info.val));

            match { .fill_way, .fill_invalid }
                <- prepareFillVictim(addr, meta, set, cur_lru, writeBackCache());

            statStoreMiss.incr();
            if (! fill_invalid)
                statStoreMissValidVictim.incr();
        end
    endrule


    rule handleFlushDirtySet (True);
        
        match {.set, .meta} = flushDirtySet.first();
        
        if (meta[flushWay] matches tagged Valid .m &&& m.dirty)
        begin
            cacheDebug($fwrite(debugLog, "handleFlushDirtySet: addr=0x%x, set=0x%x, way=0x%x", cacheAddr(m.tag, 0), set, flushWay));
            flushDirtyLine.enq(tuple4(m.tag, set, flushWay, False));
            cacheData.readReq(getDataIdx(set, flushWay, 0));
        end

        // Done with the set?
        if (flushWay == maxBound)
        begin
            // Done.  Pass the request on to the fill stage, if appropriate.
            flushDirtySet.deq();
        end

        flushWay <= flushWay + 1;

    endrule


    rule handleFlushDirtyWay (True);
        
        match {.tag, .set, .way, .reqFill} = flushDirtyLine.first();
        let v <- cacheData.readResp();

        // Done with the line?
        flushLineOffset <= flushLineOffset + 1;
        if (flushLineOffset == maxBound)
        begin
            // Done.  Pass the request on to the fill stage, if appropriate.
            flushDirtyLine.deq();
            if (reqFill)
                fillVictim.enq(tuple2(set, way));
            
            statDirtyLineFlush.incr();
        end
        else
        begin
            // Not done.  Request the next part of the line.
            cacheData.readReq(getDataIdx(set, way, flushLineOffset + 1));
        end

        let addr = cacheAddr(tag, flushLineOffset);
        link_funcp_memory.makeReq(tagged MEM_STORE { addr: addr, val: v});
        
        cacheDebug($fwrite(debugLog, "Write back DIRTY: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", addr, set, way, v));

    endrule


    //
    // handleFill --
    //    Update the cache with requested data coming back from memory.  This
    //    rule iterates through the individual register sized values within a
    //    single cache line.
    //
    rule handleFill (waiting &&& link_funcp_memory.getResp() matches tagged MEM_REPLY_LOAD_CACHELINE .v);

        match {.set, .way} = fillVictim.first();

        fillLineOffset <= fillLineOffset + 1;

        // Done with the line?
        if (fillLineOffset == maxBound)
        begin
            link_funcp_memory.deq();
            fillVictim.deq();
            waiting <= False;
            pendingQ.deq();
        end

        case (pendingQ.first.req) matches
            tagged MEM_LOAD .addr:
            begin
                // Cache the value
                cacheData.write(getDataIdx(set, way, fillLineOffset), v[fillLineOffset]);

                // Is this the portion the caller wanted?
                if (fillLineOffset == cacheLineOffset(addr))
                begin
                    link_memstate.makeResp(v[cacheLineOffset(addr)]);

                    cacheDebug($fwrite(debugLog, "Load FILL: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", addr, set, way, v));
                end
            end

            tagged MEM_STORE .st_info:
            begin
                let addr = st_info.addr;
                if (fillLineOffset != cacheLineOffset(addr))
                    cacheData.write(getDataIdx(set, way, fillLineOffset), v[fillLineOffset]);
                else
                begin
                    // Updated portion
                    cacheData.write(getDataIdx(set, way, fillLineOffset), st_info.val);

                    if (! writeBackCache())
                        link_funcp_memory.makeReq(tagged MEM_STORE st_info);

                    cacheDebug($fwrite(debugLog, "Store FILL: addr=0x%x, set=0x%x, way=0x%x, data=0x%x", addr, set, way, v));
                    cacheDebug($fwrite(debugLog, "Store WRITE: data=0x%x", st_info.val));
                end
            end
        endcase
    endrule


    //
    // invalidate --
    //     Handle requests from hybrid memory (software-side) to invalidate
    //     a specific address.
    //    
    rule invalidate (!invalidatingAll);
        MEM_ADDRESS addr = link_funcp_memory_inval.receive();
        link_funcp_memory_inval.deq();

        let set = cacheSet(addr);

        CACHE_ACCESS access;
        access.req = tagged MEM_INVALIDATE_CACHELINE addr;
        access.set = set;

        cacheLRU.readReq(set);
        cacheMeta.readReq(set);

        cacheDebug($fwrite(debugLog, "New request: INVAL addr=0x%x, set=0x%x", addr, set));
    endrule


    //
    // invalidate all --
    //     Invalidate entire cache using a FSM.
    //
    rule invalidate_all_start (!invalidatingAll && !waiting);
        cacheDebug($fwrite(debugLog, "New request: INVAL ALL"));
        link_funcp_memory_inval_all.deq();
        cacheMeta.readReq(0);
        invalidatingAll <= True;
    endrule

    rule invalidate_all (invalidatingAll);
        
        // Flush dirty lines
        let meta <- cacheMeta.readResp();
        flushDirtySet.enq(tuple2(invalidateAllSet, meta));

        cacheLRU.write(invalidateAllSet, Vector::genWith(fromInteger));
        cacheMeta.write(invalidateAllSet, Vector::replicate(tagged Invalid));

        if (invalidateAllSet == maxBound)
        begin
            invalidatingAll <= False;
            link_funcp_memory_inval_all_done.send(?);
            cacheDebug($fwrite(debugLog, "Request done: INVAL ALL"));
        end
        else
            cacheMeta.readReq(invalidateAllSet + 1);

        invalidateAllSet <= invalidateAllSet + 1;

    endrule


    //
    // Check configured parameters
    //
    rule check_param_sizes (True);
        //
        // CACHELINE_WORDS and FUNCP_CACHE_WAYS must be powers of 2
        //
        Bit#(32) one = 1;
        Bool cacheline_words_ok = (fromInteger(valueOf(CACHELINE_WORDS)) == (one << valueOf(cacheline_offset_SZ)));
        Bool cache_ways_ok = (`FUNCP_CACHE_WAYS == (one << valueOf(cache_way_SZ)));
        assertValidRequest(cacheline_words_ok && cache_ways_ok);
    endrule

endmodule
