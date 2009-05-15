//
// Copyright (C) 2009 Intel Corporation
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
// Interfaces to the central cache.
//

import FIFO::*;
import SpecialFIFOs::*;

`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/librl_bsv_cache.bsh"
`include "asim/provides/central_cache.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/provides/virtual_devices.bsh"

`include "asim/dict/PARAMS_PLATFORM_INTERFACE.bsh"
`include "asim/dict/STATS_PLATFORM_INTERFACE.bsh"

`include "asim/dict/VDEV.bsh"
`ifndef VDEV_CACHE__BASE
`define VDEV_CACHE__BASE 0
`endif


// ========================================================================
//
// Interface for any client of the central cache.
//
// ========================================================================

//
// Interface provided to clients of the central cache is identical to the
// standard direct mapped cache interface.  The n_ENTRIES parameter is the number
// of entries in a PRIVATE cache that will be allocated automatically in
// front of the central cache.  Setting n_ENTRIES to 0 instantiates a direct
// connection between the client and the central cache with no intermediate
// private cache.
//

typedef RL_DM_CACHE#(t_ADDR, t_DATA, t_REF_INFO, n_ENTRIES)
    CENTRAL_CACHE_CLIENT#(type t_ADDR, type t_DATA, type t_REF_INFO, numeric type n_ENTRIES);


//
// Interface for filling and spilling the cache.  An instance of this interface
// must be created by the client and passed to the central cache module.
//
interface CENTRAL_CACHE_CLIENT_BACKING#(type t_ADDR, type t_DATA, type t_REF_INFO);
    // Request a full line
    method Action readLineReq(t_ADDR addr, t_REF_INFO refInfo);

    // The read line response is pipelined.  For every readLineReq there must
    // be one readResp for every word in the requested line.  Cache entries
    // have CENTRAL_CACHE_WORDS_PER_LINE.  Low bits of the line are received
    // first.
    method ActionValue#(t_DATA) readResp();
    
    // Write to backing storage.  A write begins with a write request.
    // It is followed by multiple write data calls, one call per word
    // in a cache line.
    method Action writeLineReq(t_ADDR addr,
                               Vector#(CENTRAL_CACHE_WORDS_PER_LINE, Bool) wordValidMask,
                               t_REF_INFO refInfo,
                               Bool sendAck);

    // Called multiple times after a write request is received -- once for
    // each word in a line.  THIS METHOD WILL BE CALLED FOR A WORD EVEN
    // IF THE CONTROL INFORMATION SAYS THE WORD IS NOT VALID.  Low bits
    // of the line are sent first.
    method Action writeData(t_DATA val);

    // Ack from write request when sendAck is True
    method Action writeAckWait();
endinterface: CENTRAL_CACHE_CLIENT_BACKING


// ========================================================================
//
// Internal data structures.  Messages passed in soft connections between
// modules here and the platform interface.
//
// ========================================================================

//
// The request messages are already defined in the standard virtual device
// file (CENTRAL_CACHE_REQ).
//

//
// Union of all possible messages returning from the central cache.
//
typedef union tagged
{
    CENTRAL_CACHE_READ_RESP CENTRAL_CACHE_READ;
    Bool                    CENTRAL_CACHE_FLUSH_ACK;
}
CENTRAL_CACHE_RESP
    deriving (Eq, Bits);


//
// Backing storage channel
//

// Requests from cache to client
typedef union tagged
{
    CENTRAL_CACHE_BACKING_READ_REQ  CENTRAL_CACHE_BACK_READ;

    CENTRAL_CACHE_BACKING_WRITE_REQ CENTRAL_CACHE_BACK_WREQ;
    CENTRAL_CACHE_WORD              CENTRAL_CACHE_BACK_WDATA;
}
CENTRAL_CACHE_BACKING_REQ
    deriving (Eq, Bits);


// Responses from client to cache
typedef union tagged
{
    CENTRAL_CACHE_WORD CENTRAL_CACHE_BACK_READ;
    Bool               CENTRAL_CACHE_BACK_WACK;
}
CENTRAL_CACHE_BACKING_RESP
    deriving (Eq, Bits);


//
// Construct the name of the soft connection to a central cache port.
// Ports are created dynamically using dictionaries in the VDEV.CACHE
// name space.
//
function String cachePortName(Integer n) = "vdev_cache_" + integerToString(n - `VDEV_CACHE__BASE);
function String backingPortName(Integer n) = "vdev_cache_backing_" + integerToString(n - `VDEV_CACHE__BASE);


// ========================================================================
//
// Public interface to central cache.
//
// ========================================================================
    
//
// mkCentralCacheClient --
//     Make a client of the central cache.  The client defines its own address
//     and data types, which must fit in the central cache's containers.
//     The client interface uses only word-level addressing.  Methods that
//     refer to entire lines expect line-aligned addresses.
//
module [HASIM_MODULE] mkCentralCacheClient#(Integer cacheID,
                                            Bool hashLocalCacheAddrs,
                                            CENTRAL_CACHE_CLIENT_BACKING#(t_ADDR, t_DATA, t_REF_INFO) backing,
                                            RL_CACHE_STATS stats)
    // interface:
    (CENTRAL_CACHE_CLIENT#(t_ADDR, t_DATA, t_REF_INFO, n_ENTRIES))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_REF_INFO, t_REF_INFO_SZ),
       
              // data must fit in central cache space
              Bits#(CENTRAL_CACHE_WORD, t_CENTRAL_CACHE_WORD_SZ),
              Add#(extraDataBits, t_DATA_SZ, t_CENTRAL_CACHE_WORD_SZ),

              // refInfo must fit in central cache space
              Bits#(CENTRAL_CACHE_REF_INFO, t_CENTRAL_CACHE_REF_INFO_SZ),
              Add#(extraRefInfoBits, t_REF_INFO_SZ, t_CENTRAL_CACHE_REF_INFO_SZ),

              // Address space must fit in central cache addressing
              Bits#(CENTRAL_CACHE_LINE_ADDR, t_CENTRAL_CACHE_LINE_ADDR_SZ),
              Bits#(CENTRAL_CACHE_WORD_IDX, t_CENTRAL_CACHE_WORD_IDX_SZ),
              // Full central cache word-addressed space
              Add#(t_CENTRAL_CACHE_LINE_ADDR_SZ, t_CENTRAL_CACHE_WORD_IDX_SZ, t_CENTRAL_CACHE_WORD_ADDR_SZ),
              Add#(extraAddrBits, t_ADDR_SZ, t_CENTRAL_CACHE_WORD_ADDR_SZ));

    DEBUG_FILE debugLog <- mkDebugFile("memory_central_cache_" + integerToString(cacheID - `VDEV_CACHE__BASE) + ".out");

    //
    // Allocate the connection between a private cache and the central cache.
    //
    RL_DM_CACHE_SOURCE_DATA#(t_ADDR, t_DATA, t_REF_INFO) centralCacheConnection <-
        mkCentralCacheConnection(cacheID, backing, debugLog);

    RL_DM_CACHE#(t_ADDR, t_DATA, t_REF_INFO, n_ENTRIES) pvtCache;
    if (valueOf(n_ENTRIES) == 0)
        pvtCache <- mkNullCacheDirectMapped(centralCacheConnection, debugLog);
    else
        pvtCache <- mkCacheDirectMapped(centralCacheConnection, hashLocalCacheAddrs, stats, debugLog);
    
    return pvtCache;
endmodule



// ========================================================================
//
// Internal modules
//
// ========================================================================
    
//
// mkCentralCacheConnection --
//     Internal module connecting the backing store interface of a direct
//     mapped cache to the central cache.  The central cache client interface
//     above allocates a direct mapped private cache and uses this module
//     to make the connection to the central cache.
//
module [HASIM_MODULE] mkCentralCacheConnection#(Integer cacheID,
                                                CENTRAL_CACHE_CLIENT_BACKING#(t_ADDR, t_DATA, t_REF_INFO) backing,
                                                DEBUG_FILE debugLog)
    // interface:
    (RL_DM_CACHE_SOURCE_DATA#(t_ADDR, t_DATA, t_REF_INFO))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_REF_INFO, t_REF_INFO_SZ),
       
              // data must fit in central cache space
              Bits#(CENTRAL_CACHE_WORD, t_CENTRAL_CACHE_WORD_SZ),
              Add#(extraDataBits, t_DATA_SZ, t_CENTRAL_CACHE_WORD_SZ),

              // refInfo must fit in central cache space
              Bits#(CENTRAL_CACHE_REF_INFO, t_CENTRAL_CACHE_REF_INFO_SZ),
              Add#(extraRefInfoBits, t_REF_INFO_SZ, t_CENTRAL_CACHE_REF_INFO_SZ),

              // Address space must fit in central cache addressing
              Bits#(CENTRAL_CACHE_LINE_ADDR, t_CENTRAL_CACHE_LINE_ADDR_SZ),
              Bits#(CENTRAL_CACHE_WORD_IDX, t_CENTRAL_CACHE_WORD_IDX_SZ),
              // Full central cache word-addressed space
              Add#(t_CENTRAL_CACHE_LINE_ADDR_SZ, t_CENTRAL_CACHE_WORD_IDX_SZ, t_CENTRAL_CACHE_WORD_ADDR_SZ),
              Add#(extraAddrBits, t_ADDR_SZ, t_CENTRAL_CACHE_WORD_ADDR_SZ));

    Connection_Client#(CENTRAL_CACHE_REQ, CENTRAL_CACHE_RESP) link_cache <- mkConnection_Client(cachePortName(cacheID));
    Connection_Server#(CENTRAL_CACHE_BACKING_REQ, CENTRAL_CACHE_BACKING_RESP) link_cache_backing <- mkConnection_Server(backingPortName(cacheID));


    function Tuple2#(CENTRAL_CACHE_LINE_ADDR, CENTRAL_CACHE_WORD_IDX) addrToCentralAddr(t_ADDR addr);
        return unpack(zeroExtend(pack(addr)));
    endfunction

    function t_ADDR centralAddrToAddr(CENTRAL_CACHE_LINE_ADDR lAddr, CENTRAL_CACHE_WORD_IDX wIdx);
        return unpack(truncate({lAddr, wIdx}));
    endfunction


    // ====================================================================
    //
    // Backing storage requests and responses
    //
    // ====================================================================

    rule backingReadLineReq (link_cache_backing.getReq() matches tagged CENTRAL_CACHE_BACK_READ .req);
        link_cache_backing.deq();

        let addr = centralAddrToAddr(req.addr, 0);
        debugLog.record($format("backReadLineReq: addr=0x%x, l_addr=0x%x", addr, req.addr));

        backing.readLineReq(addr, unpack(truncate(req.refInfo)));
    endrule

    rule backingReadResp (True);
        let val <- backing.readResp();
        debugLog.record($format("backReadResp: val=0x%x", val));

        link_cache_backing.makeResp(tagged CENTRAL_CACHE_BACK_READ zeroExtend(pack(val)));
    endrule
    
    rule backingWriteLineReq (link_cache_backing.getReq() matches tagged CENTRAL_CACHE_BACK_WREQ .req);
        link_cache_backing.deq();
        let addr = centralAddrToAddr(req.addr, 0);

        debugLog.record($format("backWrite: addr=0x%x, l_addr=0x%x, valid=0x%x", addr, req.addr, req.wordValidMask));

        backing.writeLineReq(addr, req.wordValidMask, unpack(truncate(req.refInfo)), req.sendAck);
    endrule

    rule backingWriteData (link_cache_backing.getReq() matches tagged CENTRAL_CACHE_BACK_WDATA .val);
        link_cache_backing.deq();

        debugLog.record($format("backWriteData: val=0x%x", val));

        backing.writeData(unpack(truncate(val)));
    endrule

    //
    // backingWriteAck --
    //     Response from backing storage that a write has arrived.  The responses
    //     returns up the cache hierarchy from the bottom to the top.
    //
    (* descending_urgency = "backingWriteAck, backingReadResp" *)
    rule backingWriteAck (True);
        backing.writeAckWait();
        debugLog.record($format("backWriteAck"));

        link_cache_backing.makeResp(tagged CENTRAL_CACHE_BACK_WACK False);
    endrule


    // ====================================================================
    //
    // Cache request methods
    //
    // ====================================================================

    // Read request
    method Action readReq(t_ADDR addr, t_REF_INFO refInfo);
        match {.l_addr, .w_idx} = addrToCentralAddr(addr);
        let r = CENTRAL_CACHE_READ_REQ { addr: l_addr,
                                         wordIdx: w_idx,
                                         refInfo: zeroExtend(pack(refInfo))};

        link_cache.makeReq(tagged CENTRAL_CACHE_READ r);
        debugLog.record($format("readReq: addr=0x%x, l_addr=0x%x, wIdx=%0d", addr, l_addr, w_idx));
    endmethod

    // Read response
    method ActionValue#(RL_DM_CACHE_FILL_RESP#(t_ADDR, t_DATA, t_REF_INFO)) readResp() if (link_cache.getResp() matches tagged CENTRAL_CACHE_READ .resp);
        link_cache.deq();

        let addr = centralAddrToAddr(resp.addr, resp.wordIdx);
        t_DATA val = unpack(truncate(resp.val));
        let r = RL_DM_CACHE_FILL_RESP { addr: addr,
                                        val: val,
                                        refInfo: unpack(truncate(resp.refInfo)) };

        debugLog.record($format("readResp: addr=0x%x, val=0x%x", addr, val));

        return r;
    endmethod


    // Write request
    method Action write(t_ADDR addr, t_DATA val, t_REF_INFO refInfo);
        match {.l_addr, .w_idx} = addrToCentralAddr(addr);
        let r = CENTRAL_CACHE_WRITE_REQ { addr: l_addr,
                                          wordIdx: w_idx,
                                          val: zeroExtend(pack(val)),
                                          refInfo: zeroExtend(pack(refInfo))};

        link_cache.makeReq(tagged CENTRAL_CACHE_WRITE r);
        debugLog.record($format("write: addr=0x%x, l_addr=0x%x, wIdx=%0d, val=0x%x", addr, l_addr, w_idx, val));
    endmethod


    // Line invalidation request
    method Action invalReq(t_ADDR addr, Bool sendAck, t_REF_INFO refInfo);
        match {.l_addr, .w_idx} = addrToCentralAddr(addr);
        let r = CENTRAL_CACHE_INVAL_REQ { addr: l_addr,
                                          sendAck: sendAck,
                                          refInfo: zeroExtend(pack(refInfo)) };

        link_cache.makeReq(tagged CENTRAL_CACHE_INVAL r);
        debugLog.record($format("inval: addr=0x%x, l_addr=0x%x, ack=%0d", addr, l_addr, sendAck));
    endmethod

    // Line flush request
    method Action flushReq(t_ADDR addr, Bool sendAck, t_REF_INFO refInfo);
        match {.l_addr, .w_idx} = addrToCentralAddr(addr);
        let r = CENTRAL_CACHE_INVAL_REQ { addr: l_addr,
                                          sendAck: sendAck,
                                          refInfo: zeroExtend(pack(refInfo)) };

        link_cache.makeReq(tagged CENTRAL_CACHE_FLUSH r);
        debugLog.record($format("flush: addr=0x%x, l_addr=0x%x, ack=%0d", addr, l_addr, sendAck));
    endmethod

    // Line inval/flush response (blocks until ACK arrives)
    method Action invalOrFlushWait() if (link_cache.getResp() matches tagged CENTRAL_CACHE_FLUSH_ACK .dummy);
        link_cache.deq();
        debugLog.record($format("flush/inval: ACK"));
    endmethod
endmodule
    
    
// ===================================================================
//
// STATISTICS INTERFACE
//
// mkCentralCacheStats --
//     Statistics callbacks from central cache class.
//
// ===================================================================

module [HASIM_MODULE] mkCentralCacheStats
    // interface:
    (CENTRAL_CACHE_STATS);
    
    Vector#(9, STATS_DICT_TYPE) statIDs = newVector();

    statIDs[0] = `STATS_PLATFORM_INTERFACE_CACHE_LOAD_HIT;
    let statLoadHit = 0;

    statIDs[1] = `STATS_PLATFORM_INTERFACE_CACHE_LOAD_MISS;
    let statLoadMiss = 1;

    statIDs[2] = `STATS_PLATFORM_INTERFACE_CACHE_STORE_HIT;
    let statStoreHit  = 2;

    statIDs[3] = `STATS_PLATFORM_INTERFACE_CACHE_STORE_MISS;
    let statStoreMiss = 3;

    statIDs[4] = `STATS_PLATFORM_INTERFACE_CACHE_INVAL_LINE;
    let statInvalEntry = 4;

    statIDs[5] = `STATS_PLATFORM_INTERFACE_CACHE_DIRTY_LINE_FLUSH;
    let statDirtyEntryFlush = 5;

    statIDs[6] = `STATS_PLATFORM_INTERFACE_CACHE_FORCE_INVAL_LINE;
    let statForceInvalLine = 6;

    statIDs[7] = `STATS_PLATFORM_INTERFACE_CACHE_RECENT_LINE_HIT;
    let statRecentLineHit = 7;

    statIDs[8] = `STATS_PLATFORM_INTERFACE_CACHE_RECENT_LINE_MISS;
    let statRecentLineMiss = 8;

    let stats <- mkStatCounter_Vector(statIDs);

    interface RL_CACHE_STATS cacheStats;
        method Action readHit();
            stats.incr(statLoadHit);
        endmethod

        method Action readMiss();
            stats.incr(statLoadMiss);
        endmethod

        method Action writeHit();
            stats.incr(statStoreHit);
        endmethod

        method Action writeMiss();
            stats.incr(statStoreMiss);
        endmethod

        method Action invalEntry();
            stats.incr(statInvalEntry);
        endmethod

        method Action dirtyEntryFlush();
            stats.incr(statDirtyEntryFlush);
        endmethod

        method Action forceInvalLine();
            stats.incr(statForceInvalLine);
        endmethod
    endinterface

    interface CENTRAL_CACHE_RECENT_LINE_STATS recentLineStats;
        method Action readHit();
            stats.incr(statRecentLineHit);
        endmethod

        method Action readMiss();
            stats.incr(statRecentLineMiss);
        endmethod
    endinterface
endmodule
