///
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
// Interfaces to scratchpad memory.
//

import FIFO::*;
import SpecialFIFOs::*;


`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/librl_bsv_storage.bsh"
`include "asim/provides/librl_bsv_cache.bsh"
`include "asim/provides/scratchpad_memory.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/dict/PARAMS_SCRATCHPAD_MEMORY_SERVICE.bsh"
`include "asim/dict/DEBUG_SCAN_SCRATCHPAD_MEMORY_SERVICE.bsh"

`include "asim/dict/VDEV.bsh"
`ifndef VDEV_SCRATCH__BASE
`define VDEV_SCRATCH__BASE 0
`endif


//
// Scratchpad cache interface is a basic memory interface with an extra
// parameter controlling the cache size.
//
typedef MEMORY_IFC#(t_ADDR, t_DATA)
    SCRATCHPAD_MEMORY_IFC#(type t_ADDR, type t_DATA, numeric type n_CACHE_ENTRIES);

typedef MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA)
    SCRATCHPAD_MEMORY_MULTI_READ_IFC#(numeric type n_READERS,
                                      type t_ADDR,
                                      type t_DATA,
                                      numeric type n_CACHE_ENTRIES);

//
// Caching options for scratchpads.  The caching option also affects the way
// data structures are marshalled to scratchpad containers.
//
typedef enum
{
    // Fully cached.  Elements are packed tightly in scratchpad containers.
    SCRATCHPAD_CACHED,

    // No private L1 cache, but data may be stored in a shared, central cache.
    SCRATCHPAD_NO_PVT_CACHE,

    // Raw, right to memory.  Elements are aligned to natural sizes within
    // a scratchpad container.  (E.g. 1, 2, 4 or 8 bytes.)  Byte masks are
    // used on writes to avoid requiring read/modify/write.  The current
    // implementation supports object sizes up to the size of a
    // SCRATCHPAD_MEM_VALUE.
    SCRATCHPAD_UNCACHED
}
SCRATCHPAD_CACHE_MODE
    deriving (Eq, Bits);


//
// Data structures flowing through soft connections between scratchpad clients
// and the platform interface.
//

typedef struct
{
    SCRATCHPAD_PORT_NUM port;
    SCRATCHPAD_MEM_ADDRESS allocLastWordIdx;
    Bool cached;
}
SCRATCHPAD_INIT_REQ
    deriving (Eq, Bits);

typedef struct
{
    SCRATCHPAD_PORT_NUM port;
    SCRATCHPAD_MEM_ADDRESS addr;
    SCRATCHPAD_MEM_MASK byteReadMask;
    SCRATCHPAD_CLIENT_REF_INFO clientRefInfo;
}
SCRATCHPAD_READ_REQ
    deriving (Eq, Bits);

typedef struct
{
    SCRATCHPAD_PORT_NUM port;
    SCRATCHPAD_MEM_ADDRESS addr;
    SCRATCHPAD_MEM_VALUE val;
}
SCRATCHPAD_WRITE_REQ
    deriving (Eq, Bits);

typedef struct
{
    SCRATCHPAD_PORT_NUM port;
    SCRATCHPAD_MEM_ADDRESS addr;
    SCRATCHPAD_MEM_VALUE val;
    SCRATCHPAD_MEM_MASK byteWriteMask;
}
SCRATCHPAD_WRITE_MASKED_REQ
    deriving (Eq, Bits);


//
// Scratchpad requests (either a load or a store) from the client to the
// server.
//
typedef union tagged 
{
    SCRATCHPAD_INIT_REQ           SCRATCHPAD_MEM_INIT;

    SCRATCHPAD_READ_REQ           SCRATCHPAD_MEM_READ;
    SCRATCHPAD_WRITE_REQ          SCRATCHPAD_MEM_WRITE;
    SCRATCHPAD_WRITE_MASKED_REQ   SCRATCHPAD_MEM_WRITE_MASKED;
}
SCRATCHPAD_MEM_REQ
    deriving (Eq, Bits);


//
// Scratchpad read response.
//
typedef struct
{
    SCRATCHPAD_MEM_VALUE val;
    SCRATCHPAD_MEM_ADDRESS addr;
    SCRATCHPAD_CLIENT_REF_INFO clientRefInfo;
}
SCRATCHPAD_READ_RSP
    deriving (Eq, Bits);


// Number of slots in a read port's reorder buffer.  The scratchpad subsystem
// does not guarantee to return results in order, so all clients need a ROB.
// The ROB size limits the number of read requests in flight for a given port.
typedef 8 SCRATCHPAD_PORT_ROB_SLOTS;

// The uncached scratchpad will have more references outstanding due to latency.
// Allow more references to be in flight.
typedef 128 SCRATCHPAD_UNCACHED_PORT_ROB_SLOTS;

//
// Scratchpad ports must be unique and non-zero.  Port 0 is the server.
//
function SCRATCHPAD_PORT_NUM scratchpadPortId(Integer n);
    return fromInteger(n - `VDEV_SCRATCH__BASE + 1);
endfunction


// ========================================================================
//
// Modules that instantiate a scratchpad memory.
//
// ========================================================================
    
//
// mkScratchpad --
//     This is the typical scratchpad module.
//
//     Build a scratchpad of an arbitrary data type with marshalling to the
//     global scratchpad base memory size.
//
module [CONNECTED_MODULE] mkScratchpad#(Integer scratchpadID,
                                        SCRATCHPAD_CACHE_MODE cached)
    // interface:
    (MEMORY_IFC#(t_ADDR, t_DATA))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ));

    //
    // The scratchpad implementation is all in the multi-reader interface.
    // Allocate a multi-reader scratchpad with a single reader and convert
    // it to MEMORY_IFC.
    //

    MEMORY_MULTI_READ_IFC#(1, t_ADDR, t_DATA) m_scratch <- mkMultiReadScratchpad(scratchpadID, cached);
    MEMORY_IFC#(t_ADDR, t_DATA) scratch <- mkMultiMemIfcToMemIfc(m_scratch);
    return scratch;
endmodule

//
// mkMultiReadScratchpad --
//     The same as mkMultiReadStatScratchpad but we have null stats in this case
//
module [CONNECTED_MODULE] mkMultiReadScratchpad#(Integer scratchpadID,
                                                 SCRATCHPAD_CACHE_MODE cached)
    // interface:
    (MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ));

    let m <- mkMultiReadStatsScratchpad(scratchpadID, cached, mkNullScratchpadCacheStats);
    return m;
endmodule

//
// mkMultiReadStatsScratchpad --
//     The same as a normal mkScratchpad but with multiple read ports.
//     Requests are processed in order, with reads being scheduled before
//     a write requested in the same cycle.
//
module [CONNECTED_MODULE] mkMultiReadStatsScratchpad#(Integer scratchpadID,
                                                      SCRATCHPAD_CACHE_MODE cached,
                                                      SCRATCHPAD_STATS_CONSTRUCTOR statsConstructor
)
    // interface:
    (MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ),

              // Compute container index type (size)
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_ADDR_SZ, t_DATA_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_ADDR),
              Bits#(t_CONTAINER_ADDR, t_CONTAINER_ADDR_SZ));

    if (cached == SCRATCHPAD_UNCACHED)
    begin
        // No caches at any level.  This access pattern uses masked writes to
        // avoid read-modify-write loops when accessing objects smaller than
        // a scratchpad base data size.
        MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA) memory <- mkUncachedScratchpad(scratchpadID);

        return memory;
    end
    else if ((cached == SCRATCHPAD_CACHED) &&
             (valueOf(TExp#(t_CONTAINER_ADDR_SZ)) <= `SCRATCHPAD_STD_PVT_CACHE_ENTRIES))
    begin
        // A special case:  cached scratchpad requested but the container
        // is smaller than the cache would have been.  Just allocate a BRAM.
        MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA) memory <- mkBRAMBufferedPseudoMultiReadInitialized(unpack(0));
        return memory;
    end
    else
    begin
        // Container maps requested data size to the platform's scratchpad
        // word size.
        SCRATCHPAD_MEMORY_MULTI_READ_IFC#(n_READERS, t_CONTAINER_ADDR, SCRATCHPAD_MEM_VALUE, `SCRATCHPAD_STD_PVT_CACHE_ENTRIES) containerMemory;
        if (cached == SCRATCHPAD_CACHED)
            containerMemory <- mkUnmarshalledCachedScratchpad(scratchpadID, `PARAMS_SCRATCHPAD_MEMORY_SERVICE_SCRATCHPAD_PVT_CACHE_MODE, statsConstructor);
        else
            containerMemory <- mkUnmarshalledScratchpad(scratchpadID);
        
        // Wrap the container with a marshaller.
        let memory <- mkMemPackMultiRead(containerMemory);

        return memory;
    end
endmodule


// ========================================================================
//
// Heaps layered on scratchpad memory
//
// ========================================================================


//
// mkMemoryHeapUnionScratchpad --
//     Data and free list share same storage in a scratchpad memory.
//
module [CONNECTED_MODULE] mkMemoryHeapUnionScratchpad#(Integer scratchpadID,
                                                       SCRATCHPAD_CACHE_MODE cached)
    // interface:
    (MEMORY_HEAP#(t_INDEX, t_DATA))
    provisos (Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_INDEX, t_INDEX_SZ));

    MEMORY_HEAP_DATA#(t_INDEX, t_DATA) pool <- mkMemoryHeapUnionScratchpadStorage(scratchpadID, cached);
    MEMORY_HEAP#(t_INDEX, t_DATA) heap <- mkMemoryHeap(pool);

    return heap;
endmodule


//
// mkMemoryHeapUnionScratchpadStorage --
//     Backing storage for a memory heap where the data and free list are
//     stored in the same, unioned, scratchpad memory.
//
module [CONNECTED_MODULE] mkMemoryHeapUnionScratchpadStorage#(Integer scratchpadID,
                                                              SCRATCHPAD_CACHE_MODE cached)
    // interface:
    (MEMORY_HEAP_DATA#(t_INDEX, t_DATA))
    provisos (Bits#(t_INDEX, t_INDEX_SZ),
              Bits#(t_DATA, t_DATA_SZ),
              Max#(t_INDEX_SZ, t_DATA_SZ, t_UNION_SZ));

    MEMORY_MULTI_READ_IFC#(2, t_INDEX, Bit#(t_UNION_SZ)) pool <- mkMultiReadScratchpad(scratchpadID, cached);

    //
    // You might think that because backing storage and the free list use
    // independent scratchpad ports they would need no flow control.  You would
    // be wrong.  They only have separate read ports.  Because the write port
    // is shared, it would be possible for reads and writes to get out of order.
    //
    // These wires are used to block backing store I/O when there is free list
    // traffic.
    //

    Wire#(Bool) freeListReadReqFired <- mkDWire(False);
    Wire#(Bool) freeListWriteFired <- mkDWire(False);

    interface MEMORY_HEAP_BACKING_STORE data;
        //
        // Free list traffic gets priority over backing store I/O.  See
        // the description of the control wires above.
        //
        method Action readReq(t_INDEX addr) if (! freeListReadReqFired &&
                                                ! freeListWriteFired);
            pool.readPorts[1].readReq(addr);
        endmethod

        method ActionValue#(t_DATA) readRsp();
            let r <- pool.readPorts[1].readRsp();
            return unpack(truncateNP(r));
        endmethod

        method Action write(t_INDEX addr, t_DATA value) if (! freeListReadReqFired &&
                                                            ! freeListWriteFired);
            pool.write(addr, zeroExtendNP(pack(value)));
        endmethod
    endinterface

    //
    // The free list must use port 0 to avoid deadlocks.  For the many to 1
    // packed memory case (mkMemPackManyTo1) read port 0 is shared between
    // the client and internal logic implementing read-modify-write for writes.
    // If the client backs up reading port 0 then stores block.  The free list
    // client guarantees not to request a read without being able to consume
    // it and, consequently, avoids this deadlock.
    //
    interface MEMORY_HEAP_BACKING_STORE freeList;
        method Action readReq(t_INDEX addr);
            freeListReadReqFired <= True;
            pool.readPorts[0].readReq(addr);
        endmethod

        method ActionValue#(t_INDEX) readRsp();
            let r <- pool.readPorts[0].readRsp();
            return unpack(truncateNP(r));
        endmethod

        method Action write(t_INDEX addr, t_INDEX value);
            freeListWriteFired <= True;
            pool.write(addr, zeroExtendNP(pack(value)));
        endmethod
    endinterface
endmodule
    
    
    
// ========================================================================
//
// Internal modules
//
// ========================================================================
    
    
//
// mkUnmarshalledScratchpad --
//     Allocate a connection to the platform's scratchpad interface for
//     a single scratchpad region.  This module does no marshalling of
//     data sizes or caching.  BEWARE: the word size of the virtual
//     platform's scratchpad is platform dependent.
//
module [CONNECTED_MODULE] mkUnmarshalledScratchpad#(Integer scratchpadID)
    // interface:
    (MEMORY_MULTI_READ_IFC#(n_READERS, t_MEM_ADDRESS, SCRATCHPAD_MEM_VALUE))
    provisos (Bits#(t_MEM_ADDRESS, t_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),

              // Compute a non-zero size for the read port index
              Max#(n_READERS, 2, n_SAFE_READERS),
              Log#(n_SAFE_READERS, n_SAFE_READERS_SZ),

              // Index in a reorder buffer
              Alias#(SCOREBOARD_FIFO_ENTRY_ID#(SCRATCHPAD_PORT_ROB_SLOTS), t_REORDER_ID),

              // Reference info passed to the scratchpad needed to route the response
              Alias#(Tuple2#(Bit#(n_SAFE_READERS_SZ), t_REORDER_ID), t_REF_INFO));
    
    if (valueOf(t_MEM_ADDRESS_SZ) > valueOf(t_SCRATCHPAD_MEM_ADDRESS_SZ))
    begin
        error("Scratchpad ID " + integerToString(scratchpadID) + " address is too large: " + integerToString(valueOf(t_MEM_ADDRESS_SZ)) + " bits");
    end

    String debugLogFilename = "memory_scratchpad_" + integerToString(scratchpadID - `VDEV_SCRATCH__BASE + 1) + ".out";
    DEBUG_FILE debugLog <- (`PLATFORM_SCRATCHPAD_DEBUG_ENABLE == 1)?
                           mkDebugFile(debugLogFilename):
                           mkDebugFileNull(debugLogFilename); 

    let my_port = scratchpadPortId(scratchpadID);

    Connection_TokenRing#(SCRATCHPAD_PORT_NUM, SCRATCHPAD_MEM_REQ) link_mem_req <-
        mkConnection_TokenRingNode(`RINGID_SCRATCHPAD_MEMORY_REQ, my_port);

    Connection_TokenRing#(SCRATCHPAD_PORT_NUM, SCRATCHPAD_READ_RSP) link_mem_rsp <-
        mkConnection_TokenRingNode(`RINGID_SCRATCHPAD_MEMORY_RSP, my_port);

    // Scratchpad responses are not ordered.  Sort them with a reorder buffer.
    // Each read port gets its own reorder buffer so that each port returns data
    // when available, independent of the latency of requests on other ports.
    Vector#(n_READERS, SCOREBOARD_FIFOF#(SCRATCHPAD_PORT_ROB_SLOTS, SCRATCHPAD_MEM_VALUE)) sortResponseQ <- replicateM(mkScoreboardFIFOF());

    // Merge FIFOF combines read and write requests in temporal order,
    // with reads from the same cycle as a write going first.  Each read port
    // gets a slot.  The write port is always last.
    MERGE_FIFOF#(TAdd#(n_READERS, 1), t_MEM_ADDRESS) incomingReqQ <- mkMergeBypassFIFOF();

    // Write data is sent in a side port to keep the incomingReqQ smaller.
    FIFO#(SCRATCHPAD_MEM_VALUE) writeDataQ <- mkBypassFIFO();

    Reg#(Bool) initialized <- mkReg(False);
    
    //
    // Allocate memory for this scratchpad region
    //
    rule doInit (! initialized);
        initialized <= True;

        Bit#(t_MEM_ADDRESS_SZ) alloc = maxBound;
        SCRATCHPAD_INIT_REQ r;
        r.allocLastWordIdx = zeroExtendNP(alloc);
        r.cached = True;
        r.port = my_port;
        link_mem_req.enq(0, tagged SCRATCHPAD_MEM_INIT r);
    endrule

    //
    // Forward merged requests to the memory.
    //

    // Read requests
    rule forwardReadReq (initialized && (incomingReqQ.firstPortID() < fromInteger(valueOf(n_READERS))));
        let port = incomingReqQ.firstPortID();
        let addr = incomingReqQ.first();
        incomingReqQ.deq();
        
        // Allocate a slot in the reorder buffer for the read request.  Each
        // read port gets its own reorder buffer.
        let idx <- sortResponseQ[port].enq();

        // The clientRefInfo for this request is the concatenation of the
        // port ID and the ROB index.
        t_REF_INFO ref_info = unpack(truncateNP({ port, idx }));

        let req = SCRATCHPAD_READ_REQ { port: my_port,
                                        addr: zeroExtendNP(pack(addr)),
                                        byteReadMask: replicate(True),
                                        clientRefInfo: zeroExtendNP(pack(ref_info)) };

        link_mem_req.enq(0, tagged SCRATCHPAD_MEM_READ req);
    endrule

    // Write requests
    rule forwardWriteReq (initialized && (incomingReqQ.firstPortID() == fromInteger(valueOf(n_READERS))));
        let addr = incomingReqQ.first();
        incomingReqQ.deq();
        
        let val = writeDataQ.first();
        writeDataQ.deq();

        let req = SCRATCHPAD_WRITE_REQ { port: my_port,
                                         addr: zeroExtendNP(pack(addr)),
                                         val: val };

        link_mem_req.enq(0, tagged SCRATCHPAD_MEM_WRITE req);
    endrule

    //
    // receiveResp --
    //     Push unordered read responses to the reorder buffers.  Responses will
    //     be returned through readRsp() in order.
    //
    rule receiveResp (True);
        let s = link_mem_rsp.first();
        link_mem_rsp.deq();

        // The clientRefInfo field holds the concatenation of the port ID and
        // the port's reorder buffer index.
        t_REF_INFO port_idx = unpack(truncateNP(s.clientRefInfo));
        match {.port, .idx} = port_idx;

        sortResponseQ[port].setValue(idx, s.val);
    endrule


    //
    // Methods.  All requests are stored in the incomingReqQ to maintain their
    // order.
    //

    Vector#(n_READERS, MEMORY_READER_IFC#(t_MEM_ADDRESS, SCRATCHPAD_MEM_VALUE)) portsLocal = newVector();

    for(Integer p = 0; p < valueOf(n_READERS); p = p + 1)
    begin
        portsLocal[p] =
            interface MEMORY_READER_IFC#(t_ADDR, t_DATA);
                method Action readReq(t_MEM_ADDRESS addr);
                    incomingReqQ.ports[p].enq(addr);
                    debugLog.record($format("read port %0d: req addr=0x%x", p, addr));
                endmethod

                method ActionValue#(SCRATCHPAD_MEM_VALUE) readRsp();
                    let r = sortResponseQ[p].first();
                    sortResponseQ[p].deq();

                    debugLog.record($format("read port %0d: resp val=0x%x", p, r));
                    return r;
                endmethod

                method SCRATCHPAD_MEM_VALUE peek();
                    return sortResponseQ[p].first();
                endmethod

                method Bool notEmpty() = sortResponseQ[p].notEmpty();
                method Bool notFull() = incomingReqQ.ports[p].notFull();
            endinterface;
    end

    interface readPorts = portsLocal;

    method Action write(t_MEM_ADDRESS addr, SCRATCHPAD_MEM_VALUE val);
        // The write port is last in the merge FIFO
        incomingReqQ.ports[valueOf(n_READERS)].enq(addr);
        writeDataQ.enq(val);
        debugLog.record($format("write addr=0x%x, val=0x%x", addr, val));
    endmethod

    method Bool writeNotFull = incomingReqQ.ports[valueOf(n_READERS)].notFull();
endmodule
    
    
//
// mkUnmarshalledCachedScratchpad --
//     Allocate a cached connection to the platform's scratchpad interface for
//     a single scratchpad region.  This module does no marshalling of
//     data sizes.
//
module [CONNECTED_MODULE] mkUnmarshalledCachedScratchpad#(Integer scratchpadID, 
                                                          Integer cacheModeParam, 
                                                          SCRATCHPAD_STATS_CONSTRUCTOR statsConstructor)
    // interface:
    (SCRATCHPAD_MEMORY_MULTI_READ_IFC#(n_READERS, t_MEM_ADDRESS, SCRATCHPAD_MEM_VALUE, n_CACHE_ENTRIES))
    provisos (Bits#(t_MEM_ADDRESS, t_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),

              // Compute a non-zero size for the read port index
              Max#(n_READERS, 2, n_SAFE_READERS),
              Log#(n_SAFE_READERS, n_SAFE_READERS_SZ),

              // Index in a reorder buffer
              Alias#(SCOREBOARD_FIFO_ENTRY_ID#(SCRATCHPAD_PORT_ROB_SLOTS), t_REORDER_ID),
       
              // Reference info passed to the cache needed to route the response
              Alias#(Tuple2#(Bit#(n_SAFE_READERS_SZ), t_REORDER_ID), t_REF_INFO));

    String debugLogFilename = "platform_scratchpad_" + integerToString(scratchpadID - `VDEV_SCRATCH__BASE + 1) + ".out";
    DEBUG_FILE debugLog <- (`PLATFORM_SCRATCHPAD_DEBUG_ENABLE == 1)?
                           mkDebugFile(debugLogFilename):
                           mkDebugFileNull(debugLogFilename); 

    // Dynamic parameters
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();
    Param#(2) cacheMode <- mkDynamicParameter(fromInteger(cacheModeParam), paramNode);

    // Connection between private cache and the scratchpad virtual device
    RL_DM_CACHE_SOURCE_DATA#(Bit#(t_MEM_ADDRESS_SZ),
                             SCRATCHPAD_MEM_VALUE,
                             t_REF_INFO) sourceData <- mkScratchpadCacheSourceData(scratchpadID);


    // Private cache
    NumTypeParam#(n_CACHE_ENTRIES) num_cache_entries = ?;
    RL_DM_CACHE#(Bit#(t_MEM_ADDRESS_SZ),
                       SCRATCHPAD_MEM_VALUE,
                       t_REF_INFO) cache <- mkCacheDirectMapped(sourceData,
                                                                num_cache_entries,
                                                                True,
                                                                debugLog);

    // Hook up stats
    let cacheStats <- statsConstructor(cache.stats);
    // Merge FIFOF combines read and write requests in temporal order,
    // with reads from the same cycle as a write going first.  Each read port
    // gets a slot.  The write port is always last.
    MERGE_FIFOF#(TAdd#(n_READERS, 1), t_MEM_ADDRESS) incomingReqQ <- mkMergeFIFOF();

    // Write data is sent in a side port to keep the incomingReqQ smaller.
    FIFO#(SCRATCHPAD_MEM_VALUE) writeDataQ <- mkFIFO();

    // Cache responses are not ordered.  Sort them with a reorder buffer.
    Vector#(n_READERS, SCOREBOARD_FIFOF#(SCRATCHPAD_PORT_ROB_SLOTS, SCRATCHPAD_MEM_VALUE)) sortResponseQ <- replicateM(mkScoreboardFIFOF());

    
    // Initialization
    Reg#(Bool) initialized <- mkReg(False);
    rule doInit (! initialized);
        cache.setCacheMode(unpack(cacheMode));
        initialized <= True;
    endrule


    //
    // Forward merged requests to the cache.
    //

    // Write requests
    rule forwardWriteReq (initialized && (incomingReqQ.firstPortID() == fromInteger(valueOf(n_READERS))));
        let addr = incomingReqQ.first();
        incomingReqQ.deq();

        let val = writeDataQ.first();
        writeDataQ.deq();

        cache.write(pack(addr), val, ?);
    endrule


    // Read requests
    for (Integer p = 0; p < valueOf(n_READERS); p = p + 1)
    begin
        rule forwardReadReq (initialized && (incomingReqQ.firstPortID() == fromInteger(p)));
            let addr = incomingReqQ.first();
            incomingReqQ.deq();

            // Allocate a slot in the reorder buffer for the read request.  Each
            // read port gets its own reorder buffer.
            let idx <- sortResponseQ[p].enq();

            // The refInfo for this request is the concatenation of the
            // port ID and the ROB index.
            t_REF_INFO ref_info = tuple2(fromInteger(p), idx);

            // Request data from the cache
            cache.readReq(pack(addr), ref_info);
        endrule

        //
        // receiveResp --
        //     Push read responses to the reorder buffer.  They will be returned
        //     through readRsp() in order.
        //
        rule receiveResp (tpl_1(cache.peekResp().refInfo) == fromInteger(p));
            let r <- cache.readResp();

            // The clientRefInfo field holds the concatenation of the port ID and
            // the port's reorder buffer index.
            match {.port, .idx} = r.refInfo;

            sortResponseQ[p].setValue(idx, r.val);
        endrule
    end


    //
    // Methods.  All requests are stored in the incomingReqQ to maintain their
    // order.
    //

    Vector#(n_READERS, MEMORY_READER_IFC#(t_MEM_ADDRESS, SCRATCHPAD_MEM_VALUE)) portsLocal = newVector();

    for(Integer p = 0; p < valueOf(n_READERS); p = p + 1)
    begin
        portsLocal[p] =
            interface MEMORY_READER_IFC#(t_ADDR, t_DATA);
                method Action readReq(t_MEM_ADDRESS addr);
                    incomingReqQ.ports[p].enq(addr);
                    debugLog.record($format("read port %0d: req addr=0x%x", p, addr));
                endmethod

                method ActionValue#(SCRATCHPAD_MEM_VALUE) readRsp();
                    let r = sortResponseQ[p].first();
                    sortResponseQ[p].deq();

                    debugLog.record($format("read port %0d: resp val=0x%x", p, r));
                    return r;
                endmethod

                method SCRATCHPAD_MEM_VALUE peek();
                    return sortResponseQ[p].first();
                endmethod

                method Bool notEmpty() = sortResponseQ[p].notEmpty();
                method Bool notFull() = incomingReqQ.ports[p].notFull();
            endinterface;
    end

    interface readPorts = portsLocal;

    method Action write(t_MEM_ADDRESS addr, SCRATCHPAD_MEM_VALUE val);
        // The write port is last in the merge FIFO
        incomingReqQ.ports[valueOf(n_READERS)].enq(addr);
        writeDataQ.enq(val);
        debugLog.record($format("write addr=0x%x, val=0x%x", addr, val));
    endmethod

    method Bool writeNotFull = incomingReqQ.ports[valueOf(n_READERS)].notFull();
endmodule


//
// mkScratchpadCacheSourceData --
//     Connection between a private cache for a scratchpad and the platform's
//     scratchpad virtual device.
//
module [CONNECTED_MODULE] mkScratchpadCacheSourceData#(Integer scratchpadID)
    // interface:
    (RL_DM_CACHE_SOURCE_DATA#(t_CACHE_ADDR, SCRATCHPAD_MEM_VALUE, t_CACHE_REF_INFO))
    provisos (Bits#(t_CACHE_ADDR, t_CACHE_ADDR_SZ),
              Bits#(t_CACHE_REF_INFO, t_CACHE_REF_INFO_SZ),
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Alias#(RL_DM_CACHE_FILL_RESP#(t_CACHE_ADDR, SCRATCHPAD_MEM_VALUE, t_CACHE_REF_INFO), t_CACHE_FILL_RESP));

    if (valueOf(t_CACHE_ADDR_SZ) > valueOf(t_SCRATCHPAD_MEM_ADDRESS_SZ))
    begin
        error("Scratchpad ID " + integerToString(scratchpadID) + " address is too large: " + integerToString(valueOf(t_CACHE_ADDR_SZ)) + " bits");
    end

    let my_port = scratchpadPortId(scratchpadID);

    Connection_TokenRing#(SCRATCHPAD_PORT_NUM, SCRATCHPAD_MEM_REQ) link_mem_req <-
        mkConnection_TokenRingNode(`RINGID_SCRATCHPAD_MEMORY_REQ, my_port);

    Connection_TokenRing#(SCRATCHPAD_PORT_NUM, SCRATCHPAD_READ_RSP) link_mem_rsp <-
        mkConnection_TokenRingNode(`RINGID_SCRATCHPAD_MEMORY_RSP, my_port);

    Reg#(Bool) initialized <- mkReg(False);

    //
    // Allocate memory for this scratchpad region
    //
    rule doInit (! initialized);
        initialized <= True;

        Bit#(t_CACHE_ADDR_SZ) alloc = maxBound;
        SCRATCHPAD_INIT_REQ r;
        r.port = my_port;
        r.allocLastWordIdx = zeroExtendNP(alloc);
        r.cached = True;
        link_mem_req.enq(0, tagged SCRATCHPAD_MEM_INIT r);
    endrule

    method Action readReq(t_CACHE_ADDR addr, t_CACHE_REF_INFO refInfo) if (initialized);
        let req = SCRATCHPAD_READ_REQ { port: my_port,
                                        addr: zeroExtendNP(pack(addr)),
                                        byteReadMask: replicate(True),
                                        clientRefInfo: zeroExtendNP(pack(refInfo)) };
        link_mem_req.enq(0, tagged SCRATCHPAD_MEM_READ req);
    endmethod

    method ActionValue#(t_CACHE_FILL_RESP) readResp();
        let s = link_mem_rsp.first();
        link_mem_rsp.deq();

        t_CACHE_FILL_RESP r;
        r.addr = unpack(truncateNP(s.addr));
        r.val = s.val;
        r.refInfo = unpack(truncateNP(s.clientRefInfo));

        return r;
    endmethod

    method t_CACHE_FILL_RESP peekResp();
        let s = link_mem_rsp.first();

        t_CACHE_FILL_RESP r;
        r.addr = unpack(truncateNP(s.addr));
        r.val = s.val;
        r.refInfo = unpack(truncateNP(s.clientRefInfo));

        return r;
    endmethod

    // Asynchronous write (no response)
    method Action write(t_CACHE_ADDR addr,
                        SCRATCHPAD_MEM_VALUE val,
                        t_CACHE_REF_INFO refInfo) if (initialized);
        let req = SCRATCHPAD_WRITE_REQ { port: my_port,
                                         addr: zeroExtendNP(pack(addr)),
                                         val: val };
        link_mem_req.enq(0, tagged SCRATCHPAD_MEM_WRITE req);
    endmethod

    //
    // Invalidate / flush not required for scratchpad memory.
    //
    method Action invalReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo);
        noAction;
    endmethod

    method Action flushReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo);
        noAction;
    endmethod

    method Action invalOrFlushWait();
        noAction;
    endmethod
endmodule



//
// mkUncachedScratchpad --
//     The uncached scratchpad is connected directly to the scratchpad memory
//     and uses neither a private nor the central cache.  To avoid read-modify-
//     write operations on data smaller than a SCRATCHPAD_MEM_VALUE, data
//     is tiled in containers that are one byte or larger and a size that is
//     a power of 2.  A byte write mask is passed to the memory along with
//     write data, thus eliminating the need to read partial values.
//
module [CONNECTED_MODULE] mkUncachedScratchpad#(Integer scratchpadID)
    // interface:
    (MEMORY_MULTI_READ_IFC#(n_READERS, t_IN_ADDR, t_DATA))
    provisos (Bits#(t_IN_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ),

              Alias#(Bit#(t_ADDR_SZ), t_ADDR),    

              // Compute the natural size in bits.  The natural size must be
              // a power of 2 bits that is one byte or larger.
              Max#(8, TExp#(TLog#(t_DATA_SZ)), t_NATURAL_SZ),
              NumAlias#(TDiv#(t_NATURAL_SZ, 8), t_NATURAL_BYTES),

              // Compute scratchpad container index type (size)
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),

              // Index a naturally sized t_DATA within a SCRATCHPAD_MEM_VALUE
              Alias#(Bit#(TLog#(TDiv#(t_SCRATCHPAD_MEM_VALUE_SZ, t_NATURAL_SZ))),
                     t_NATURAL_IDX),

              // Compute a non-zero size for the read port index
              Max#(n_READERS, 2, n_SAFE_READERS),
              NumAlias#(TLog#(n_SAFE_READERS), n_SAFE_READERS_SZ),

              // Index in a reorder buffer
              Alias#(SCOREBOARD_FIFO_ENTRY_ID#(SCRATCHPAD_UNCACHED_PORT_ROB_SLOTS), t_REORDER_ID),

              // Reference info passed to the scratchpad needed to route the response
              Alias#(Tuple3#(Bit#(n_SAFE_READERS_SZ), t_NATURAL_IDX, t_REORDER_ID), t_REF_INFO));

    String debugLogFilename = "platform_scratchpad_" + integerToString(scratchpadID - `VDEV_SCRATCH__BASE + 1) + ".out";
    DEBUG_FILE debugLog <- (`PLATFORM_SCRATCHPAD_DEBUG_ENABLE == 1)?
                           mkDebugFile(debugLogFilename):
                           mkDebugFileNull(debugLogFilename); 

    //
    // Elaboration time checks
    //
    if (valueOf(t_NATURAL_SZ) > valueOf(t_SCRATCHPAD_MEM_VALUE_SZ))
    begin
        //
        // Object size is larger than SCRATCHPAD_MEM_VALUE.  The code here could
        // support this case by issuing multiple reads and writes for every
        // reference.  For now it does not.
        //
        error("Uncached scratchpad doesn't support data larger than scratchpad's base size");
    end

    if (valueOf(TDiv#(t_ADDR_SZ, TDiv#(t_SCRATCHPAD_MEM_VALUE_SZ, t_NATURAL_SZ))) >
        valueOf(t_SCRATCHPAD_MEM_ADDRESS_SZ))
    begin
        //
        // Requested address space is larger than the maximum scratchpad size.
        //
        error("Address space too large.");
    end


    let my_port = scratchpadPortId(scratchpadID);

    Connection_TokenRing#(SCRATCHPAD_PORT_NUM, SCRATCHPAD_MEM_REQ) link_mem_req <-
        mkConnection_TokenRingNode(`RINGID_SCRATCHPAD_MEMORY_REQ, my_port);

    Connection_TokenRing#(SCRATCHPAD_PORT_NUM, SCRATCHPAD_READ_RSP) link_mem_rsp <-
        mkConnection_TokenRingNode(`RINGID_SCRATCHPAD_MEMORY_RSP, my_port);

    // Scratchpad responses are not ordered.  Sort them with a reorder buffer.
    // Each read port gets its own reorder buffer so that each port returns data
    // when available, independent of the latency of requests on other ports.
    Vector#(n_READERS,
            SCOREBOARD_FIFOF#(SCRATCHPAD_UNCACHED_PORT_ROB_SLOTS,
                              t_DATA)) sortResponseQ <- replicateM(mkBRAMScoreboardFIFOF());

    // Buffer between reorder buffer (sortResponseQ) and output methods to
    // reduce timing pressure.
    Vector#(n_READERS, FIFOF#(t_DATA)) responseQ <- replicateM(mkFIFOF());

    // Merge FIFOF combines read and write requests in temporal order,
    // with reads from the same cycle as a write going first.  Each read port
    // gets a slot.  The write port is always last.
    MERGE_FIFOF#(TAdd#(n_READERS, 1), Bit#(t_ADDR_SZ)) incomingReqQ <- mkMergeBypassFIFOF();

    // Write data is sent in a side port to keep the incomingReqQ smaller.
    FIFO#(t_DATA) writeDataQ <- mkBypassFIFO();

    // Most recent writes are collected in a buffer in order to group
    // streaming writes sharing a container into a single transaction.
    Reg#(Maybe#(SCRATCHPAD_MEM_ADDRESS)) lastWriteAddr <- mkReg(tagged Invalid);
    Reg#(SCRATCHPAD_MEM_VALUE) lastWriteVal <- mkRegU();
    Reg#(SCRATCHPAD_MEM_MASK) lastWriteMask <- mkRegU();

    // The most recent read address is recorded.  Multiple read requests
    // to the same address are collapsed into a single request from the backing
    // storage.
    Reg#(Maybe#(SCRATCHPAD_MEM_ADDRESS)) lastReadAddr <- mkReg(tagged Invalid);
    Reg#(SCRATCHPAD_MEM_VALUE) lastReadVal <- mkRegU();

    // Record the source of the next read response (either backing storage
    // or a repeat of the last read's location.
    FIFO#(Tuple4#(Bool,
                  Bit#(n_SAFE_READERS_SZ),
                  t_NATURAL_IDX,
                  t_REORDER_ID))
        readRspSourceQ <- mkSizedFIFO(valueOf(SCRATCHPAD_UNCACHED_PORT_ROB_SLOTS));


    //
    // scratchpadAddr --
    //     Compute scratchpad address given an object address.  Multiple objects
    //     may be stored in a single scratchpad entry.
    //
    function SCRATCHPAD_MEM_ADDRESS scratchpadAddr(t_ADDR addr);
        return zeroExtendNP(unpack(addr) /
                            fromInteger(valueOf(TDiv#(t_SCRATCHPAD_MEM_VALUE_SZ,
                                                      t_NATURAL_SZ))));
    endfunction

    //
    // scratchpadAddrIdx --
    //     Compute the index of a naturally sized object within a scratchpad's
    //     base container size.  This is the remainder of the scratchpadAddr
    //     computation above when multiple objects are stored in each
    //     scratchpad container.
    //
    function t_NATURAL_IDX scratchpadAddrIdx(t_ADDR addr);
        return truncateNP(unpack(addr) % fromInteger(valueOf(TDiv#(t_SCRATCHPAD_MEM_VALUE_SZ,
                                                                   t_NATURAL_SZ))));
    endfunction


    //
    // scratchpadByteMask --
    //     Compute the byte mask of an object within a scratchpad word.
    //
    function SCRATCHPAD_MEM_MASK scratchpadByteMask(t_ADDR addr);
        t_NATURAL_IDX addr_idx = scratchpadAddrIdx(addr);

        // Build a mask of valid bytes
        Vector#(TDiv#(t_SCRATCHPAD_MEM_VALUE_SZ,
                      t_NATURAL_SZ),
                Bit#(TDiv#(t_NATURAL_SZ, 8))) b_mask = replicate(0);
        b_mask[addr_idx] = -1;

        // Size should match.  Resize avoids a proviso.
        return unpack(resize(pack(b_mask)));
    endfunction


    //
    // Allocate memory for this scratchpad region
    //
    Reg#(Bool) initialized <- mkReg(False);
    
    rule doInit (! initialized);
        initialized <= True;

        Bit#(t_ADDR_SZ) alloc = maxBound;
        SCRATCHPAD_INIT_REQ r;
        r.port = my_port;
        r.allocLastWordIdx = scratchpadAddr(alloc);
        r.cached = False;
        link_mem_req.enq(0, tagged SCRATCHPAD_MEM_INIT r);
    endrule


    //
    // Forward merged requests to the memory.
    //

    // Read requests
    rule forwardReadReq (initialized && (incomingReqQ.firstPortID() < fromInteger(valueOf(n_READERS))));
        let port = incomingReqQ.firstPortID();
        let addr = incomingReqQ.first();

        let s_addr = scratchpadAddr(addr);

        if (lastWriteAddr matches tagged Valid .lw_addr &&&
            s_addr == lw_addr)
        begin
            //
            // Conflict with last write.  Flush the last write first.  The
            // read will be retried next cycle.
            //
            let req = SCRATCHPAD_WRITE_MASKED_REQ { port: my_port,
                                                    addr: lw_addr,
                                                    val: lastWriteVal,
                                                    byteWriteMask: lastWriteMask };
            link_mem_req.enq(0, tagged SCRATCHPAD_MEM_WRITE_MASKED req);

            lastWriteAddr <= tagged Invalid;

            debugLog.record($format("port %0d: flush write for read conflict s_addr=0x%x", port, s_addr));
        end
        else
        begin
            //
            // Do the read...
            //
            incomingReqQ.deq();

            // Allocate a slot in the reorder buffer for the read request.  Each
            // read port gets its own reorder buffer.
            let rob_idx <- sortResponseQ[port].enq();

            t_NATURAL_IDX addr_idx = scratchpadAddrIdx(addr);

            if (lastReadAddr matches tagged Valid .lr_addr &&&
                s_addr == lr_addr)
            begin
                // Reading the same address as the last request.  Reuse the response.
                readRspSourceQ.enq(tuple4(True, zeroExtendNP(port), addr_idx, rob_idx));

                debugLog.record($format("read port %0d: reuse addr=0x%x, s_addr=0x%x, s_idx=%0d", port, addr, s_addr, addr_idx));
            end
            else
            begin
                // The clientRefInfo for this request is the concatenation of the
                // port ID, the offset in the scratchpad value, and the ROB index.
                // Resize just eliminates a proviso...
                t_REF_INFO ref_info = resize(tuple3(port, addr_idx, rob_idx));

                let req = SCRATCHPAD_READ_REQ { port: my_port,
                                                addr: s_addr,
                                                byteReadMask: scratchpadByteMask(addr),
                                                clientRefInfo: resize(pack(ref_info)) };

                link_mem_req.enq(0, tagged SCRATCHPAD_MEM_READ req);

                // Record the source of the read value (scratchpad)
                lastReadAddr <= tagged Valid s_addr;
                readRspSourceQ.enq(tuple4(False, ?, ?, ?));

                debugLog.record($format("read port %0d: req addr=0x%x, s_addr=0x%x, s_idx=%0d", port, addr, s_addr, addr_idx));
            end
        end
    endrule


    // Write requests
    rule forwardWriteReq (initialized && (incomingReqQ.firstPortID() == fromInteger(valueOf(n_READERS))));
        let addr = incomingReqQ.first();
        incomingReqQ.deq();
        
        let w_data = writeDataQ.first();
        writeDataQ.deq();

        // Put the data at the right place in the scratchpad word
        Vector#(TDiv#(t_SCRATCHPAD_MEM_VALUE_SZ, t_NATURAL_SZ), Bit#(t_NATURAL_SZ)) d = unpack(0);
        d[scratchpadAddrIdx(addr)] = zeroExtendNP(pack(w_data));

        let s_addr = scratchpadAddr(addr);
        let b_mask = scratchpadByteMask(addr);

        if (lastWriteAddr matches tagged Valid .lw_addr &&&
            s_addr == lw_addr)
        begin
            // Write to same address as previous write.  Merge writes.
            // Resizing is to avoid tautological provisos.  Sizes are actually
            // identical.
            lastWriteVal <= lastWriteVal | resize(pack(d));
            lastWriteMask <= unpack(pack(lastWriteMask) | pack(b_mask));
        end
        else
        begin
            // Write to a new address.  Flush the previous write buffer.
            if (lastWriteAddr matches tagged Valid .lw_addr)
            begin
                let req = SCRATCHPAD_WRITE_MASKED_REQ { port: my_port,
                                                        addr: lw_addr,
                                                        val: lastWriteVal,
                                                        byteWriteMask: lastWriteMask };
                link_mem_req.enq(0, tagged SCRATCHPAD_MEM_WRITE_MASKED req);
            end

            // Record the latest write in the buffer.
            lastWriteAddr <= tagged Valid s_addr;
            lastWriteVal <= resize(pack(d));
            lastWriteMask <= b_mask;

            // Need to invalidate the read history due to conflicting address?
        end

        if (validValue(lastReadAddr) == s_addr)
        begin
            lastReadAddr <= tagged Invalid;
        end

        debugLog.record($format("write addr=0x%x, val=0x%x, s_addr=0x%x, s_val=0x%x, s_bmask=%b", addr, w_data, scratchpadAddr(addr), pack(d), pack(b_mask)));
    endrule


    //
    // receiveResp --
    //     Push unordered read responses to the reorder buffers.  Responses will
    //     be returned through readRsp() in order.
    //
    rule receiveResp (! tpl_1(readRspSourceQ.first()));
        readRspSourceQ.deq();

        let s = link_mem_rsp.first();
        link_mem_rsp.deq();

        // The clientRefInfo field holds the concatenation of the port ID and
        // the port's reorder buffer index.
        t_REF_INFO ref_info = unpack(truncateNP(s.clientRefInfo));
        match {.port, .addr_idx, .rob_idx} = ref_info;

        Vector#(TDiv#(t_SCRATCHPAD_MEM_VALUE_SZ, t_NATURAL_SZ), Bit#(t_NATURAL_SZ)) d;
        // The resize here is required only to avoid a proviso asserting the
        // tautology that Mul#() is equivalent to TMul#().
        d = unpack(resize(s.val));

        t_DATA v = unpack(truncateNP(d[addr_idx]));
        sortResponseQ[port].setValue(rob_idx, v);

        // Record the value in case it is used by reuseResp
        lastReadVal <= s.val;

        debugLog.record($format("read port %0d: resp val=0x%x, s_idx=%0d", port, v, addr_idx));
    endrule


    //
    // reuseResp --
    //     Re-use the same word as the response for the next read request.    
    //
    rule reuseResp (tpl_1(readRspSourceQ.first()));
        match {.reuse, .port, .addr_idx, .rob_idx} = readRspSourceQ.first();
        readRspSourceQ.deq();

        Vector#(TDiv#(t_SCRATCHPAD_MEM_VALUE_SZ, t_NATURAL_SZ), Bit#(t_NATURAL_SZ)) d;
        // The resize here is required only to avoid a proviso asserting the
        // tautology that Mul#() is equivalent to TMul#().
        d = unpack(resize(lastReadVal));

        t_DATA v = unpack(truncateNP(d[addr_idx]));
        sortResponseQ[port].setValue(rob_idx, v);

        debugLog.record($format("read port %0d: reuse val=0x%x, s_idx=%0d", port, v, addr_idx));
    endrule


    //
    // forwardResp --
    //     Forward the next response to the output FIFO.  This stage exists
    //     solely to reduce FPGA timing pressure.
    //
    for (Integer r = 0; r < valueOf(n_READERS); r = r + 1)
    begin
        rule forwardResp (True);
            let d = sortResponseQ[r].first();
            sortResponseQ[r].deq();
            
            responseQ[r].enq(d);
        endrule
    end


    //
    // Methods.  All requests are stored in the incomingReqQ to maintain their
    // order.
    //

    Vector#(n_READERS, MEMORY_READER_IFC#(t_IN_ADDR, t_DATA)) portsLocal = newVector();

    for(Integer p = 0; p < valueOf(n_READERS); p = p + 1)
    begin
        portsLocal[p] =
            interface MEMORY_READER_IFC#(t_IN_ADDR, t_DATA);
                method Action readReq(t_IN_ADDR addr);
                    incomingReqQ.ports[p].enq(pack(addr));
                endmethod

                method ActionValue#(t_DATA) readRsp();
                    let r = responseQ[p].first();
                    responseQ[p].deq();

                    return r;
                endmethod

                method t_DATA peek();
                    return responseQ[p].first();
                endmethod

                method Bool notEmpty() = responseQ[p].notEmpty();
                method Bool notFull() = incomingReqQ.ports[p].notFull();
            endinterface;
    end

    interface readPorts = portsLocal;

    method Action write(t_IN_ADDR addr, t_DATA val);
        // The write port is last in the merge FIFO
        incomingReqQ.ports[valueOf(n_READERS)].enq(pack(addr));
        writeDataQ.enq(val);
    endmethod

    method Bool writeNotFull = incomingReqQ.ports[valueOf(n_READERS)].notFull();
endmodule
