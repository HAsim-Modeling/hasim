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
// Data structures flowing through soft connections between scratchpad clients
// and the platform interface.
//

typedef struct
{
    SCRATCHPAD_MEM_ADDRESS addr;
    SCRATCHPAD_CLIENT_REF_INFO clientRefInfo;
}
SCRATCHPAD_READ_REQ
    deriving (Eq, Bits);

typedef struct
{
    SCRATCHPAD_MEM_ADDRESS addr;
    SCRATCHPAD_MEM_VALUE val;
}
SCRATCHPAD_WRITE_REQ
    deriving (Eq, Bits);

//
// Scratchpad requests (either a load or a store).
//
typedef union tagged 
{
    SCRATCHPAD_MEM_ADDRESS SCRATCHPAD_MEM_INIT;

    SCRATCHPAD_READ_REQ    SCRATCHPAD_MEM_READ;
    SCRATCHPAD_WRITE_REQ   SCRATCHPAD_MEM_WRITE;
}
SCRATCHPAD_MEM_REQUEST
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
SCRATCHPAD_READ_RESP
    deriving (Eq, Bits);


// Number of slots in a read port's reorder buffer.  The scratchpad subsystem
// does not guarantee to return results in order, so all clients need a ROB.
// The ROB size limits the number of read requests in flight for a given port.
typedef 2 SCRATCHPAD_PORT_ROB_SLOTS;

//
// Construct the name of the soft connection to a scratchpad memory port.
// Ports are created dynamically using dictionaries in the VDEV.SCRATCH
// name space.
//
function String scratchPortName(Integer n) = "vdev_memory_" + integerToString(n - `VDEV_SCRATCH__BASE);


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
module [CONNECTED_MODULE] mkScratchpad#(Integer scratchpadID, Bool cached)
    // interface:
    (MEMORY_IFC#(t_ADDR, t_DATA))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ),

              // Compute container index type (size)
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_ADDR_SZ, t_DATA_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_ADDR),

              // Requested address type must be smaller than scratchpad maximum
              Bits#(t_CONTAINER_ADDR, t_CONTAINER_ADDR_SZ),
              Add#(a__, t_CONTAINER_ADDR_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));

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
//     The same as a normal mkScratchpad but with multiple read ports.
//     Requests are processed in order, with reads being scheduled before
//     a write requested in the same cycle.
//
module [CONNECTED_MODULE] mkMultiReadScratchpad#(Integer scratchpadID, Bool cached)
    // interface:
    (MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ),

              // Compute container index type (size)
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_ADDR_SZ, t_DATA_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_ADDR),

              // Requested address type must be smaller than scratchpad maximum
              Bits#(t_CONTAINER_ADDR, t_CONTAINER_ADDR_SZ),
              Add#(a__, t_CONTAINER_ADDR_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));

    if (cached && (valueOf(TExp#(t_CONTAINER_ADDR_SZ)) <= `SCRATCHPAD_STD_PVT_CACHE_ENTRIES))
    begin
        // A special case:  cached scratchpad requested but the container
        // is smaller than the cache would have been.  Just allocate a BRAM.
        MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA) memory <- mkBRAMBufferedPseudoMultiReadInitialized(unpack(0));

        // Dummy soft connection
        Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_READ_RESP) link_memory <- mkConnection_Client(scratchPortName(scratchpadID));

        return memory;
    end
    else
    begin
        // Container maps requested data size to the platform's scratchpad
        // word size.
        SCRATCHPAD_MEMORY_MULTI_READ_IFC#(n_READERS, t_CONTAINER_ADDR, SCRATCHPAD_MEM_VALUE, `SCRATCHPAD_STD_PVT_CACHE_ENTRIES) containerMemory;
        if (cached)
            containerMemory <- mkUnmarshalledCachedScratchpad(scratchpadID, `PARAMS_SCRATCHPAD_MEMORY_SERVICE_SCRATCHPAD_PVT_CACHE_MODE);
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
module [CONNECTED_MODULE] mkMemoryHeapUnionScratchpad#(Integer scratchpadID, Bool cached)
    // interface:
    (MEMORY_HEAP#(t_INDEX, t_DATA))
    provisos (Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_INDEX, t_INDEX_SZ),
              Max#(t_INDEX_SZ, t_DATA_SZ, t_UNION_SZ),

              // Compute container index type (size)
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_INDEX_SZ, t_UNION_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_INDEX),

              // Assert that container index the container fits in the scratchpad
              // address space.
              Bits#(t_CONTAINER_INDEX, t_CONTAINER_INDEX_SZ),
              Add#(x, t_CONTAINER_INDEX_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));

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
                                                          Bool cached)
    // interface:
    (MEMORY_HEAP_DATA#(t_INDEX, t_DATA))
    provisos (Bits#(t_INDEX, t_INDEX_SZ),
              Bits#(t_DATA, t_DATA_SZ),
              Max#(t_INDEX_SZ, t_DATA_SZ, t_UNION_SZ),

              // Compute container index type (size)
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_INDEX_SZ, t_UNION_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_INDEX),

              // Assert that container index the container fits in the scratchpad
              // address space.
              Bits#(t_CONTAINER_INDEX, t_CONTAINER_INDEX_SZ),
              Add#(x, t_CONTAINER_INDEX_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));

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
              Alias#(Tuple2#(Bit#(n_SAFE_READERS_SZ), t_REORDER_ID), t_REF_INFO),

              // Requested address type must be smaller than scratchpad maximum
              Add#(a__, t_MEM_ADDRESS_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));
    
    DEBUG_FILE debugLog <- mkDebugFile("memory_scratchpad_" + integerToString(scratchpadID - `VDEV_SCRATCH__BASE) + ".out");

    Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_READ_RESP) link_memory <- mkConnection_Client(scratchPortName(scratchpadID));

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
        link_memory.makeReq(tagged SCRATCHPAD_MEM_INIT zeroExtend(alloc));
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

        let req = SCRATCHPAD_READ_REQ { addr: zeroExtend(pack(addr)),
                                        clientRefInfo: zeroExtendNP(pack(ref_info)) };

        link_memory.makeReq(tagged SCRATCHPAD_MEM_READ req);
    endrule

    // Write requests
    rule forwardWriteReq (initialized && (incomingReqQ.firstPortID() == fromInteger(valueOf(n_READERS))));
        let addr = incomingReqQ.first();
        incomingReqQ.deq();
        
        let val = writeDataQ.first();
        writeDataQ.deq();

        let req = SCRATCHPAD_WRITE_REQ { addr: zeroExtend(pack(addr)),
                                         val: val };

        link_memory.makeReq(tagged SCRATCHPAD_MEM_WRITE req);
    endrule

    //
    // receiveResp --
    //     Push unordered read responses to the reorder buffers.  Responses will
    //     be returned through readRsp() in order.
    //
    rule receiveResp (True);
        let s = link_memory.getResp();
        link_memory.deq();

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
module [CONNECTED_MODULE] mkUnmarshalledCachedScratchpad#(Integer scratchpadID, Integer cacheModeParam)
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
              Alias#(Tuple2#(Bit#(n_SAFE_READERS_SZ), t_REORDER_ID), t_REF_INFO),

              // Requested address type must be smaller than scratchpad maximum.
              Add#(a__, t_MEM_ADDRESS_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));
    

    String debugLogFilename = "platform_scratchpad_" + integerToString(scratchpadID - `VDEV_SCRATCH__BASE) + ".out";
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

    // Dummy statistics buckets
    RL_CACHE_STATS stats <- mkNullRLCacheStats();

    // Private cache
    NumTypeParam#(n_CACHE_ENTRIES) num_cache_entries = ?;
    RL_DM_CACHE#(Bit#(t_MEM_ADDRESS_SZ),
                       SCRATCHPAD_MEM_VALUE,
                       t_REF_INFO) cache <- mkCacheDirectMapped(sourceData,
                                                                num_cache_entries,
                                                                False,
                                                                stats,
                                                                debugLog);

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
              Alias#(RL_DM_CACHE_FILL_RESP#(t_CACHE_ADDR, SCRATCHPAD_MEM_VALUE, t_CACHE_REF_INFO), t_CACHE_FILL_RESP),

              // Requested address type must be smaller than scratchpad maximum
              Add#(a__, t_CACHE_ADDR_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));

    Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_READ_RESP) link_memory <- mkConnection_Client(scratchPortName(scratchpadID));

    Reg#(Bool) initialized <- mkReg(False);

    //
    // Allocate memory for this scratchpad region
    //
    rule doInit (! initialized);
        initialized <= True;

        Bit#(t_CACHE_ADDR_SZ) alloc = maxBound;
        link_memory.makeReq(tagged SCRATCHPAD_MEM_INIT zeroExtend(alloc));
    endrule

    method Action readReq(t_CACHE_ADDR addr, t_CACHE_REF_INFO refInfo) if (initialized);
        let req = SCRATCHPAD_READ_REQ { addr: zeroExtend(pack(addr)),
                                        clientRefInfo: zeroExtendNP(pack(refInfo)) };
        link_memory.makeReq(tagged SCRATCHPAD_MEM_READ req);
    endmethod

    method ActionValue#(t_CACHE_FILL_RESP) readResp();
        let s = link_memory.getResp();
        link_memory.deq();

        t_CACHE_FILL_RESP r;
        r.addr = unpack(truncate(s.addr));
        r.val = s.val;
        r.refInfo = unpack(truncateNP(s.clientRefInfo));

        return r;
    endmethod

    method t_CACHE_FILL_RESP peekResp();
        let s = link_memory.getResp();
        
        t_CACHE_FILL_RESP r;
        r.addr = unpack(truncate(s.addr));
        r.val = s.val;
        r.refInfo = unpack(truncateNP(s.clientRefInfo));

        return r;
    endmethod

    // Asynchronous write (no response)
    method Action write(t_CACHE_ADDR addr,
                        SCRATCHPAD_MEM_VALUE val,
                        t_CACHE_REF_INFO refInfo) if (initialized);
        let req = SCRATCHPAD_WRITE_REQ { addr: zeroExtend(pack(addr)),
                                         val: val };
        link_memory.makeReq(tagged SCRATCHPAD_MEM_WRITE req);
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
