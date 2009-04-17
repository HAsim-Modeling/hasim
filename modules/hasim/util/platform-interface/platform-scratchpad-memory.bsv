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
// Interfaces to scratchpad memory.
//

import FIFO::*;

`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/librl_bsv_cache.bsh"
`include "asim/provides/scratchpad_memory.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/dict/PARAMS_PLATFORM_INTERFACE.bsh"

`include "asim/dict/VDEV.bsh"
`ifndef VDEV_SCRATCH__BASE
`define VDEV_SCRATCH__BASE 0
`endif

//
// Scratchpad requests (either a load or a store).
//
typedef union tagged 
{
    SCRATCHPAD_MEM_ADDRESS SCRATCHPAD_MEM_INIT;

    SCRATCHPAD_MEM_ADDRESS SCRATCHPAD_MEM_READ;
    struct {SCRATCHPAD_MEM_ADDRESS addr; SCRATCHPAD_MEM_VALUE val;} SCRATCHPAD_MEM_WRITE;
}
SCRATCHPAD_MEM_REQUEST
    deriving (Eq, Bits);


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
module [HASIM_MODULE] mkScratchpad#(Integer scratchpadID, Bool cached)
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

    if (cached && (valueOf(TExp#(t_CONTAINER_ADDR_SZ)) <= `SCRATCHPAD_PVT_CACHE_ENTRIES))
    begin
        // A special case:  cached scratchpad requested but the container
        // is smaller than the cache would have been.  Just allocate a BRAM.
        MEMORY_IFC#(t_ADDR, t_DATA) memory <- mkBRAMInitialized(unpack(0));

        // Dummy soft connection
        Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE) link_memory <- mkConnection_Client(scratchPortName(scratchpadID));

        return memory;
    end
    else
    begin
        // Container maps requested data size to the platform's scratchpad
        // word size.
        MEMORY_IFC#(t_CONTAINER_ADDR, SCRATCHPAD_MEM_VALUE) containerMemory;
        if (cached)
            containerMemory <- mkUnmarshalledCachedScratchpad(scratchpadID);
        else
            containerMemory <- mkUnmarshalledScratchpad(scratchpadID);
    
        // Wrap the container with a marshaller.
        MEM_PACK#(t_ADDR, t_DATA, t_CONTAINER_ADDR, SCRATCHPAD_MEM_VALUE) memory <- mkMemPack(containerMemory);

        return memory;
    end
endmodule


//
// mkPseudoMultiReadScratchpad --
//     The same as a normal mkScratchpad but with multiple, virtual, read
//     ports.  The read ports share a single output FIFO, so deadlocks can
//     result without care for read request and response consumption order.
//
module [HASIM_MODULE] mkPseudoMultiReadScratchpad#(Integer scratchpadID, Bool cached)
    // interface:
    (MEMORY_MULTI_READ_IFC#(nReaders, t_ADDR, t_DATA))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ),

              // Compute container index type (size)
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_ADDR_SZ, t_DATA_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_ADDR),

              // Requested address type must be smaller than scratchpad maximum
              Bits#(t_CONTAINER_ADDR, t_CONTAINER_ADDR_SZ),
              Add#(a__, t_CONTAINER_ADDR_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));

    if (cached && (valueOf(TExp#(t_CONTAINER_ADDR_SZ)) <= `SCRATCHPAD_PVT_CACHE_ENTRIES))
    begin
        // A special case:  cached scratchpad requested but the container
        // is smaller than the cache would have been.  Just allocate a BRAM.
        MEMORY_MULTI_READ_IFC#(nReaders, t_ADDR, t_DATA) memory <- mkBRAMPseudoMultiReadInitialized(unpack(0));

        // Dummy soft connection
        Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE) link_memory <- mkConnection_Client(scratchPortName(scratchpadID));

        return memory;
    end
    else
    begin
        // Container maps requested data size to the platform's scratchpad
        // word size.
        MEMORY_IFC#(t_CONTAINER_ADDR, SCRATCHPAD_MEM_VALUE) containerMemory;
        if (cached)
            containerMemory <- mkUnmarshalledCachedScratchpad(scratchpadID);
        else
            containerMemory <- mkUnmarshalledScratchpad(scratchpadID);
    
        // Wrap the container with a marshaller.
        MEM_PACK_MULTI_READ#(nReaders, t_ADDR, t_DATA, t_CONTAINER_ADDR, SCRATCHPAD_MEM_VALUE) memory <- mkMemPackPseudoMultiRead(containerMemory);

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
module [HASIM_MODULE] mkMemoryHeapUnionScratchpad#(Integer scratchpadID, Bool cached)
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
module [HASIM_MODULE] mkMemoryHeapUnionScratchpadStorage#(Integer scratchpadID,
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

    MEMORY_MULTI_READ_IFC#(2, t_INDEX, Bit#(t_UNION_SZ)) pool <- mkPseudoMultiReadScratchpad(scratchpadID, cached);

    //
    // Scheduling hints like descending_urgency don't work on methods.  We use
    // wires instead.
    //
    Wire#(Bool) freeListReadReqFired <- mkDWire(False);
    Wire#(Bool) freeListWriteFired <- mkDWire(False);

    interface MEMORY_HEAP_BACKING_STORE data;
        method Action readReq(t_INDEX addr) if (! freeListReadReqFired &&
                                                ! freeListWriteFired);
            pool.readPorts[0].readReq(addr);
        endmethod

        method ActionValue#(t_DATA) readRsp();
            let r <- pool.readPorts[0].readRsp();
            return unpack(truncateNP(r));
        endmethod

        method Action write(t_INDEX addr, t_DATA value) if (! freeListReadReqFired &&
                                                            ! freeListWriteFired);
            pool.write(addr, zeroExtendNP(pack(value)));
        endmethod
    endinterface

    interface MEMORY_HEAP_BACKING_STORE freeList;
        method Action readReq(t_INDEX addr);
            freeListReadReqFired <= True;
            pool.readPorts[1].readReq(addr);
        endmethod

        method ActionValue#(t_INDEX) readRsp();
            let r <- pool.readPorts[1].readRsp();
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
module [HASIM_MODULE] mkUnmarshalledScratchpad#(Integer scratchpadID)
    // interface:
    (MEMORY_IFC#(t_MEM_ADDRESS, SCRATCHPAD_MEM_VALUE))
    provisos (Bits#(t_MEM_ADDRESS, t_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),

              // Requested address type must be smaller than scratchpad maximum
              Add#(a__, t_MEM_ADDRESS_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));
    
    Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE) link_memory <- mkConnection_Client(scratchPortName(scratchpadID));

    // Merge FIFOF combines read and write requests in temporal order,
    // with reads from the same cycle as a write going first.
    MERGE_FIFOF#(2, SCRATCHPAD_MEM_REQUEST) mergeQ <- mkMergeBypassFIFOF();

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
    rule forwardReq (initialized);
        let r = mergeQ.first();
        mergeQ.deq();
        
        link_memory.makeReq(r);
    endrule

    method Action readReq(t_MEM_ADDRESS addr);
        mergeQ.ports[0].enq(tagged SCRATCHPAD_MEM_READ zeroExtend(pack(addr)));
    endmethod

    method ActionValue#(SCRATCHPAD_MEM_VALUE) readRsp();
        let v = link_memory.getResp();
        link_memory.deq();
    
        return v;
    endmethod

    method Action write(t_MEM_ADDRESS addr, SCRATCHPAD_MEM_VALUE val);
        mergeQ.ports[1].enq(tagged SCRATCHPAD_MEM_WRITE { addr: zeroExtend(pack(addr)), val: val });
    endmethod
endmodule
    
    
//
// mkUnmarshalledCachedScratchpad --
//     Allocate a cached connection to the platform's scratchpad interface for
//     a single scratchpad region.  This module does no marshalling of
//     data sizes.
//
module [HASIM_MODULE] mkUnmarshalledCachedScratchpad#(Integer scratchpadID)
    // interface:
    (MEMORY_IFC#(t_MEM_ADDRESS, SCRATCHPAD_MEM_VALUE))
    provisos (Bits#(t_MEM_ADDRESS, t_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),

              Alias#(SCOREBOARD_FIFO_ENTRY_ID#(2), t_REORDER_ID),

              // Requested address type must be smaller than scratchpad maximum
              Add#(a__, t_MEM_ADDRESS_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));
    
    DEBUG_FILE debugLog <- mkDebugFile("platform_scratchpad_" + integerToString(scratchpadID - `VDEV_SCRATCH__BASE) + ".out");

    // Dynamic parameters
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();
    Param#(2) cacheMode <- mkDynamicParameter(`PARAMS_PLATFORM_INTERFACE_SCRATCHPAD_PVT_CACHE_MODE, paramNode);

    RL_DM_CACHE_SOURCE_DATA#(Bit#(t_MEM_ADDRESS_SZ),
                             SCRATCHPAD_MEM_VALUE,
                             t_REORDER_ID) sourceData <- mkScratchpadCacheSourceData(scratchpadID);

    RL_DM_CACHE#(Bit#(t_MEM_ADDRESS_SZ),
                 SCRATCHPAD_MEM_VALUE,
                 t_REORDER_ID,
                 0,
                 `SCRATCHPAD_PVT_CACHE_ENTRIES) cache <- mkCacheDirectMapped(sourceData, debugLog);

    // Cache responses are not ordered.  Sort them with a reorder buffer.
    SCOREBOARD_FIFO#(2, SCRATCHPAD_MEM_VALUE) sortResponseQ <- mkScoreboardFIFO();
    
    // Initialization
    Reg#(Bool) initialized <- mkReg(False);
    rule doInit (! initialized);
        cache.setCacheMode(unpack(cacheMode));
        initialized <= True;
    endrule


    //
    // receiveResp --
    //     Push read responses to the reorder buffer.  They will be returned
    //     through readRsp() in order.
    //
    rule receiveResp (True);
        let r <- cache.readResp();
        sortResponseQ.setValue(r.refInfo, r.val);
    endrule


    method Action readReq(t_MEM_ADDRESS addr) if (initialized);
        let idx <- sortResponseQ.enq();
        cache.readReq(pack(addr), idx);
    endmethod

    method ActionValue#(SCRATCHPAD_MEM_VALUE) readRsp();
        let r = sortResponseQ.first();
        sortResponseQ.deq();

        return r;
    endmethod

    method Action write(t_MEM_ADDRESS addr, SCRATCHPAD_MEM_VALUE val) if (initialized);
        cache.write(pack(addr), val, ?);
    endmethod
endmodule
    

//
// mkScratchpadCacheSourceData --
//     Connection between a private cache for a scratchpad and the platform's
//     scratchpad virtual device.
//
module [HASIM_MODULE] mkScratchpadCacheSourceData#(Integer scratchpadID)
    // interface:
    (RL_DM_CACHE_SOURCE_DATA#(t_CACHE_ADDR, SCRATCHPAD_MEM_VALUE, t_CACHE_REF_INFO))
    provisos (Bits#(t_CACHE_ADDR, t_CACHE_ADDR_SZ),
              Bits#(t_CACHE_REF_INFO, t_CACHE_REF_INFO_SZ),
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Alias#(RL_DM_CACHE_FILL_RESP#(t_CACHE_ADDR, SCRATCHPAD_MEM_VALUE, t_CACHE_REF_INFO), t_CACHE_FILL_RESP),

              // Requested address type must be smaller than scratchpad maximum
              Add#(a__, t_CACHE_ADDR_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));

    Connection_Client#(SCRATCHPAD_MEM_REQUEST, SCRATCHPAD_MEM_VALUE) link_memory <- mkConnection_Client(scratchPortName(scratchpadID));

    Reg#(Bool) initialized <- mkReg(False);

    FIFO#(Tuple2#(t_CACHE_ADDR, t_CACHE_REF_INFO)) fillQ <- mkFIFO();
    FIFO#(Bool) writeSyncQ <- mkFIFO();

    //
    // Allocate memory for this scratchpad region
    //
    rule doInit (! initialized);
        initialized <= True;

        Bit#(t_CACHE_ADDR_SZ) alloc = maxBound;
        link_memory.makeReq(tagged SCRATCHPAD_MEM_INIT zeroExtend(alloc));
    endrule

    method Action readReq(t_CACHE_ADDR addr, t_CACHE_REF_INFO refInfo) if (initialized);
        link_memory.makeReq(tagged SCRATCHPAD_MEM_READ zeroExtend(pack(addr)));
        fillQ.enq(tuple2(addr, refInfo));
    endmethod

    method ActionValue#(t_CACHE_FILL_RESP) readResp();
        match {.addr, .refInfo} = fillQ.first();
        fillQ.deq();

        let v = link_memory.getResp();
        link_memory.deq();
    
        t_CACHE_FILL_RESP r;
        r.addr = addr;
        r.val = v;
        r.refInfo = refInfo;

        return r;
    endmethod


    // Asynchronous write (no response)
    method Action write(t_CACHE_ADDR addr,
                        SCRATCHPAD_MEM_VALUE val,
                        t_CACHE_REF_INFO refInfo) if (initialized);
        link_memory.makeReq(tagged SCRATCHPAD_MEM_WRITE { addr: zeroExtend(pack(addr)), val: val });
    endmethod

    // Synchronous write.  writeSyncWait() blocks until the response arrives.
    method Action writeSyncReq(t_CACHE_ADDR addr,
                               SCRATCHPAD_MEM_VALUE val,
                               t_CACHE_REF_INFO refInfo) if (initialized);
        link_memory.makeReq(tagged SCRATCHPAD_MEM_WRITE { addr: zeroExtend(pack(addr)), val: val });

        // Waiting for a flush doesn't mean anything for scratchpad memory.
        // Just feed the ack right back.
        writeSyncQ.enq(?);
    endmethod

    method Action writeSyncWait();
        writeSyncQ.deq();
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
