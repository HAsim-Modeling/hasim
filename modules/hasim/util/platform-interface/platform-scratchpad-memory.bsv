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

`include "asim/provides/libfpga_bsv_base.bsh"
`include "asim/provides/scratchpad_memory.bsh"

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
// mkBasicScratchpad --
//     Build a basic scratchpad with no local cache.
//
module [HASIM_MODULE] mkBasicScratchpad#(Integer scratchpadID)
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

    // Container maps requested data size to the platform's scratchpad
    // word size.
    MEMORY_IFC#(t_CONTAINER_ADDR, SCRATCHPAD_MEM_VALUE) containerMemory <- mkDirectScratchpad(scratchpadID);
    
    // Wrap the container with a marshaller.
    MEM_PACK#(t_ADDR, t_DATA, t_CONTAINER_ADDR, SCRATCHPAD_MEM_VALUE) memory <- mkMemPack(containerMemory);

    return memory;
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
module [HASIM_MODULE] mkMemoryHeapUnionScratchpad#(Integer scratchpadID)
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

    MEMORY_HEAP_DATA#(t_INDEX, t_DATA) pool <- mkMemoryHeapUnionScratchpadStorage(scratchpadID);
    MEMORY_HEAP#(t_INDEX, t_DATA) heap <- mkMemoryHeap(pool);

    return heap;
endmodule


//
// mkMemoryHeapUnionScratchpadStorage --
//     Backing storage for a memory heap where the data and free list are
//     stored in the same, unioned, scratchpad memory.
//
module [HASIM_MODULE] mkMemoryHeapUnionScratchpadStorage#(Integer scratchpadID)
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

    // Union storage
    MEMORY_IFC#(t_CONTAINER_INDEX, SCRATCHPAD_MEM_VALUE) containerMemory <- mkDirectScratchpad(scratchpadID);
    MEM_PACK_MULTI_READ#(2, t_INDEX, Bit#(t_UNION_SZ), t_CONTAINER_INDEX, SCRATCHPAD_MEM_VALUE) pool <- mkMemPackPseudoMultiRead(containerMemory);

    interface MEMORY_HEAP_BACKING_STORE data;
        method Action readReq(t_INDEX addr) = pool.readPorts[0].readReq(addr);

        method ActionValue#(t_DATA) readRsp();
            let r <- pool.readPorts[0].readRsp();
            return unpack(truncateNP(r));
        endmethod

        method Action write(t_INDEX addr, t_DATA value);
            pool.write(addr, zeroExtendNP(pack(value)));
        endmethod
    endinterface

    interface MEMORY_HEAP_BACKING_STORE freeList;
        method Action readReq(t_INDEX addr) = pool.readPorts[1].readReq(addr);

        method ActionValue#(t_INDEX) readRsp();
            let r <- pool.readPorts[1].readRsp();
            return unpack(truncateNP(r));
        endmethod

        method Action write(t_INDEX addr, t_INDEX value);
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
// mkDirectScratchpad --
//     Allocate a connection to the platform's scratchpad interface for
//     a single scratchpad region.  This module does no marshalling of
//     data sizes or caching.  BEWARE: the word size of the virtual
//     platform's scratchpad is platform dependent.
//
module [HASIM_MODULE] mkDirectScratchpad#(Integer scratchpadID)
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
    rule forwardReq (True);
        let r = mergeQ.first();
        mergeQ.deq();
        
        link_memory.makeReq(r);
    endrule

    method Action readReq(t_MEM_ADDRESS addr) if (initialized);
        mergeQ.ports[0].enq(tagged SCRATCHPAD_MEM_READ zeroExtend(pack(addr)));
    endmethod

    method ActionValue#(SCRATCHPAD_MEM_VALUE) readRsp();
        let v = link_memory.getResp();
        link_memory.deq();
    
        return v;
    endmethod

    method Action write(t_MEM_ADDRESS addr, SCRATCHPAD_MEM_VALUE val) if (initialized);
        mergeQ.ports[1].enq(tagged SCRATCHPAD_MEM_WRITE { addr: zeroExtend(pack(addr)), val: val });
    endmethod
endmodule
