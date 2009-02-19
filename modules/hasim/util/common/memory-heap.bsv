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
// Managed memory pools.  There are two managed pool interfaces.  The
// immediate version is for LUT-based storage that can be be read and used
// in the same cycle.  The other is for storage with multi-cycle reads.
//

import FIFO::*;
import FIFOF::*;


interface MEMORY_HEAP#(type t_INDEX, type t_DATA);
    // Allocation
    method ActionValue#(t_INDEX) malloc();
    method Action free(t_INDEX addr);
    
    // Data reference
    method Action readReq(t_INDEX addr);
    method ActionValue#(t_DATA) readRsp();
    method Action write(t_INDEX addr, t_DATA value);
endinterface


interface MEMORY_HEAP_IMM#(type t_INDEX, type t_DATA);
    // Allocation
    method ActionValue#(t_INDEX) malloc();
    method Action free(t_INDEX addr);
    
    // Data reference
    method t_DATA sub(t_INDEX addr);
    method Action upd(t_INDEX addr, t_DATA value);
endinterface


// ========================================================================
//
//   Multi-cycle (e.g. BRAM) memory heap manager.
//
// ========================================================================

//
// mkMemoryHeapImm --
//     Storage agnostic implementation of a managed pool of data.  The module
//     takes the memory pool as an argument and thus can manage data stored
//     anywhere.
//
//     This heap manager requires backing storage than is accessed in
//     multiple cycles such as BRAM.
//
module mkMemoryHeap#(MEMORY_HEAP_DATA#(t_INDEX, t_DATA) heap)
    // interface:
    (MEMORY_HEAP#(t_INDEX, t_DATA))
    provisos (Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_INDEX, t_INDEX_SZ));
    
    Reg#(Maybe#(Bit#(t_INDEX_SZ))) freeListHead <- mkReg(tagged Valid minBound);

    //
    // Initialize free list
    //
    Reg#(Bool) initialized <- mkReg(False);
    Reg#(Bit#(t_INDEX_SZ)) init_idx <- mkReg(minBound);
    
    rule initFreeList (! initialized);
        let next_idx = init_idx + 1;

        if (init_idx != maxBound)
        begin
            heap.freeList.write(unpack(init_idx), unpack(next_idx));
        end
        else
        begin
            // Reference to self means end of list
            heap.freeList.write(unpack(init_idx), unpack(init_idx));
            initialized <= True;
        end
        
        init_idx <= next_idx;
    endrule


    //
    // Allocation: malloc / free
    //

    FIFOF#(Bool) mallocReqQ <- mkFIFOF1();
    FIFO#(t_INDEX) mallocQ <- mkFIFO();
    FIFOF#(t_INDEX) freeQ <- mkFIFOF();

    //
    // readFreeList --
    //     Find the element in the free list after the head in preparation for
    //     popping the free list.
    //
    rule fillFreeList (initialized &&&
                       ! freeQ.notEmpty() &&&
                       freeListHead matches tagged Valid .f);
        heap.freeList.readReq(unpack(f));
        // FIFO1 keeps a single request in flight for a given free list head
        mallocReqQ.enq(?);
    endrule

    //
    // manageFreeList --
    //     One rule to control the free list head.  If free() has been called
    //     take the address to free from the freeQ and push it on the free list.
    //     If there is nothing to free then attempt to move the head of the free
    //     list to the malloc queue.
    //
    rule manageFreeList (initialized);
        //
        // Either push freed storage on the free list or try to pop a free
        // entry to the mallocQ.
        //
        if (freeQ.notEmpty())
        begin
            let addr = freeQ.first();
            freeQ.deq();

            // Push on free list
            if (freeListHead matches tagged Valid .f)
                heap.freeList.write(addr, unpack(f));
            else
                // Free list was empty.  Node is end of free list.
                heap.freeList.write(addr, addr);

            freeListHead <= tagged Valid pack(addr);
            
            // Did fillFreeList read from the now stale freeListHead?  Drop
            // the orphaned read.
            if (mallocReqQ.notEmpty())
            begin
                mallocReqQ.deq();
                let fl_next <- heap.freeList.readRsp();
            end
        end
        else if (mallocReqQ.notEmpty() &&& freeListHead matches tagged Valid .f)
        begin
            mallocReqQ.deq();

            // Update free list head pointer
            let fl_next <- heap.freeList.readRsp();
            if (f == pack(fl_next))
                // Pointer to self means end of list
                freeListHead <= tagged Invalid;
            else
                freeListHead <= tagged Valid pack(fl_next);

            mallocQ.enq(unpack(f));
        end
    endrule

    method ActionValue#(t_INDEX) malloc();
        let f = mallocQ.first();
        mallocQ.deq();
        return f;
    endmethod

    method Action free(t_INDEX addr);
        freeQ.enq(addr);
    endmethod


    //
    // Data references
    //

    method Action readReq(t_INDEX addr) = heap.data.readReq(addr);
    method ActionValue#(t_DATA) readRsp() = heap.data.readRsp();
    method Action write(t_INDEX addr, t_DATA value) = heap.data.write(addr, value);
endmodule


//
// Convenience modules for allocating storage and a memory heap.
//

//
// mkMemoryHeapUnionBRAM --
//     Data and free list share same storage.
//
module mkMemoryHeapUnionBRAM
    // interface:
    (MEMORY_HEAP#(t_INDEX, t_DATA))
    provisos (Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_INDEX, t_INDEX_SZ));

    MEMORY_HEAP_DATA#(t_INDEX, t_DATA) pool <- mkMemoryHeapUnionBRAMStorage();
    MEMORY_HEAP#(t_INDEX, t_DATA) heap <- mkMemoryHeap(pool);

    return heap;
endmodule


// ========================================================================
//
//   Immediate (single-cycle) memory heap manager.
//
// ========================================================================

//
// mkMemoryHeapImm --
//     Storage agnostic implementation of a managed pool of data.  The module
//     takes the memory pool as an argument and thus can manage data stored
//     anywhere.
//
//     This heap manager requires backing storage than can be accessed in
//     a single cycle such as LUTRAM or vectors of registers.
//
module mkMemoryHeapImm#(MEMORY_HEAP_IMM_DATA#(t_INDEX, t_DATA) heap)
    // interface:
    (MEMORY_HEAP_IMM#(t_INDEX, t_DATA))
    provisos (Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_INDEX, t_INDEX_SZ),
              Bounded#(t_INDEX));
    
    Reg#(Maybe#(t_INDEX)) freeListHead <- mkReg(tagged Valid minBound);

    //
    // Initialize free list
    //
    Reg#(Bool) initialized <- mkReg(False);
    Reg#(t_INDEX) init_idx <- mkReg(minBound);
    
    rule initFreeList (! initialized);
        // Hack to avoid needing Arith proviso
        let next_idx = unpack(pack(init_idx) + 1);

        // Hack to avoid needing Eq proviso for comparison
        t_INDEX max = maxBound;
        if (pack(init_idx) != pack(max))
        begin
            heap.freeList.upd(init_idx, next_idx);
        end
        else
        begin
            // Reference to self means end of list
            heap.freeList.upd(init_idx, init_idx);
            initialized <= True;
        end
        
        init_idx <= next_idx;
    endrule


    //
    // Allocation: malloc / free
    //

    //
    // malloc and free logic is in rules to manage concurrency.  The rules may
    // not fire in the same cycle.
    //
    FIFO#(t_INDEX) mallocQ <- mkFIFO();
    FIFOF#(t_INDEX) freeQ <- mkFIFOF();

    //
    // fillMallocQ --
    //     Keep a queue of free storage ready for malloc.
    //
    rule fillMallocQ (initialized &&&
                      ! freeQ.notEmpty() &&&
                      freeListHead matches tagged Valid .f);
        // Update free list head pointer
        let fl_next = heap.freeList.sub(f);
        if (pack(f) == pack(fl_next))    // pack hack avoids Eq proviso requirement
            // Pointer to self means end of list
            freeListHead <= tagged Invalid;
        else
            freeListHead <= tagged Valid fl_next;

        mallocQ.enq(f);
    endrule

    rule pushFreeStorage (initialized && freeQ.notEmpty());
        let addr = freeQ.first();
        freeQ.deq();

        if (freeListHead matches tagged Valid .f)
            heap.freeList.upd(addr, f);
        else
            // Free list was empty.  Node is end of free list.
            heap.freeList.upd(addr, addr);
    
        freeListHead <= tagged Valid addr;
    endrule

    method ActionValue#(t_INDEX) malloc();
        let f = mallocQ.first();
        mallocQ.deq();
        return f;
    endmethod

    method Action free(t_INDEX addr);
        freeQ.enq(addr);
    endmethod


    //
    // Data references
    //

    method t_DATA sub(t_INDEX addr) = heap.data.sub(addr);
    method Action upd(t_INDEX addr, t_DATA value) = heap.data.upd(addr, value);
endmodule


//
// Convenience modules for allocating storage and a memory heap.
//

//
// mkMemoryHeapUnionLUTRAM --
//     Data and free list share same storage.
//
module mkMemoryHeapUnionLUTRAM
    // interface:
    (MEMORY_HEAP_IMM#(t_INDEX, t_DATA))
    provisos (Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_INDEX, t_INDEX_SZ),
              Bounded#(t_INDEX));

    MEMORY_HEAP_IMM_DATA#(t_INDEX, t_DATA) pool <- mkMemoryHeapUnionLUTRAMStorage();
    MEMORY_HEAP_IMM#(t_INDEX, t_DATA) heap <- mkMemoryHeapImm(pool);

    return heap;
endmodule


//
// mkMemoryHeapLUTRAM --
//     Separate storage for data and free list.  Uses more space than union
//     LUTRAM above but may allow more readers since free list and data
//     accesses don't share LUTRAM read ports.
//
module mkMemoryHeapLUTRAM
    // interface:
    (MEMORY_HEAP_IMM#(t_INDEX, t_DATA))
    provisos (Bits#(t_DATA, t_DATA_SZ),
              Bits#(t_INDEX, t_INDEX_SZ),
              Bounded#(t_INDEX));

    MEMORY_HEAP_IMM_DATA#(t_INDEX, t_DATA) pool <- mkMemoryHeapLUTRAMStorage();
    MEMORY_HEAP_IMM#(t_INDEX, t_DATA) heap <- mkMemoryHeapImm(pool);

    return heap;
endmodule


// ========================================================================
//
//   Interfaces to multi-cycle (e.g. BRAM) storage for free lists and data.
//
// ========================================================================

//
// MEMORY_HEAP_DATA interface provides interfaces for storage of both data
// and the free list.  It is up to the heap data module either to maintain
// separate storage for data and free lists or to overlay them.  The memory
// heap manager's access pattern guarantees that overlaying data and free
// list storage is safe.
//
interface MEMORY_HEAP_DATA#(type t_INDEX, type t_DATA);
    // Data
    interface MEMORY_HEAP_BACKING_STORE#(t_INDEX, t_DATA) data;

    // Free list
    interface MEMORY_HEAP_BACKING_STORE#(t_INDEX, t_INDEX) freeList;
endinterface

//
// Finally -- actual storage.
//
interface MEMORY_HEAP_BACKING_STORE#(type t_INDEX, type t_DATA);
    method Action readReq(t_INDEX addr);
    method ActionValue#(t_DATA) readRsp();

    method Action write(t_INDEX addr, t_DATA value);
endinterface


//
// mkMemoryHeapUnionBRAMStorage --
//     Backing storage for a memory heap where the data and free list are
//     stored in the same, unioned, BRAM.
//
module mkMemoryHeapUnionBRAMStorage
    // interface:
    (MEMORY_HEAP_DATA#(t_INDEX, t_DATA))
    provisos (Bits#(t_INDEX, t_INDEX_SZ),
              Bits#(t_DATA, t_DATA_SZ),
              Max#(t_INDEX_SZ, t_DATA_SZ, t_UNION_SZ));

    // Union storage
    BRAM_MULTI_READ#(2, t_INDEX, Bit#(t_UNION_SZ)) pool <- mkBRAMPseudoMultiRead();

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
//   Interfaces to single-cycle (immediate) storage for free lists and data.
//
// ========================================================================

//
// MEMORY_HEAP_IMM_DATA interface provides interfaces for storage of both data
// and the free list.  It is up to the heap data module either to maintain
// separate storage for data and free lists or to overlay them.  The memory
// heap manager's access pattern guarantees that overlaying data and free
// list storage is safe.
//
interface MEMORY_HEAP_IMM_DATA#(type t_INDEX, type t_DATA);
    // Data
    interface MEMORY_HEAP_IMM_BACKING_STORE#(t_INDEX, t_DATA) data;

    // Free list
    interface MEMORY_HEAP_IMM_BACKING_STORE#(t_INDEX, t_INDEX) freeList;
endinterface

//
// Actual storage.
//
interface MEMORY_HEAP_IMM_BACKING_STORE#(type t_INDEX, type t_DATA);
    method t_DATA sub(t_INDEX addr);
    method Action upd(t_INDEX addr, t_DATA value);
endinterface


//
// mkMemoryHeapUnionLUTRAMStorage --
//     Backing storage for a memory heap where the data and free list are
//     stored in the same, unioned, LUTRAM.
//
module mkMemoryHeapUnionLUTRAMStorage
    // interface:
    (MEMORY_HEAP_IMM_DATA#(t_INDEX, t_DATA))
    provisos (Bits#(t_INDEX, t_INDEX_SZ),
              Bits#(t_DATA, t_DATA_SZ),
              Bounded#(t_INDEX),
              Max#(t_INDEX_SZ, t_DATA_SZ, t_UNION_SZ));

    // Union storage
    LUTRAM#(t_INDEX, Bit#(t_UNION_SZ)) pool <- mkLUTRAMU();

    interface MEMORY_HEAP_IMM_BACKING_STORE data;
        method t_DATA sub(t_INDEX addr);
            return unpack(truncateNP(pool.sub(addr)));
        endmethod

        method Action upd(t_INDEX addr, t_DATA value);
            pool.upd(addr, zeroExtendNP(pack(value)));
        endmethod
    endinterface

    interface MEMORY_HEAP_IMM_BACKING_STORE freeList;
        method t_INDEX sub(t_INDEX addr);
            return unpack(truncateNP(pool.sub(addr)));
        endmethod

        method Action upd(t_INDEX addr, t_INDEX value);
            pool.upd(addr, zeroExtendNP(pack(value)));
        endmethod
    endinterface
endmodule


//
// mkMemoryHeapLUTRAMStorage --
//     Backing storage for a memory heap where the data and free list are
//     stored in separate LUTRAms.
//
module mkMemoryHeapLUTRAMStorage
    // interface:
    (MEMORY_HEAP_IMM_DATA#(t_INDEX, t_DATA))
    provisos (Bits#(t_INDEX, t_INDEX_SZ),
              Bits#(t_DATA, t_DATA_SZ),
              Bounded#(t_INDEX));

    LUTRAM#(t_INDEX, t_DATA) dataStorage <- mkLUTRAMU();
    LUTRAM#(t_INDEX, t_INDEX) freeListStorage <- mkLUTRAMU();

    interface MEMORY_HEAP_IMM_BACKING_STORE data;
        method t_DATA sub(t_INDEX addr) = dataStorage.sub(addr);
        method Action upd(t_INDEX addr, t_DATA value) = dataStorage.upd(addr, value);
    endinterface

    interface MEMORY_HEAP_IMM_BACKING_STORE freeList;
        method t_INDEX sub(t_INDEX addr) = freeListStorage.sub(addr);
        method Action upd(t_INDEX addr, t_INDEX value) = freeListStorage.upd(addr, value);
    endinterface
endmodule