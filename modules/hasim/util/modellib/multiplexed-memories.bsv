//
// Copyright (C) 2011 Intel Corporation
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

`include "asim/provides/mem_services.bsh"

//
// Instantiate multiplexed memories in which the instances appear to be separate
// storage.  In reality, the separation is achieved using a single storage
// array and incorporating the multiplexed instance ID into the address.
//

// =============================================================================
//
//   MEMORY_IFC-based memories
//
// =============================================================================

//
// A memory accessed by multiple instances.
//
interface MEMORY_IFC_MULTIPLEXED#(numeric type t_NUM_INSTANCES, type t_ADDR, type t_DATA);
    method Action readReq(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
    method ActionValue#(t_DATA) readRsp(INSTANCE_ID#(t_NUM_INSTANCES) iid);

    method Action write(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr, t_DATA val);
endinterface


//
// A memory with multiple ports, where an instance wants access to multiple
// readReqs/Rsps.
//
interface MEMORY_READER_IFC_MULTIPLEXED#(numeric type t_NUM_INSTANCES, type t_ADDR, type t_DATA);
    method Action readReq(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
    method ActionValue#(t_DATA) readRsp(INSTANCE_ID#(t_NUM_INSTANCES) iid);
endinterface

interface MEMORY_MULTI_READ_IFC_MULTIPLEXED#(numeric type t_NUM_INSTANCES, numeric type nReaders, type t_ADDR, type t_DATA);
    interface Vector#(nReaders, MEMORY_READER_IFC_MULTIPLEXED#(t_NUM_INSTANCES, t_ADDR, t_DATA)) readPorts;

    method Action write(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr, t_DATA val);
endinterface


//
// mkMemory_Multiplexed --
//     Wraps any storage having a MEMORY_IFC with the illusion of separate
//     storage for a multiplexed set.  In reality, the address space is
//     simply exptended to add the instance ID.
//
//     Instantiate the module by passing a module for building the storage, e.g.:
//         mkMemory_Multiplexed(mkBRAM())
//
module [m] mkMemory_Multiplexed#(function m#(MEMORY_IFC#(Bit#(t_EXTENDED_ADDR_SZ), t_DATA)) memImpl)
    // interface:
    (MEMORY_IFC_MULTIPLEXED#(t_NUM_INSTANCES, t_ADDR, t_DATA))
    provisos
        (IsModule#(m, a__),
         Bits#(t_ADDR, t_ADDR_SZ),
         Bits#(t_DATA, t_DATA_SZ),
         // Introduce an extended address space indexed by the number of contexts.
         Add#(INSTANCE_ID_BITS#(t_NUM_INSTANCES), t_ADDR_SZ, t_EXTENDED_ADDR_SZ));

    MEMORY_IFC#(Bit#(t_EXTENDED_ADDR_SZ), t_DATA) memory <- memImpl();

    function Bit#(t_EXTENDED_ADDR_SZ) extendAddress(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
        // NOTE: We might want to reverse this concatenation. It may reduce conflicts in direct-mapped private caches.
        return {pack(iid), pack(addr)};
    endfunction
    
    method Action readReq(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
        // Concatenate the instanceID onto the address and do the read.
        Bit#(t_EXTENDED_ADDR_SZ) ext_addr = extendAddress(iid, addr);
        memory.readReq(ext_addr);
    endmethod
    
    method ActionValue#(t_DATA) readRsp(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        let rsp <- memory.readRsp();
        return rsp;
    endmethod

    method Action write(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr, t_DATA val);
        // Concatenate the instanceId onto the address and do the write.
        Bit#(t_EXTENDED_ADDR_SZ) ext_addr = extendAddress(iid, addr);
        memory.write(ext_addr, val);
    endmethod
endmodule


//
// mkScratchpad_Multiplexed --
//    Convenience module that implements a multiplexed scratchpad memory.
//
module [HASIM_MODULE] mkScratchpad_Multiplexed#(Integer scratchpadID, SCRATCHPAD_CACHE_MODE cached)
    // interface:
    (MEMORY_IFC_MULTIPLEXED#(t_NUM_INSTANCES, t_ADDR, t_DATA))
    provisos
        (Bits#(t_ADDR, t_ADDR_SZ),
         Bits#(t_DATA, t_DATA_SZ),
         // Introduce an extended address space indexed by the number of contexts.
         Add#(INSTANCE_ID_BITS#(t_NUM_INSTANCES), t_ADDR_SZ, t_EXTENDED_ADDR_SZ));

    // Instantiate either a scratchpad or BRAM with the extended address range,
    // depending on whether AWB parameter allows scratchpads.
`ifdef MULTIPLEXED_MEM_USE_SCRATCHPAD_Z
    let m <- mkMemory_Multiplexed(mkBRAMInitialized(unpack(0)));
`else
    let m <- mkMemory_Multiplexed(mkScratchpad(scratchpadID, cached));
`endif

    return m;
endmodule


//
// mkMemoryMultiRead_Multiplexed --
//     Same as mkMemory_Multiplexed above, but for memory with mutliple read
//     ports.
//
//     Instantiate the module by passing a module for building the storage, e.g.:
//         mkMemoryMultiRead_Multiplexed(mkBRAMBufferedPseudoMultiRead())
//
module [m] mkMemoryMultiRead_Multiplexed#(function m#(MEMORY_MULTI_READ_IFC#(t_NUM_READERS, Bit#(t_EXTENDED_ADDR_SZ), t_DATA)) memImpl)
    // interface:
    (MEMORY_MULTI_READ_IFC_MULTIPLEXED#(t_NUM_INSTANCES, t_NUM_READERS, t_ADDR, t_DATA))
    provisos
        (IsModule#(m, a__),
         Bits#(t_ADDR, t_ADDR_SZ),
         Bits#(t_DATA, t_DATA_SZ),
         // Introduce an extended address space indexed by the number of contexts.
         Add#(INSTANCE_ID_BITS#(t_NUM_INSTANCES), t_ADDR_SZ, t_EXTENDED_ADDR_SZ));

    // Instantiate a scratchpad with the extended address range.
    MEMORY_MULTI_READ_IFC#(t_NUM_READERS, Bit#(t_EXTENDED_ADDR_SZ), t_DATA) memory <- memImpl();
    
    Vector#(t_NUM_READERS, MEMORY_READER_IFC_MULTIPLEXED#(t_NUM_INSTANCES, t_ADDR, t_DATA)) localPorts = newVector();

    function Bit#(t_EXTENDED_ADDR_SZ) extendAddress(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
        // NOTE: We might want to reverse this concatenation. It may reduce conflicts in direct-mapped private caches.
        return {pack(iid), pack(addr)};
    endfunction

    for (Integer x = 0; x < valueof(t_NUM_READERS); x = x + 1)
    begin
        localPorts[x] = interface MEMORY_READER_IFC_MULTIPLEXED
                           method Action readReq(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
                               Bit#(t_EXTENDED_ADDR_SZ) ext_addr = extendAddress(iid, addr);
                               memory.readPorts[x].readReq(ext_addr);
                           endmethod

                           method ActionValue#(t_DATA) readRsp(INSTANCE_ID#(t_NUM_INSTANCES) iid);
                               let rsp <- memory.readPorts[x].readRsp();
                               return rsp;
                           endmethod
                       endinterface;
    end

    method Action write(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr, t_DATA val);
        // Concatenate the instanceId onto the address and do the write.
        Bit#(t_EXTENDED_ADDR_SZ) ext_addr = extendAddress(iid, addr);
        memory.write(ext_addr, val);
    endmethod
    
    interface readPorts = localPorts;
endmodule


//
// mkMultiReadScratchpad_Multiplexed --
//    Convenience module that implements a multiplexed scratchpad memory with
//    multiple read ports.
//
module [HASIM_MODULE] mkMultiReadScratchpad_Multiplexed#(Integer scratchpadID, SCRATCHPAD_CACHE_MODE cached)
    // interface:
    (MEMORY_MULTI_READ_IFC_MULTIPLEXED#(t_NUM_INSTANCES, t_NUM_READERS, t_ADDR, t_DATA))
    provisos
        (Bits#(t_ADDR, t_ADDR_SZ),
         Bits#(t_DATA, t_DATA_SZ),
         // Introduce an extended address space indexed by the number of contexts.
         Add#(INSTANCE_ID_BITS#(t_NUM_INSTANCES), t_ADDR_SZ, t_EXTENDED_ADDR_SZ));

    // Instantiate either a scratchpad or BRAM with the extended address range,
    // depending on whether AWB parameter allows scratchpads.
`ifdef MULTIPLEXED_MEM_USE_SCRATCHPAD_Z
    let m <- mkMemoryMultiRead_Multiplexed(mkBRAMBufferedPseudoMultiReadInitialized(True, unpack(0)));
`else
    let m <- mkMemoryMultiRead_Multiplexed(mkMultiReadScratchpad(scratchpadID, cached));
`endif

    return m;
endmodule



// =============================================================================
//
//   LUTRAM-based memories
//
// =============================================================================

//
// MULTIPLEXED_LUTRAM --
//   An abstraction of a multiplexed LUTRAM which is actually a single LUTRAM
//   with a larger address space.
//
interface MULTIPLEXED_LUTRAM#(numeric type t_NUM_INSTANCES, type t_ADDR, type t_DATA);
    method LUTRAM#(t_ADDR, t_DATA) getRAM(INSTANCE_ID#(t_NUM_INSTANCES) iid);
endinterface

interface MULTIPLEXED_LUTRAM_MULTI_READ#(numeric type t_NUM_INSTANCES,
                                         numeric type n_READERS,
                                         type t_ADDR,
                                         type t_DATA);

    method LUTRAM#(t_ADDR, t_DATA) getRAM(INSTANCE_ID#(t_NUM_INSTANCES) iid,
                                          Integer readPort);
endinterface


//
// mapMultiplexedLUTRAMInitFunc() converts an initialization function within
// the space of a single instance ID to the global shared space.
//
typedef function t_DATA f(Tuple2#(t_INSTANCE_ID, t_ADDR) mergedIdx)
    MULTIPLEXED_LUTRAM_INITFUNC#(type t_INSTANCE_ID, type t_ADDR, type t_DATA);

function MULTIPLEXED_LUTRAM_INITFUNC#(t_INSTANCE_ID, t_ADDR, t_DATA)
    mapMultiplexedLUTRAMInitFunc(function t_DATA init(t_ADDR addr));

    function t_DATA merged_init(Tuple2#(t_INSTANCE_ID, t_ADDR) mergedIdx);
        match {.iid, .idx} = mergedIdx;
        return init(idx);
    endfunction

    return merged_init;
endfunction


//
// mkLUTRAM_Multiplexed --
//     Like mkMemory_Multiplexed, wraps any storage having a LUTRAM interface
//     with the illusion of separate storage.
//
//     Note:  If a an initialization function is passed to a LUTRAM constructor
//            the index range must be expanded to include the IID portion.  See
//            mkMultiplexedLUTRAM() below for an example.
//
module [m] mkLUTRAM_Multiplexed#(function m#(LUTRAM#(t_MERGED_IDX, t_DATA)) memImpl)
    // interface:
    (MULTIPLEXED_LUTRAM#(t_NUM_INSTANCES, t_INDEX, t_DATA))
    provisos (IsModule#(m, a__),
              Bits#(t_DATA, t_DATA_SZ),
              Alias#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_INDEX), t_MERGED_IDX),
              Bounded#(t_MERGED_IDX),
              Bits#(t_MERGED_IDX, t_MERGED_IDX_SZ));

    LUTRAM#(t_MERGED_IDX, t_DATA) memory <- memImpl();

    method LUTRAM#(t_INDEX, t_DATA) getRAM(INSTANCE_ID#(t_NUM_INSTANCES) iid);

        // Some Bluespec trickery. Make a LUTRAM interface which wraps the larger address space 
        // LUTRAM and makes it look like a smaller one. Keeping this as a method (as opposed 
        // to a subinterface like a vector) means that we get the dynamic indexing of the LUTRAM.
        
        return interface LUTRAM#(t_INDEX, t_DATA);
                    method t_DATA sub(t_INDEX a) = memory.sub(tuple2(iid, a));
                    method Action upd(t_INDEX a, t_DATA d)  = memory.upd(tuple2(iid, a), d);
               endinterface;
    endmethod
endmodule


//
// mkMultiReadLUTRAM_Multiplexed --
//     Same as mkLUTRA_Multiplexed above but with a multi-read interface for
//     controlling read port allocation explicitly.
//
module [m] mkMultiReadLUTRAM_Multiplexed#(function m#(LUTRAM_MULTI_READ#(n_READERS,
                                                                         t_MERGED_IDX,
                                                                         t_DATA)) memImpl)
    // interface:
    (MULTIPLEXED_LUTRAM_MULTI_READ#(t_NUM_INSTANCES, n_READERS, t_INDEX, t_DATA))
    provisos (IsModule#(m, a__),
              Bits#(t_DATA, t_DATA_SZ),
              Alias#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_INDEX), t_MERGED_IDX),
              Bounded#(t_MERGED_IDX),
              Bits#(t_MERGED_IDX, t_MERGED_IDX_SZ));

    LUTRAM_MULTI_READ#(n_READERS, t_MERGED_IDX, t_DATA) memory <- memImpl();

    //
    // Get a RAM interface to a specified IID and bound to a specific read port...
    //
    method LUTRAM#(t_INDEX, t_DATA) getRAM(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer readPort);

        // Some Bluespec trickery. Make a LUTRAM interface which wraps the larger address space 
        // LUTRAM and makes it look like a smaller one. Keeping this as a method (as opposed 
        // to a subinterface like a vector) means that we get the dynamic indexing of the LUTRAM.
        
        return interface LUTRAM#(t_INDEX, t_DATA);
                    method t_DATA sub(t_INDEX a) = memory.readPorts[readPort].sub(tuple2(iid, a));
                    method Action upd(t_INDEX a, t_DATA d)  = memory.upd(tuple2(iid, a), d);
               endinterface;
    endmethod
endmodule


//
// mkMultiplexedLUTRAM --
//     Convenience module for making a standard multiplexed LUTRAM.
//
module [m] mkMultiplexedLUTRAM#(t_DATA initVal)
    // Interface:
    (MULTIPLEXED_LUTRAM#(t_NUM_INSTANCES, t_INDEX, t_DATA))
    provisos (IsModule#(m, a),
              Bits#(t_INDEX, t_INDEX_SZ),
              Bits#(t_DATA, t_DATA_SZ),
              Bounded#(t_INDEX));

    // Make a dummy initalization function;
    function t_DATA initFunc(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_INDEX) i) = initVal;

    MULTIPLEXED_LUTRAM#(t_NUM_INSTANCES, t_INDEX, t_DATA) m <- mkLUTRAM_Multiplexed(mkLUTRAMWith(initFunc));
    return m;
endmodule


//
// mkMultiplexedLUTRAMInitializedWith --
//     Convenience module for making a multiplexed LUTRAM initialized by a
//     function.
//
//     NOTE:  The constructor function should NOT have its initial value
//            be a function of the index, since the index of the
//            instantiated LUTRAM is different.
//
module [m] mkMultiplexedLUTRAMInitializedWith#(function t_DATA getInitVal(t_INDEX i))
    // Interface:
    (MULTIPLEXED_LUTRAM#(t_NUM_INSTANCES, t_INDEX, t_DATA))
    provisos (IsModule#(m, a),
              Bits#(t_INDEX, t_INDEX_SZ),
              Bits#(t_DATA, t_DATA_SZ),
              Bounded#(t_INDEX));

    let initfunc = mapMultiplexedLUTRAMInitFunc(getInitVal);

    let m <- mkLUTRAM_Multiplexed(mkLUTRAMWith(initfunc));
    return m;
endmodule


//
// MULTIPLEXED_LUTRAM_MULTI_WRITE --
//
//     An abstraction of LUTRAM with multiple write ports. More expensive
//     than above, but fewer conflicts.
//
interface MULTIPLEXED_LUTRAM_MULTI_WRITE#(numeric type t_NUM_INSTANCES, numeric type t_NUM_PORTS, type t_ADDR, type t_DATA);
    method LUTRAM#(t_ADDR, t_DATA) getRAMWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);
endinterface


//
// mkMultiplexedLUTRAMMultiWrite --
//   A multiplexed LUTRAM, implemented as a vector of RAMS. This version has unlimited
//   read/write ports. We use some magic to ensure that writes are conflict-free
//   thus two separate pipeline stages can write the RAM (for instance stage1
//   may set an address to Invalid, and stage3 may conditionally set it to Valid X) without
//   introducing a conflict. The stages may even write the same address, since they are
//   really writing different LUTRAMS in the vector.
//
//   NOTE: This assumes that a given instance ID is not in the pipeline more than once.
//
module [m] mkMultiplexedLUTRAMMultiWrite#(t_DATA initval) 
    // interface:
    (MULTIPLEXED_LUTRAM_MULTI_WRITE#(t_NUM_INSTANCES, t_NUM_PORTS, t_ADDR, t_DATA))
    provisos 
        (Bits#(t_DATA, t_DATA_SZ),
         Bits#(t_ADDR, t_ADDR_SZ),
         Bounded#(t_ADDR),
         IsModule#(m, a),
         Alias#(Tuple3#(INSTANCE_ID#(t_NUM_INSTANCES), t_ADDR, t_DATA), t_WRITE_MSG));

    // The vector of LUTRAMs.
    Vector#(t_NUM_INSTANCES, LUTRAM#(t_ADDR, t_DATA)) ramvec <- replicateM(mkLUTRAMU());

    // A group of wires to record all writes across all writeports.
    // Using wires ensure writes will be conflict-free.
    Vector#(t_NUM_PORTS, RWire#(t_WRITE_MSG)) writeWires <- replicateM(mkRWire);

    // Update each RAM. Favor smaller-numbered write ports, although really
    // it's probably an error if two of the index are valid at the same time
    // across write ports.

    //
    // Initialize storage.  We can't use standard initialized LUTRAMs because
    // that would leave a race between the initialization loop and write
    // requests flowing to updateRAMs below.
    //

    Reg#(Bool) initialized_m <- mkReg(False);
    Reg#(t_ADDR) init_idx <- mkReg(minBound);

    rule initializing (! initialized_m);
        for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
        begin
            ramvec[x].upd(init_idx, initval);
        end

        // Hack to avoid needing Eq proviso for comparison
        t_ADDR max = maxBound;
        initialized_m <= (pack(init_idx) == pack(max));

        // Hack to avoid needing Arith proviso
        init_idx <= unpack(pack(init_idx) + 1);
    endrule

    (* fire_when_enabled *)
    (* no_implicit_conditions *)
    rule updateRAMs (initialized_m);
        // iidMatch() will be used as a predicate to a find() to detect messages
        // bound for a specific instance ID.
        function Bool iidMatch(Integer tgtIID, RWire#(t_WRITE_MSG) msg);
            let m = msg.wget();
            return isValid(m) && (fromInteger(tgtIID) == tpl_1(validValue(m)));
        endfunction

        for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
        begin
            // Is there a write for this instance ID?
            if (find(iidMatch(x), writeWires) matches tagged Valid {.msg})
            begin
                match {.iid, .addr, .val} = validValue(msg.wget());
                ramvec[x].upd(addr, val);
            end
        end
    endrule

    method LUTRAM#(t_ADDR, t_DATA) getRAMWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);

        // Some Bluespec trickery. Make a LUTRAM interface which wraps the Vector
        // and RWires and makes them look like a RAM. As long as different pipeline
        // stages use different integer indices, then the scheduler will not make
        // them conflict.

        return interface LUTRAM#(t_ADDR, t_DATA);
                   method t_DATA sub(t_ADDR a) if (initialized_m);
                       return ramvec[iid].sub(a);
                   endmethod

                   method Action upd(t_ADDR a, t_DATA d) if (initialized_m);
                       writeWires[portnum].wset(tuple3(iid, a, d));
                   endmethod
               endinterface;

    endmethod

endmodule


//
// mkMultiplexedLUTRAMPseudoMultiWrite --
//     Same interface and function as mkMultiplexedLUTRAMMultiWrite but
//     with a single actual write port for a more efficient implementation.
//     Here we trade performance for space.  If multiple write requests arrive
//     in a cycle new requests are blocked until all writes are completed.
//
module [m] mkMultiplexedLUTRAMPseudoMultiWrite#(t_DATA initval) 
    // interface:
    (MULTIPLEXED_LUTRAM_MULTI_WRITE#(t_NUM_INSTANCES, t_NUM_PORTS, t_ADDR, t_DATA))
    provisos 
        (Bits#(t_DATA, t_DATA_SZ),
         Bits#(t_ADDR, t_ADDR_SZ),
         Bounded#(t_ADDR),
         IsModule#(m, a),
         Alias#(Tuple3#(INSTANCE_ID#(t_NUM_INSTANCES), t_ADDR, t_DATA), t_WRITE_MSG));

    // The storage
    MULTIPLEXED_LUTRAM#(t_NUM_INSTANCES, t_ADDR, t_DATA) ram <- mkMultiplexedLUTRAM(initval);

    // Incoming write requests
    MERGE_FIFOF#(t_NUM_PORTS, Tuple3#(INSTANCE_ID#(t_NUM_INSTANCES),
                                      t_ADDR,
                                      t_DATA)) writeQ <- mkMergeBypassFIFOF();

    // Block reads because writes are incomplete?  A BypassFIFO introduces a
    // conbinatorial path on the notEmpty() line, so we must be clever and
    // record whether any reads may potentially fire before the queued stores.
    Reg#(Bool) blockReads <- mkReg(False);
    PulseWire didWrite <- mkPulseWire();

    //
    // updateRAMs --
    //     Consume write requests and commit to memory.
    //
    rule updateRAMs (True);
        match {.iid, .addr, .data} = writeQ.first();
        writeQ.deq();

        ram.getRAM(iid).upd(addr, data);

        // Any more writes in this group?  A bypass FIFO only has one storage
        // slot, so this is equivalent to testing notEmpty.
        blockReads <= ! writeQ.lastInGroup();
        didWrite.send();
    endrule

    //
    // noWrite --
    //     Update the blockReads state when no write is serviced.  This should
    //     not fire for typical LUTRAM implementations, since nothing should
    //     block a write.
    //
    (* fire_when_enabled *)
    (* no_implicit_conditions *)
    rule noWrite (! didWrite);
        blockReads <= writeQ.notEmpty();
    endrule


    method LUTRAM#(t_ADDR, t_DATA) getRAMWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);

        return interface LUTRAM#(t_ADDR, t_DATA);
                   method t_DATA sub(t_ADDR a) if (! blockReads);
                       return ram.getRAM(iid).sub(a);
                   endmethod

                   method Action upd(t_ADDR a, t_DATA d);
                       writeQ.ports[portnum].enq(tuple3(iid, a, d));
                   endmethod
               endinterface;

    endmethod

endmodule
