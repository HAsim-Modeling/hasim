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
    let m <- mkMemoryMultiRead_Multiplexed(mkBRAMBufferedPseudoMultiReadInitialized(unpack(0)));
`else
    let m <- mkMemoryMultiRead_Multiplexed(mkMultiReadScratchpad(scratchpadID, cached));
`endif

    return m;
endmodule
