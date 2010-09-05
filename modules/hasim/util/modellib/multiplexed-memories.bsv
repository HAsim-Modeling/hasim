
`include "asim/provides/mem_services.bsh"


// A memory accessed by multiple instances.
interface MEMORY_IFC_MULTIPLEXED#(numeric type t_NUM_INSTANCES, type t_ADDR, type t_DATA);

    method Action readReq(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
    method ActionValue#(t_DATA) readRsp(INSTANCE_ID#(t_NUM_INSTANCES) iid);

    method Action write(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr, t_DATA val);
    
endinterface


// A memory with multiple ports, where an instance wants access to multiple readReqs/Rsps

interface MEMORY_READER_IFC_MULTIPLEXED#(numeric type t_NUM_INSTANCES, type t_ADDR, type t_DATA);

    method Action readReq(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
    method ActionValue#(t_DATA) readRsp(INSTANCE_ID#(t_NUM_INSTANCES) iid);

endinterface

interface MEMORY_MULTI_READ_IFC_MULTIPLEXED#(numeric type t_NUM_INSTANCES, numeric type nReaders, type t_ADDR, type t_DATA);

    interface Vector#(nReaders, MEMORY_READER_IFC_MULTIPLEXED#(t_NUM_INSTANCES, t_ADDR, t_DATA)) readPorts;

    method Action write(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr, t_DATA val);
    
endinterface

module [HASIM_MODULE] mkScratchpad_Multiplexed#(Integer scratchpadID, SCRATCHPAD_CACHE_MODE cached)
    // interface:
        (MEMORY_IFC_MULTIPLEXED#(t_NUM_INSTANCES, t_ADDR, t_DATA))
    provisos
        (Bits#(t_ADDR, t_ADDR_SZ),
         Bits#(t_DATA, t_DATA_SZ),
         Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
         Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_ADDR_SZ, t_EXTENDED_ADDR_SZ)); // Introduce an extended address space indexed by the number of contexts.


    // Instantiate either a scratchpad or BRAM with the extended address range.
    MEMORY_IFC#(Bit#(t_EXTENDED_ADDR_SZ), t_DATA) scratchpad <-
`ifdef MULTIPLEXED_MEM_USE_SCRATCHPAD_Z
        mkBRAMInitialized(unpack(0));
`else
        mkScratchpad(scratchpadID, cached);
`endif
    
    function Bit#(t_EXTENDED_ADDR_SZ) extendAddress(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
    
        // NOTE: We might want to reverse this concatenation. It may reduce conflicts in direct-mapped private caches.
        return {pack(iid), pack(addr)};
    
    endfunction
    
    method Action readReq(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr);
     
        // Concatenate the instanceID onto the address and do the read.
        Bit#(t_EXTENDED_ADDR_SZ) ext_addr = extendAddress(iid, addr);
        scratchpad.readReq(ext_addr);

    endmethod
    
    method ActionValue#(t_DATA) readRsp(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    
        let rsp <- scratchpad.readRsp();

        return rsp;

    endmethod

    method Action write(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr, t_DATA val);
    
        // Concatenate the instanceId onto the address and do the write.
        Bit#(t_EXTENDED_ADDR_SZ) ext_addr = extendAddress(iid, addr);
        scratchpad.write(ext_addr, val);
    
    endmethod

endmodule

module [HASIM_MODULE] mkMultiReadScratchpad_Multiplexed#(Integer scratchpadID, SCRATCHPAD_CACHE_MODE cached)
    // interface:
        (MEMORY_MULTI_READ_IFC_MULTIPLEXED#(t_NUM_INSTANCES, t_NUM_READERS, t_ADDR, t_DATA))
    provisos
        (Bits#(t_ADDR, t_ADDR_SZ),
         Bits#(t_DATA, t_DATA_SZ),
         Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
         Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_ADDR_SZ, t_EXTENDED_ADDR_SZ)); // Introduce an extended address space indexed by the number of contexts.


    // Instantiate a scratchpad with the extended address range.
    MEMORY_MULTI_READ_IFC#(t_NUM_READERS, Bit#(t_EXTENDED_ADDR_SZ), t_DATA) scratchpad <-
`ifdef MULTIPLEXED_MEM_USE_SCRATCHPAD_Z
        mkBRAMBufferedPseudoMultiReadInitialized(unpack(0));
`else
        mkMultiReadScratchpad(scratchpadID, cached);
`endif
    
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
                               scratchpad.readPorts[x].readReq(ext_addr);

                           endmethod

                           method ActionValue#(t_DATA) readRsp(INSTANCE_ID#(t_NUM_INSTANCES) iid);

                               let rsp <- scratchpad.readPorts[x].readRsp();
                               return rsp;

                           endmethod

                       endinterface;

    end

    method Action write(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_ADDR addr, t_DATA val);
    
        // Concatenate the instanceId onto the address and do the write.
        Bit#(t_EXTENDED_ADDR_SZ) ext_addr = extendAddress(iid, addr);
        scratchpad.write(ext_addr, val);
    
    endmethod
    
    interface readPorts = localPorts;

endmodule
