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
import FIFOF::*;
import SpecialFIFOs::*;
import Vector::*;


`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/librl_bsv_cache.bsh"
`include "asim/provides/scratchpad_memory.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/dict/PARAMS_SCRATCHPAD_MEMORY_SERVICE.bsh"

`include "asim/dict/VDEV.bsh"
`ifndef VDEV_SCRATCH__BASE
`define VDEV_SCRATCH__BASE 0
`endif

//
// mkMultiReadMultiCacheScratchpad --
//     The same as a normal mkScratchpad but with multiple read ports.
//     Each read port has its own cache.
//     Requests are processed in order, with reads being scheduled before
//     a write requested in the same cycle.
//

module [CONNECTED_MODULE] mkMultiReadMultiCacheScratchpad#(
    Integer scratchpadID,
    Vector#(n_READERS, Integer) cacheModes,
    Vector#(n_READERS, SCRATCHPAD_STATS_CONSTRUCTOR) mkCacheStats,
    Vector#(n_READERS, SCRATCHPAD_CACHE_CONSTRUCTOR#(t_CONTAINER_ADDR_SZ,
                                                     t_REF_INFO)) cacheConstructors)
    // interface:
        (MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ),
 
              // these provisos are required by the t_REF_INFO
              // Compute a non-zero size for the read port index
              Max#(n_READERS, 2, n_SAFE_READERS),
              Log#(n_SAFE_READERS, n_SAFE_READERS_SZ),
              Add#(1,extra_READERS,n_READERS),

              // Index in a reorder buffer
              Alias#(SCOREBOARD_FIFO_ENTRY_ID#(SCRATCHPAD_PORT_ROB_SLOTS), t_REORDER_ID),

              // Reference info passed to the scratchpad needed to route the response
              Alias#(Tuple2#(Bit#(n_SAFE_READERS_SZ), t_REORDER_ID), t_REF_INFO),

              // Compute container index type (size)
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_ADDR_SZ, t_DATA_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_ADDR),

              // Requested address type must be smaller than scratchpad maximum
              Bits#(t_CONTAINER_ADDR, t_CONTAINER_ADDR_SZ),
              Add#(a__, t_CONTAINER_ADDR_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ)/*,
              IsModule#(m,c)*/);

    if (valueOf(TExp#(t_CONTAINER_ADDR_SZ)) <= `SCRATCHPAD_STD_PVT_CACHE_ENTRIES)
    begin
        // A special case:  cached scratchpad requested but the container
        // is smaller than the cache would have been.  Just allocate a BRAM.
        // Cache behavior is automatic, but any user cache functions will be ignored. 
        // May want to get rid of this behavior at some point.  
        MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA) memory <- mkBRAMBufferedPseudoMultiReadInitialized(unpack(0));

        return memory;
    end
    else 
    begin
        // Container maps requested data size to the platform's scratchpad
        // word size.  
        SCRATCHPAD_MEMORY_MULTI_READ_IFC#(n_READERS, 
                                          t_CONTAINER_ADDR, 
                                          SCRATCHPAD_MEM_VALUE, 
                                          `SCRATCHPAD_STD_PVT_CACHE_ENTRIES) containerMemory;

        containerMemory <- mkUnmarshalledMultiCachedScratchpad(scratchpadID, 
                                                               cacheModes,
                                                               mkCacheStats,
                                                               cacheConstructors );
       
        // Wrap the container with a marshaller.
        // since each read port is logically independent, we give each its own marshaller.
        
        // First make a bunch of sub interfaces... 
        Vector#(n_READERS, MEMORY_IFC#(t_CONTAINER_ADDR, 
                                        SCRATCHPAD_MEM_VALUE))   containerMemIfcs <- 
              mkMultiReadMemIfcToVectorMemIfc(containerMemory);

        // Give each a marshaller         
        let memories <- mapM(mkMemPack,containerMemIfcs);
 
        
        // and map them back to the original interface
        // Here we only give the Zero interface writes
        let memReaders <- mapM(mkMemIfcToMemReaderIfc,memories);
        let memWriter  <- mkMemIfcToMemWriterIfc(head(memories));
        let memory <- mkMemWriterAndVectorMemReaderIfcToMultiReadMemIfc(memWriter, memReaders);
        
        

        return memory;
    end
endmodule



//
// mkMultiReadMultiCacheScratchpad --
//     The same as a normal mkScratchpad but with multiple read ports.
//     Each read port has its own cache.
//     Requests are processed in order, with reads being scheduled before
//     a write requested in the same cycle.
//
// Do something about n_Entries...  It isn't necessary, but okay for now.

module [CONNECTED_MODULE] mkMultiReadMultiCacheWriteCacheScratchpad#(
    Integer scratchpadID,
    Integer writeCacheMode,
    SCRATCHPAD_STATS_CONSTRUCTOR mkWriteCacheStats,
    SCRATCHPAD_CACHE_CONSTRUCTOR#(t_CONTAINER_ADDR_SZ,
                                  t_REF_INFO) writeCacheConstructor,
    Vector#(n_READERS, Integer) cacheModes,
    Vector#(n_READERS, SCRATCHPAD_STATS_CONSTRUCTOR) mkCacheStats,
    Vector#(n_READERS, SCRATCHPAD_CACHE_CONSTRUCTOR#(t_CONTAINER_ADDR_SZ,
                                                     t_REF_INFO)) cacheConstructors)
    // interface:
    (MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA))
    provisos (Bits#(t_ADDR, t_ADDR_SZ),
              Bits#(t_DATA, t_DATA_SZ),
 
              // these provisos are required by the t_REF_INFO
              // Compute a non-zero size for the read port index
              Max#(TAdd#(1,n_READERS), 2, n_SAFE_READERS),
              Add#(1, n_READERS, TAdd#(1, n_READERS)),
              Log#(n_SAFE_READERS, n_SAFE_READERS_SZ),


              // Index in a reorder buffer
              Alias#(SCOREBOARD_FIFO_ENTRY_ID#(SCRATCHPAD_PORT_ROB_SLOTS), t_REORDER_ID),

              // Reference info passed to the scratchpad needed to route the response
              Alias#(Tuple2#(Bit#(n_SAFE_READERS_SZ), t_REORDER_ID), t_REF_INFO),

              // Compute container index type (size)
              Bits#(SCRATCHPAD_MEM_ADDRESS, t_SCRATCHPAD_MEM_ADDRESS_SZ),
              Bits#(SCRATCHPAD_MEM_VALUE, t_SCRATCHPAD_MEM_VALUE_SZ),
              Alias#(MEM_PACK_CONTAINER_ADDR#(t_ADDR_SZ, t_DATA_SZ, t_SCRATCHPAD_MEM_VALUE_SZ), t_CONTAINER_ADDR),

              // Requested address type must be smaller than scratchpad maximum
              Bits#(t_CONTAINER_ADDR, t_CONTAINER_ADDR_SZ),
              Add#(a__, t_CONTAINER_ADDR_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ));

    if (valueOf(TExp#(t_CONTAINER_ADDR_SZ)) <= `SCRATCHPAD_STD_PVT_CACHE_ENTRIES)
    begin
        // A special case:  cached scratchpad requested but the container
        // is smaller than the cache would have been.  Just allocate a BRAM.
        // Cache behavior is automatic, but any user cache functions will be ignored. 
        // May want to get rid of this behavior at some point.  
        MEMORY_MULTI_READ_IFC#(n_READERS, t_ADDR, t_DATA) memory <- mkBRAMBufferedPseudoMultiReadInitialized(unpack(0));

        return memory;
    end
    else 
    begin
        // Container maps requested data size to the platform's scratchpad
        // word size.  
        SCRATCHPAD_MEMORY_MULTI_READ_IFC#(TAdd#(1,n_READERS), 
                                          t_CONTAINER_ADDR, 
                                          SCRATCHPAD_MEM_VALUE, 
                                          `SCRATCHPAD_STD_PVT_CACHE_ENTRIES) containerMemory;

        // the write cache has to be write through to maintain coherence. 
        // probably want write no allocate as well.
        
        // build a null buffer for the writes?
        String debugLogFilename = "platform_scratchpad_" + integerToString(scratchpadID - `VDEV_SCRATCH__BASE) + "_write.out";
    DEBUG_FILE debugLog <- (`PLATFORM_SCRATCHPAD_DEBUG_ENABLE == 1)?
                           mkDebugFile(debugLogFilename):
                           mkDebugFileNull(debugLogFilename); 
        
 
         containerMemory <- mkUnmarshalledMultiCachedScratchpad(scratchpadID, 
                                                               cons(writeCacheMode, cacheModes), 
                                                               cons(mkWriteCacheStats, mkCacheStats),
                                                               cons(writeCacheConstructor, cacheConstructors));
       
        // Wrap the container with a marshaller.
        // since each read port is logically independent, we give each its own marshaller.
        
        // First make a bunch of sub interfaces... 
        Vector#(TAdd#(1,n_READERS), MEMORY_IFC#(t_CONTAINER_ADDR, 
                                        SCRATCHPAD_MEM_VALUE))   containerMemIfcs <- 
              mkMultiReadMemIfcToVectorMemIfc(containerMemory);

        // Give each a marshaller         
        let memories <- mapM(mkMemPack,containerMemIfcs);

        // and map them back to the original interface
        // Here we only give the Zero interface writes
        let memReaders <- mapM(mkMemIfcToMemReaderIfc,tail(memories));
        let memWriter  <- mkMemIfcToMemWriterIfc(head(memories));
        let memory <- mkMemWriterAndVectorMemReaderIfcToMultiReadMemIfc(memWriter, memReaders);

        return memory;
    end
endmodule 



typedef struct {
  t_LOCAL_REF_INFO localRefInfo;
  t_CLIENT_REF_INFO clientRefInfo;
} MULTI_READ_REF_INFO#(type t_LOCAL_REF_INFO, type t_CLIENT_REF_INFO)
  deriving (Bits,Eq);

//
// mkMultiReadSourceData - this module allows a source to be shared among several ports
// by tagging read requests with a port number 
//

module mkMultiReadSourceData#(RL_DM_CACHE_SOURCE_DATA#(t_CACHE_ADDR,
                                                       t_CACHE_DATA,
                                                       MULTI_READ_REF_INFO#(Bit#(n_SAFE_READERS),
                                                                            t_CACHE_REF_INFO)) sourceData,
                              Integer reader)

                          (RL_DM_CACHE_SOURCE_DATA#(t_CACHE_ADDR,
                                                    t_CACHE_DATA,
                                                    t_CACHE_REF_INFO))
    provisos( Bits#(t_CACHE_ADDR, t_CACHE_ADDR_SZ),
              Bits#(t_CACHE_DATA, t_CACHE_DATA_SZ),
              Bits#(t_CACHE_REF_INFO, t_CACHE_REF_INFO_SZ));

    // Fill request and response with data.  Since the response is tagged with
    // the details of the request, responses may be returned in any order.
    method Action readReq(t_CACHE_ADDR addr, t_CACHE_REF_INFO refInfo);
      sourceData.readReq(addr, MULTI_READ_REF_INFO{localRefInfo: fromInteger(reader), 
                                                   clientRefInfo: refInfo});
    endmethod

    method ActionValue#(RL_DM_CACHE_FILL_RESP#(t_CACHE_ADDR,
                                               t_CACHE_DATA,
                                               t_CACHE_REF_INFO)) readResp() if(sourceData.peekResp.refInfo.localRefInfo == fromInteger(reader));
        let resp <- sourceData.readResp();
        return RL_DM_CACHE_FILL_RESP{addr: resp.addr, 
                                     val: resp.val, 
                                     refInfo: resp.refInfo.clientRefInfo};
    endmethod

    method RL_DM_CACHE_FILL_RESP#(t_CACHE_ADDR,
                                  t_CACHE_DATA,
                                  t_CACHE_REF_INFO) peekResp;
        let resp = sourceData.peekResp();
        return RL_DM_CACHE_FILL_RESP{addr: resp.addr, val: resp.val, refInfo: resp.refInfo.clientRefInfo};
    endmethod

    // These methods are the same, irrespective of port, but require ref
    // info translation 
    method Action write(t_CACHE_ADDR addr,
                        t_CACHE_DATA val,
                        t_CACHE_REF_INFO refInfo);
        sourceData.write(addr,
                         val, 
                         MULTI_READ_REF_INFO{localRefInfo: fromInteger(reader), 
                                             clientRefInfo: refInfo});                           
    endmethod    

    method Action invalReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo);
        sourceData.invalReq(addr,
                            sendAck, 
                            MULTI_READ_REF_INFO{localRefInfo: fromInteger(reader), 
                                                clientRefInfo: refInfo});
    endmethod

    method Action flushReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo);
        sourceData.flushReq(addr,
                            sendAck, 
                            MULTI_READ_REF_INFO{localRefInfo: fromInteger(reader), 
                                                clientRefInfo: refInfo});
    endmethod

    method invalOrFlushWait = sourceData.invalOrFlushWait;

endmodule
    

//
// mkMultiReadNoWriteourceData - this module allows a source to be shared 
// among several ports by tagging read requests with a port number.  Writes 
// requests are dropped.  This allows a coherence broadcast, while preventing
// the source from being inundated with duplicate write requests. 
//

module mkMultiReadNoWriteSourceData#(RL_DM_CACHE_SOURCE_DATA#(t_CACHE_ADDR,
                                                              t_CACHE_DATA,
                                                              MULTI_READ_REF_INFO#(Bit#(n_SAFE_READERS),
                                                                                   t_CACHE_REF_INFO)) sourceData,
                              Integer reader)

                          (RL_DM_CACHE_SOURCE_DATA#(t_CACHE_ADDR,
                                                    t_CACHE_DATA,
                                                    t_CACHE_REF_INFO))
    provisos( Bits#(t_CACHE_ADDR, t_CACHE_ADDR_SZ),
              Bits#(t_CACHE_DATA, t_CACHE_DATA_SZ),
              Bits#(t_CACHE_REF_INFO, t_CACHE_REF_INFO_SZ));

    // Fill request and response with data.  Since the response is tagged with
    // the details of the request, responses may be returned in any order.
    method Action readReq(t_CACHE_ADDR addr, t_CACHE_REF_INFO refInfo);
        sourceData.readReq(addr, MULTI_READ_REF_INFO{localRefInfo: fromInteger(reader), 
                                                   clientRefInfo: refInfo});
    endmethod

    method ActionValue#(RL_DM_CACHE_FILL_RESP#(t_CACHE_ADDR,
                                               t_CACHE_DATA,
                                               t_CACHE_REF_INFO)) readResp() if(sourceData.peekResp.refInfo.localRefInfo == fromInteger(reader));
        let resp <- sourceData.readResp();
        return RL_DM_CACHE_FILL_RESP{addr: resp.addr, 
                                     val: resp.val, 
                                     refInfo: resp.refInfo.clientRefInfo};
    endmethod

    method RL_DM_CACHE_FILL_RESP#(t_CACHE_ADDR,
                                  t_CACHE_DATA,
                                  t_CACHE_REF_INFO) peekResp;
        let resp = sourceData.peekResp();
        return RL_DM_CACHE_FILL_RESP{addr: resp.addr, 
                                     val: resp.val, 
                                     refInfo: resp.refInfo.clientRefInfo};
    endmethod

    // Drop writes on the floor  
    method Action write(t_CACHE_ADDR addr,
                        t_CACHE_DATA val,
                        t_CACHE_REF_INFO refInfo);
        noAction; // Dropping the write on the floor
    endmethod    

    method Action invalReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo);
        sourceData.invalReq(addr, 
                            sendAck, 
                            MULTI_READ_REF_INFO{localRefInfo: fromInteger(reader), 
                                                clientRefInfo: refInfo});
    endmethod

    method Action flushReq(t_CACHE_ADDR addr, Bool sendAck, t_CACHE_REF_INFO refInfo);
        sourceData.flushReq(addr,
                            sendAck, 
                            MULTI_READ_REF_INFO{localRefInfo: fromInteger(reader), 
                                                clientRefInfo: refInfo});
    endmethod

    method invalOrFlushWait = sourceData.invalOrFlushWait;

endmodule


//
// mkUnmarshalledMultiCacheScratchpad --
//     Allocate a connection to the platform's scratchpad interface for
//     a single scratchpad region, with each reader having its own cache.  
//     Coherence is achieved by broadcasting all writes to all caches. 
//     This module does no marshalling of data sizes. 
//     Since we send writes directly to the backing store, we have a 
//     possibility of re-ordering reads and writes.  Be aware of this 
//     relaxed memory ordering when using this module.  
             
module [CONNECTED_MODULE] mkUnmarshalledMultiCachedScratchpad#(
    Integer scratchpadID, 
    Vector#(n_READERS, Integer) cacheModes,
    Vector#(n_READERS, SCRATCHPAD_STATS_CONSTRUCTOR) mkCacheStats,
    Vector#(n_READERS, SCRATCHPAD_CACHE_CONSTRUCTOR#(t_MEM_ADDRESS_SZ,
                                                     t_REF_INFO)) cacheConstructors)
                                                          
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
    

    // Connection between private cache and the scratchpad virtual device
    RL_DM_CACHE_SOURCE_DATA#(Bit#(t_MEM_ADDRESS_SZ),
                             SCRATCHPAD_MEM_VALUE,
                             MULTI_READ_REF_INFO#(Bit#(n_SAFE_READERS_SZ),
                                                  t_REF_INFO)) sourceData <- 
                                 mkScratchpadCacheSourceData(scratchpadID);

    FIFOF#(RL_DM_CACHE_STORE_REQ#(SCRATCHPAD_MEM_VALUE,
                                  Bit#(t_MEM_ADDRESS_SZ))) writeDataQ <- mkFIFOF; 

    Vector#(n_READERS,RL_DM_CACHE#(Bit#(t_MEM_ADDRESS_SZ), 
                                   SCRATCHPAD_MEM_VALUE, 
                                   t_REF_INFO)) caches 
        = newVector();

    String debugLogFilename = "platform_scratchpad_" + integerToString(scratchpadID - `VDEV_SCRATCH__BASE) + ".out";
    DEBUG_FILE debugLog <- (`PLATFORM_SCRATCHPAD_DEBUG_ENABLE == 1)?
                           mkDebugFile(debugLogFilename):
                           mkDebugFileNull(debugLogFilename); 

 
    // call all the constructors for the caches
    // some setup for each cache is needed.
    for(Integer i = 0; i < valueof(n_READERS); i = i + 1) 
    begin

        RL_DM_CACHE_SOURCE_DATA#(Bit#(t_MEM_ADDRESS_SZ),
                                 SCRATCHPAD_MEM_VALUE,
                                 t_REF_INFO) sourceDataWrap <- 
                                     mkMultiReadNoWriteSourceData(sourceData, i);

        caches[i] <- cacheConstructors[i](sourceDataWrap);
        
        // Instantiate statistics tracker
        let cacheStats <- mkCacheStats[i](caches[i].stats);

    end 

    // Cache responses are not ordered.  Sort them with a reorder buffer.
    Vector#(n_READERS, SCOREBOARD_FIFOF#(SCRATCHPAD_PORT_ROB_SLOTS, SCRATCHPAD_MEM_VALUE)) sortResponseQ <- replicateM(mkScoreboardFIFOF());

    
    // Initialization
    Reg#(Bool) initialized <- mkReg(False);
    rule doInit (! initialized);
        for(Integer i = 0; i < valueof(n_READERS); i = i + 1) 
        begin
            caches[i].setCacheMode(unpack(fromInteger(cacheModes[i])));
        end
        initialized <= True;
    endrule

    rule doWrite(initialized);
        writeDataQ.deq;
        t_REF_INFO refInfo = tuple2(0,0); // dummy value
        // Send Write to all caches (keep them all coherent for now)
        // Since the writes will be killed we also send the write directly
        // to main mem.
        sourceData.write(writeDataQ.first.addr, 
                         writeDataQ.first.val, 
                         MULTI_READ_REF_INFO{localRefInfo: 0, 
                                             clientRefInfo: refInfo});
        for (Integer p = 0; p < valueOf(n_READERS); p = p + 1)
        begin
            caches[p].write(writeDataQ.first.addr, 
                            writeDataQ.first.val,
                            refInfo);
        end
        debugLog.record($format("write addr=0x%x, val=0x%x", writeDataQ.first.addr, writeDataQ.first.val));
    endrule

    // Read requests
    for (Integer p = 0; p < valueOf(n_READERS); p = p + 1)
    begin
        //
        // receiveResp --
        //     Push read responses to the reorder buffer.  They will be returned
        //     through readRsp() in order.
        //
        rule receiveResp;
            let r <- caches[p].readResp();

            // The clientRefInfo field holds the concatenation of the port ID and
            // the port's reorder buffer index.
            match {.port, .idx} = r.refInfo;
            debugLog.record($format("readResp port %d addr=0x%x, val=0x%x", port, idx, r.val));
            sortResponseQ[p].setValue(idx, r.val);
        endrule
    end


    //
    // Methods.  
    //

    Vector#(n_READERS, MEMORY_READER_IFC#(t_MEM_ADDRESS, SCRATCHPAD_MEM_VALUE)) portsLocal = newVector();

    for(Integer p = 0; p < valueOf(n_READERS); p = p + 1)
    begin
        portsLocal[p] =
            interface MEMORY_READER_IFC#(t_ADDR, t_DATA);
                method Action readReq(t_MEM_ADDRESS addr) if(initialized);
                    debugLog.record($format("read port %0d: req addr=0x%x", p, addr));
                    let idx <- sortResponseQ[p].enq();
                    // The refInfo for this request is the concatenation of the
                    // port ID and the ROB index.
                    t_REF_INFO ref_info = tuple2(fromInteger(p), idx);

                    // Request data from the cache
                    caches[p].readReq(zeroExtend(pack(addr)), ref_info);
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
                method Bool notFull() = sortResponseQ[p].notFull();
            endinterface;
    end

    interface readPorts = portsLocal;

    method Action write(t_MEM_ADDRESS addr, SCRATCHPAD_MEM_VALUE val);
        // Forward all writes straight to memory
        // Cast up addr immediately
        writeDataQ.enq(RL_DM_CACHE_STORE_REQ{addr:  zeroExtend(pack(addr)), val: val});
    endmethod

    method Bool writeNotFull = writeDataQ.notFull();
endmodule


//
// mkUnmarshalledMultiCacheScratchpad --
//     Allocate a connection to the platform's scratchpad interface for
//     a single scratchpad region, with each reader having its own cache.  
//     No coherence is guranteed. 
//    This module does no marshalling of data sizes. 
             
module [CONNECTED_MODULE] mkUnmarshalledMultiCachedIncoherentScratchpad#(
    Integer scratchpadID, 
    Vector#(n_READERS, Integer) cacheModes,
    Vector#(n_READERS, SCRATCHPAD_STATS_CONSTRUCTOR) cacheStatConstructors,
    Vector#(n_READERS, function CONNECTED_MODULE#(RL_DM_CACHE#(Bit#(t_MEM_ADDRESS_SZ), 
                                                SCRATCHPAD_MEM_VALUE, 
                                                t_REF_INFO)) 
                       f(RL_DM_CACHE_SOURCE_DATA#(Bit#(t_MEM_ADDRESS_SZ), 
                                                  SCRATCHPAD_MEM_VALUE, 
                                                  t_REF_INFO) source,   
                         Bool hashAddresses,
                         DEBUG_FILE debugLog)) cacheConstructors)
                                                          
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
              Add#(a__, t_MEM_ADDRESS_SZ, t_SCRATCHPAD_MEM_ADDRESS_SZ)/*,
              IsModule#(m,c)*/);
    

    // Connection between private cache and the scratchpad virtual device
    RL_DM_CACHE_SOURCE_DATA#(Bit#(t_MEM_ADDRESS_SZ),
                             SCRATCHPAD_MEM_VALUE,
                             MULTI_READ_REF_INFO#(Bit#(n_SAFE_READERS_SZ),
                                                  t_REF_INFO)) sourceData <- 
                                 mkScratchpadCacheSourceData(scratchpadID);

    FIFOF#(RL_DM_CACHE_STORE_REQ#(SCRATCHPAD_MEM_VALUE,
                                  Bit#(t_MEM_ADDRESS_SZ))) writeDataQ <- mkFIFOF; 

    Vector#(n_READERS,RL_DM_CACHE#(Bit#(t_MEM_ADDRESS_SZ), 
                                   SCRATCHPAD_MEM_VALUE, 
                                   t_REF_INFO)) caches 
        = newVector();

    String debugLogFilename = "platform_scratchpad_" + integerToString(scratchpadID - `VDEV_SCRATCH__BASE) + ".out";
    DEBUG_FILE debugLog <- (`PLATFORM_SCRATCHPAD_DEBUG_ENABLE == 1)?
                           mkDebugFile(debugLogFilename):
                           mkDebugFileNull(debugLogFilename); 

 
    // call all the constructors for the caches
    // some setup for each cache is needed.
    for(Integer i = 0; i < valueof(n_READERS); i = i + 1) 
    begin
        // Dummy statistics buckets

        String debugLogCacheFilename = "platform_scratchpad_" + 
                                       integerToString(scratchpadID - 
                                       `VDEV_SCRATCH__BASE) + "_cache_" + 
                                       integerToString(i) +".out";

        DEBUG_FILE debugLogCache <- (`PLATFORM_SCRATCHPAD_DEBUG_ENABLE == 1)?
                           mkDebugFile(debugLogCacheFilename):
                           mkDebugFileNull(debugLogCacheFilename); 

        RL_DM_CACHE_SOURCE_DATA#(Bit#(t_MEM_ADDRESS_SZ),
                                 SCRATCHPAD_MEM_VALUE,
                                 t_REF_INFO) sourceDataWrap <- 
                                     mkMultiReadSourceData(sourceData, i);

        caches[i] <- cacheConstructors[i](sourceDataWrap, False, debugLogCache);
        
        let cacheStats <- cacheStatConstructors[i](caches[i].stats);

    end 

    // Cache responses are not ordered.  Sort them with a reorder buffer.
    Vector#(n_READERS, SCOREBOARD_FIFOF#(SCRATCHPAD_PORT_ROB_SLOTS, SCRATCHPAD_MEM_VALUE)) sortResponseQ <- replicateM(mkScoreboardFIFOF());

    
    // Initialization
    Reg#(Bool) initialized <- mkReg(False);
    rule doInit (! initialized);
        for(Integer i = 0; i < valueof(n_READERS); i = i + 1) 
        begin
            caches[i].setCacheMode(unpack(fromInteger(cacheModes[i])));
        end
        initialized <= True;
    endrule

    rule doWrite;
        writeDataQ.deq;
        t_REF_INFO refInfo = tuple2(0,0); // dummy value
        // Send Write to all caches (keep them all coherent for now)
        caches[0].write(writeDataQ.first.addr, 
                        writeDataQ.first.val,
                        refInfo);
        debugLog.record($format("write addr=0x%x, val=0x%x", writeDataQ.first.addr, writeDataQ.first.val));
    endrule

    // Read requests
    for (Integer p = 0; p < valueOf(n_READERS); p = p + 1)
    begin
        //
        // receiveResp --
        //     Push read responses to the reorder buffer.  They will be returned
        //     through readRsp() in order.
        //
        rule receiveResp;
            let r <- caches[p].readResp();

            // The clientRefInfo field holds the concatenation of the port ID and
            // the port's reorder buffer index.
            match {.port, .idx} = r.refInfo;
            debugLog.record($format("readResp port %d addr=0x%x, val=0x%x", port, idx, r.val));
            sortResponseQ[p].setValue(idx, r.val);
        endrule
    end


    //
    // Methods.  
    //

    Vector#(n_READERS, MEMORY_READER_IFC#(t_MEM_ADDRESS, SCRATCHPAD_MEM_VALUE)) portsLocal = newVector();

    for(Integer p = 0; p < valueOf(n_READERS); p = p + 1)
    begin
        portsLocal[p] =
            interface MEMORY_READER_IFC#(t_ADDR, t_DATA);
                method Action readReq(t_MEM_ADDRESS addr) if(initialized);
                    debugLog.record($format("read port %0d: req addr=0x%x", p, addr));
                    let idx <- sortResponseQ[p].enq();
                    // The refInfo for this request is the concatenation of the
                    // port ID and the ROB index.
                    t_REF_INFO ref_info = tuple2(fromInteger(p), idx);

                    // Request data from the cache
                    caches[p].readReq(zeroExtend(pack(addr)), ref_info);
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
                method Bool notFull() = sortResponseQ[p].notFull();
            endinterface;
    end

    interface readPorts = portsLocal;

    method Action write(t_MEM_ADDRESS addr, SCRATCHPAD_MEM_VALUE val);
        // Forward all writes straight to memory
        // Cast up addr immediately
        writeDataQ.enq(RL_DM_CACHE_STORE_REQ{addr:  zeroExtend(pack(addr)), val: val});
    endmethod

    method Bool writeNotFull = writeDataQ.notFull();
endmodule

