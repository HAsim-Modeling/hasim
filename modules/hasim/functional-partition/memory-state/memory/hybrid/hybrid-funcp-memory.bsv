//
// Copyright (C) 2008 Intel Corporation
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

import FIFO::*;
import FIFOF::*;
import DefaultValue::*;

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_services_lib.bsh"
`include "asim/provides/soft_services_deps.bsh"
`include "asim/provides/common_services.bsh"
`include "asim/provides/mem_services.bsh"
`include "asim/provides/funcp_regstate_base_types.bsh"
`include "asim/provides/rrr.bsh"
`include "asim/provides/channelio.bsh"

`include "asim/rrr/remote_client_stub_FUNCP_MEMORY.bsh"
`include "asim/rrr/remote_server_stub_FUNCP_MEMORY.bsh"

`include "asim/dict/VDEV_CACHE.bsh"
`include "asim/dict/PARAMS_FUNCP_MEMORY.bsh"


// Can't include hasim_isa.bsh here or it causes a loop
typedef MEM_VALUE ISA_ADDRESS;

//
// Temporary until RRR has a real type system
//
typedef Bit#(64) FUNCP_PADDR_RRR;

//
// READ_META passed through the cache.
typedef struct
{
    CONTEXT_ID contextId;
    FUNCP_MEMREF_TOKEN memRefToken;
}
FUNCP_CACHE_READ_META
    deriving (Eq, Bits);


//
// Define the interface for the module that communicates with the host.
//

typedef CENTRAL_CACHE_CLIENT_BACKING#(FUNCP_MEM_WORD_PADDR,
                                      MEM_VALUE,
                                      t_READ_META) FUNCP_CENTRAL_CACHE_BACKING#(type t_READ_META);

//
// One sub-interface is used for passing invalidation requests to the top level
// of the local functional memory cache.
//
interface FUNCP_MEM_INVAL_IFC;
    method ActionValue#(Tuple2#(MEM_ADDRESS, Bool)) getReq();
    method Action sendResp();
endinterface: FUNCP_MEM_INVAL_IFC

//
// Full host communication interface.
//
interface FUNCP_MEM_HOST_IFC#(type t_READ_META);
    // Cache backing storage interface
    interface FUNCP_CENTRAL_CACHE_BACKING#(t_READ_META) cacheBacking;

    // Invalidation / flush request interface
    interface FUNCP_MEM_INVAL_IFC inval;
endinterface: FUNCP_MEM_HOST_IFC


// Low address bits indexing base ISA data size.  Addresses come in from the
// model pointing to a byte.  The cache points to a word.
typedef TLog#(TDiv#(`FUNCP_ISA_INT_REG_SIZE, 8)) FUNCP_MEM_ISA_WORD_OFFSET_BITS;
typedef Bit#(FUNCP_MEM_ISA_WORD_OFFSET_BITS)     FUNCP_MEM_ISA_WORD_OFFSET;

typedef Bit#(TSub#(`FUNCP_ISA_P_ADDR_SIZE, FUNCP_MEM_ISA_WORD_OFFSET_BITS)) FUNCP_MEM_WORD_PADDR;

function FUNCP_MEM_WORD_PADDR wordAddrFromByteAddr(MEM_ADDRESS addr);
    Tuple2#(FUNCP_MEM_WORD_PADDR, FUNCP_MEM_ISA_WORD_OFFSET) a = unpack(addr);
    return tpl_1(a);
endfunction

function MEM_ADDRESS byteAddrFromWordAddr(FUNCP_MEM_WORD_PADDR addr);
    FUNCP_MEM_ISA_WORD_OFFSET w = 0;
    return { addr, w };
endfunction


// ***** Modules *****

// mkFuncpMemory

module [HASIM_MODULE] mkFUNCP_Memory
    // interface:
    ();

    // ***** Local State *****
    
    DEBUG_FILE debugLog <- mkDebugFile("hasim_funcp_memory.out");

    // Dynamic parameters
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();
    Param#(3) cacheMode <- mkDynamicParameter(`PARAMS_FUNCP_MEMORY_FUNCP_MEM_PVT_CACHE_MODE, paramNode);
    Param#(3) prefetchConfig <- mkDynamicParameter(`PARAMS_FUNCP_MEMORY_FUNCP_MEM_PVT_PREFETCH_MODE, paramNode);

    function Bool prefetchEnableFillFromHost() = unpack(~prefetchConfig[2]);
    function Bool prefetchAllRefTypes() = unpack(prefetchConfig[1]);
    function RL_DM_CACHE_PREFETCH_MODE prefetchMode() =
        ((prefetchConfig[0] == 0) ? RL_DM_PREFETCH_DISABLE : RL_DM_PREFETCH_ENABLE);

    // Debugging output stream, useful for getting a stream of status messages
    // when running on an FPGA.
    STDIO#(Bit#(64)) stdio <- mkStdIO_Debug();

    // Links that we expose to the outside world
    Connection_Server#(MEM_REQUEST, MEMSTATE_RESP) linkMemory <- mkConnection_Server("funcp_memory");

    // Connection between the central cache and remote functional memory
    let remoteFuncpMem <- mkRemoteFuncpMem(prefetchEnableFillFromHost, debugLog);

    // Local functional memory cache and cache prefetcher
    NumTypeParam#(`FUNCP_PVT_CACHE_ENTRIES) num_pvt_entries = ?;
    
    let prefetcher <- mkFuncpCachePrefetcher(prefetchAllRefTypes, debugLog);
    
    CENTRAL_CACHE_CLIENT#(FUNCP_MEM_WORD_PADDR, MEM_VALUE, FUNCP_CACHE_READ_META) cache <-
        mkCentralCacheClient(`VDEV_CACHE_FUNCP_MEMORY,
                             num_pvt_entries,
                             prefetcher,
                             True,
                             remoteFuncpMem.cacheBacking);

    // Private cache and prefetch statistics
    let stats <- mkFuncpMemPvtCacheStats(cache.stats);

    // Invalidate requests
    FIFOF#(Tuple2#(FUNCP_MEM_WORD_PADDR, Bool)) invalQ <- mkFIFOF();

    // Debug messages
    let msgLD_Req <- getGlobalStringUID("FUNCP Mem: LD REQ  ctx=%d, tok=%d, PA=0x%016llx, w_addr=0x%016llx\n");
    let msgLD_Rsp <- getGlobalStringUID("FUNCP Mem: LD RSP  tok=%d, val=0x%016llx\n");
    let msgST     <- getGlobalStringUID("FUNCP Mem: ST PA   ctx=%d, PA=0x%016llx, w_addr=0x%016llx, val=0x%016llx\n");


    // ====================================================================
    //
    // Initialization
    //
    // ====================================================================

    Reg#(Bool) initialized <- mkReg(False);
    rule doInit (! initialized);
        cache.setCacheMode(unpack(cacheMode[1:0]), prefetchMode);
        initialized <= True;
    endrule


    // ====================================================================
    //
    // Debug scan state
    //
    // ====================================================================

    COUNTER#(5) loadsInFlight <- mkLCounter(0);

    //
    // Debug state that can be scanned out.
    //
    DEBUG_SCAN_FIELD_LIST dbg_list = List::nil;
    dbg_list <- addDebugScanField(dbg_list, "Loads in flight", loadsInFlight.value());
    dbg_list <- addDebugScanField(dbg_list, "invalQ not empty", invalQ.notEmpty);

    let dbgNode <- mkDebugScanNode("FUNCP Memory", dbg_list);


    // ====================================================================
    //
    // Main rules
    //
    // ====================================================================

    //
    // handleMemReq --
    //     Service memory requests from the model.
    //
    rule handleMemReq (initialized);
        let req = linkMemory.getReq();
        linkMemory.deq();
        
        case (req) matches
            tagged MEM_LOAD .ldinfo:
            begin
                let read_meta = FUNCP_CACHE_READ_META { contextId: ldinfo.contextId,
                                                        memRefToken: ldinfo.memRefToken };
                let w_addr = wordAddrFromByteAddr(ldinfo.addr);
                cache.readReq(w_addr, read_meta, defaultValue());

                loadsInFlight.up();
                debugLog.record($format("cache readReq: ctx=%0d, addr=0x%x, w_addr=0x%x", ldinfo.contextId, ldinfo.addr, w_addr));

                stdio.printf(msgLD_Req, list(zeroExtend(ldinfo.contextId),
                                             zeroExtend(pack(ldinfo.memRefToken)),
                                             zeroExtend(ldinfo.addr),
                                             zeroExtend(w_addr)));
            end
            
            tagged MEM_STORE .stinfo:
            begin
                let w_addr = wordAddrFromByteAddr(stinfo.addr);
                cache.write(w_addr, stinfo.val);
                debugLog.record($format("cache write: ctx=%0d, addr=0x%x, w_addr=0x%x, val=0x%x", stinfo.contextId, stinfo.addr, w_addr, stinfo.val));

                stdio.printf(msgST, list(zeroExtend(stinfo.contextId),
                                         zeroExtend(stinfo.addr),
                                         zeroExtend(w_addr),
                                         zeroExtend(stinfo.val)));
            end
        endcase

    endrule
  
    //
    // getMemResp --
    //     Return load response from the cache to the model.
    //
    rule getMemResp (True);
        let r <- cache.readResp();
        linkMemory.makeResp(memStateRespLoad(r.readMeta.memRefToken, r.val));

        loadsInFlight.down();
        debugLog.record($format("cache readResp: val=0x%x", r.val));
        stdio.printf(msgLD_Rsp, list(zeroExtend(pack(r.readMeta.memRefToken)),
                                     resize(r.val)));
    endrule


    //
    // getInvalidateReq --
    //     Process incoming invalidation requests from the host and send
    //     then on to processInvalidateReq.
    //
    rule getInvalidateReq (initialized);
        let r <- remoteFuncpMem.inval.getReq();
        match {.addr, .only_flush} = r;

        let w_addr = wordAddrFromByteAddr(addr);
        
        invalQ.enq(tuple2(w_addr, only_flush));
        debugLog.record($format("cache flush/inval req: addr=0x%x, w_addr=0x%x", addr, w_addr));
    endrule

    //
    // processInvalidateReq --
    //     Invalidate all words in a line.
    //
    Reg#(Bit#(TLog#(FUNCP_MEM_CACHELINE_WORDS))) invalWordIdx <- mkReg(0);

    (* descending_urgency = "processInvalidateReq, handleMemReq" *)
    rule processInvalidateReq (True);
        match {.addr, .only_flush} = invalQ.first();

        Bool lastWordInLine = (invalWordIdx == maxBound);
        if (lastWordInLine)
            invalQ.deq();

        // Invalidate the next word in the line.  The software side guarantees
        // the address is line-aligned, so the OR operation works.
        FUNCP_MEM_WORD_PADDR w_addr = addr | zeroExtend(invalWordIdx);
        invalWordIdx <= invalWordIdx + 1;

        if (only_flush)
        begin
            cache.flushReq(w_addr, lastWordInLine);
            debugLog.record($format("cache flush: w_addr=0x%x", w_addr));
        end
        else
        begin
            cache.invalReq(w_addr, lastWordInLine);
            debugLog.record($format("cache inval: w_addr=0x%x", w_addr));
        end
    endrule

    //
    // sendInvalidateResp --
    //     Remote client waits for a write back to complete.  Notify when done.
    //
    rule sendInvalidateResp (True);
        cache.invalOrFlushWait();
        remoteFuncpMem.inval.sendResp();
        debugLog.record($format("cache inval DONE"));
    endrule

endmodule


//
// mkRemoteFuncpMem --
//     Connection between the central cache and the remote functional memory
//     service.
//
module [HASIM_MODULE] mkRemoteFuncpMem#(Bool prefetchEnableFillFromHost,
                                        DEBUG_FILE debugLog)
    // interface:
    (FUNCP_MEM_HOST_IFC#(t_READ_META))
    provisos (Alias#(t_READ_META, RL_DM_CACHE_READ_META#(FUNCP_CACHE_READ_META)));
    
    // Stubs for host functional memory communication.
    ServerStub_FUNCP_MEMORY serverStub <- mkServerStub_FUNCP_MEMORY();
    ClientStub_FUNCP_MEMORY clientStub <- mkClientStub_FUNCP_MEMORY();

    //
    // Buffered store state to merge control and data messages into a single
    // RRR message.
    //
    FIFO#(Tuple3#(FUNCP_MEM_CACHELINE_WORD_VALID_MASK,
                  Bool,
                  FUNCP_MEM_WORD_PADDR)) stCtrlQ <- mkFIFO();

    Reg#(Bit#(TLog#(FUNCP_MEM_CACHELINE_WORDS))) rdWordIdx <- mkReg(0);
    FIFO#(Bool) rdIsCacheableQ <- mkFIFO();
    MARSHALLER#(MEM_VALUE,
                Vector#(FUNCP_MEM_CACHELINE_WORDS, MEM_VALUE)) rdRespMar <-
       mkSimpleMarshaller();

    Reg#(Bit#(TLog#(FUNCP_MEM_CACHELINE_WORDS))) stWordIdx <- mkReg(0);
    Reg#(FUNCP_MEM_CACHELINE) stData <- mkRegU();

    //
    // Track requests that are just prefetches.  These will be squashed
    // by returning "uncacheable" instead of sending them to the host.
    //
    FIFO#(RL_CACHE_GLOBAL_READ_META) reqMeta <-
        mkSizedFIFO(valueOf(MAX_FUNCP_INFLIGHT_MEMREFS));

    //
    // Marshall incoming read responses
    //
    rule compressReadRsp (True);
        let meta = reqMeta.first();
        reqMeta.deq();

        OUT_TYPE_LoadLine r;

        if (prefetchEnableFillFromHost || ! meta.isPrefetch)
        begin
            // Normal path.  Get the result from the host.
            r <- clientStub.getResponse_LoadLine();
        end
        else
        begin
            // Prefetch requests never went to the host.
            r.data0 = ?;
            r.data1 = ?;
            r.data2 = ?;
            r.data3 = ?;
            r.isCacheable = 0;
        end

        rdIsCacheableQ.enq(unpack(r.isCacheable[0]));
        rdRespMar.enq(unpack({ r.data3, r.data2, r.data1, r.data0 }));
    endrule


    //
    // Interface between host functional memory and the central cache.
    //
    interface FUNCP_CENTRAL_CACHE_BACKING cacheBacking;
        //
        // readLineReq --
        //     Request a full line of data.
        //
        method Action readLineReq(FUNCP_MEM_WORD_PADDR wAddr,
                                  t_READ_META readMeta,
                                  RL_CACHE_GLOBAL_READ_META globalReadMeta);
            let client_meta = readMeta.clientReadMeta;
            let addr = byteAddrFromWordAddr(wAddr);
            debugLog.record($format("back readReq: ctx=%0d, pref=%b, addr=0x%x", client_meta.contextId, globalReadMeta.isPrefetch, addr));

            reqMeta.enq(globalReadMeta);
            if (prefetchEnableFillFromHost || ! globalReadMeta.isPrefetch)
            begin
                clientStub.makeRequest_LoadLine(zeroExtend(addr),
                                                zeroExtend(pack(globalReadMeta.isPrefetch)));
            end
        endmethod

        //
        // readResp --
        //     Pick the next word from the line-sized response.
        //
        method ActionValue#(Tuple2#(MEM_VALUE, Bool)) readResp();
            let val = rdRespMar.first();
            rdRespMar.deq();

            let is_cacheable = rdIsCacheableQ.first();
            if (rdWordIdx == maxBound)
            begin
                rdIsCacheableQ.deq();
            end
            rdWordIdx <= rdWordIdx + 1;

            debugLog.record($format("back readResp: idx=%0d, cache=%b, val=0x%x", rdWordIdx, is_cacheable, val));
            return tuple2(val, is_cacheable);
        endmethod

        //
        // writeLineReq --
        //     Begin a store transaction, sending the control information.  The
        //     data will follow in calls to writeData() below.
        //
        //     NOTE:  Bluespec will trigger an error here if the words per line
        //            doesn't match the configuration of the central cache.
        //            The central cache currently requires 4 words per line.
        //
        method Action writeLineReq(FUNCP_MEM_WORD_PADDR wAddr,
                                   FUNCP_MEM_CACHELINE_WORD_VALID_MASK wordValidMask,
                                   Bool sendAck) if (stWordIdx == 0);
            let addr = byteAddrFromWordAddr(wAddr);
            debugLog.record($format("back writeCtrl: addr=0x%x, valid=0x%x, ack=%d", addr, pack(wordValidMask), pack(sendAck)));

            stCtrlQ.enq(tuple3(wordValidMask, sendAck, wAddr));
        endmethod

        //
        // writeData --
        //     Forward data associated with writeLineReq() above.
        //
        method Action writeData(MEM_VALUE val);
            debugLog.record($format("back writeData: idx=%0d, val=0x%x", stWordIdx, val));

            let sd = shiftInAtN(stData, val);
            stData <= sd;

            if (stWordIdx == maxBound)
            begin
                // Send the store
                match {.word_valid_mask, .send_ack, .w_addr} = stCtrlQ.first();
                stCtrlQ.deq();

                clientStub.makeRequest_StoreLine(zeroExtend(pack(word_valid_mask)),
                                                 zeroExtend(pack(send_ack)),
                                                 zeroExtend(byteAddrFromWordAddr(w_addr)),
                                                 sd[0],
                                                 sd[1],
                                                 sd[2],
                                                 sd[3]);
            end

            stWordIdx <= stWordIdx + 1;
        endmethod

        method Action writeAckWait();
            let dummy <- serverStub.acceptRequest_StoreACK();
            debugLog.record($format("back write ACK"));
        endmethod
    endinterface


    //
    // Interface between host functional memory and the top of the local
    // cache.
    //
    interface FUNCP_MEM_INVAL_IFC inval;
        // Incoming line invalidation request
        method ActionValue#(Tuple2#(MEM_ADDRESS, Bool)) getReq();
            let r <- serverStub.acceptRequest_Invalidate();
            return tuple2(truncate(r.addr),
                          unpack(truncate(r.onlyFlush)));
        endmethod

        // ACK that flush is complete
        method Action sendResp();
            serverStub.sendResponse_Invalidate(?);
        endmethod
    endinterface
endmodule


//
// mkFuncpMemPvtCacheStats --
//     Statistics callbacks from private cache in front of the central cache.
//
module [HASIM_MODULE] mkFuncpMemPvtCacheStats#(RL_CACHE_STATS stats)
    // interface:
    ();
    
    STAT_ID statIDs[2] = {
        statName("FUNCP_MEMORY_PVT_CACHE_LOAD_HIT",
                 "FUNCP Mem: Private cache load hits"),
        statName("FUNCP_MEMORY_PVT_CACHE_LOAD_MISS",
                 "FUNCP Mem: Private cache load misses")
    };

    STAT_VECTOR#(2) sv <- mkStatCounter_Vector(statIDs);

    rule readHit (stats.readHit());
        sv.incr(0);
    endrule

    rule readMiss (stats.readMiss());
        sv.incr(1);
    endrule

endmodule



//
// mkFuncpCachePrefetcher --
//     The learning prefetcher in libRL is not successful for functional memory.
//     We use a very simple prefetcher.
//
//     The L1 private cache holds words.  The central cache holds lines.
//     This prefetcher loads the remainder of a line into the L1 cache as fill
//     responses arrive from the central cache.  The central cache is optimized
//     for repeated references to the same line, making the prefetches
//     relatively inexpensive.
//
module mkFuncpCachePrefetcher#(Bool prefetchAllRefTypes, DEBUG_FILE debugLog)
    // interface:
    (CACHE_PREFETCHER#(t_CACHE_IDX, FUNCP_MEM_WORD_PADDR, FUNCP_CACHE_READ_META))
    provisos (Bits#(t_CACHE_IDX, t_CACHE_IDX_SZ),
              Alias#(t_CACHE_ADDR, FUNCP_MEM_WORD_PADDR),
              Alias#(t_CACHE_READ_META, FUNCP_CACHE_READ_META),

              Bits#(MEM_VALUE, t_WORD_SZ),
              Bits#(t_CACHE_ADDR, t_CACHE_ADDR_SZ),

              // Word index within a cache line
              NumAlias#(t_WORD_IDX_SZ, TLog#(FUNCP_MEM_CACHELINE_WORDS)),
              Alias#(t_WORD_IDX, Bit#(t_WORD_IDX_SZ)),

              // Line index within a 4KB page (excluding word index)
              NumAlias#(t_LINE_IDX_SZ, TSub#(TLog#(TDiv#(TMul#(4096, 8), t_WORD_SZ)),
                                             t_WORD_IDX_SZ)),
              Alias#(t_LINE_IDX, Bit#(t_LINE_IDX_SZ)),

              // Page index within physical memory space, excluding word and
              // line indices.
              NumAlias#(t_PAGE_IDX_SZ, TSub#(t_CACHE_ADDR_SZ,
                                             TAdd#(t_LINE_IDX_SZ, t_WORD_IDX_SZ))),
              Alias#(t_PAGE_IDX, Bit#(t_PAGE_IDX_SZ)));
    
    //
    // Address manipulation functions.
    //
    function Tuple3#(t_PAGE_IDX,
                     t_LINE_IDX,
                     t_WORD_IDX) burstAddr(FUNCP_MEM_WORD_PADDR addr) =
        unpack(addr);

    function FUNCP_MEM_WORD_PADDR packAddr(t_PAGE_IDX pageIdx,
                                           t_LINE_IDX lineIdx,
                                           t_WORD_IDX wordIdx) =
        { pageIdx, lineIdx, wordIdx };



    Wire#(FUNCP_MEM_WORD_PADDR) newReq <- mkWire();
    FIFOF#(FUNCP_MEM_WORD_PADDR) prefReqQ <- mkSizedFIFOF(4);

    (* fire_when_enabled *)
    rule fwdReq (True);
        prefReqQ.enq(newReq);
        debugLog.record($format(" prefetch enq: addr=0x%x", newReq));
    endrule

    function PREFETCH_REQ#(t_CACHE_ADDR, t_CACHE_READ_META) genReq();
        return PREFETCH_REQ { addr: prefReqQ.first(),
                              readMeta: ?,
                              prio: PREFETCH_PRIO_LOW };
    endfunction

    method Action setPrefetchMode(Tuple2#(PREFETCH_MODE, PREFETCH_DIST_PARAM) mode,
                                  PREFETCH_LEARNER_SIZE_LOG size);
        noAction;
    endmethod

    method Bool hasReq() = prefReqQ.notEmpty;

    method ActionValue#(PREFETCH_REQ#(t_CACHE_ADDR, t_CACHE_READ_META)) getReq();
        prefReqQ.deq();    
        return genReq();
    endmethod

    method PREFETCH_REQ#(t_CACHE_ADDR, t_CACHE_READ_META) peekReq();
        return genReq();
    endmethod

    method Action readHit(t_CACHE_IDX idx, t_CACHE_ADDR addr);
        noAction;
    endmethod

    method Action readMiss(t_CACHE_IDX idx,
                           t_CACHE_ADDR addr,
                           Bool isPrefetch,
                           t_CACHE_READ_META readMeta);
        noAction;
    endmethod

    method Action prefetchInval(t_CACHE_IDX idx);
        noAction;
    endmethod

    method Action shuntNewCacheReq(t_CACHE_IDX idx, t_CACHE_ADDR addr);
        noAction;
    endmethod

    method Action fillResp(t_CACHE_IDX idx,
                           t_CACHE_ADDR addr,
                           Bool isPrefetch,
                           t_CACHE_READ_META readMeta);

        match {.p_idx, .l_idx, .w_idx} = burstAddr(addr);

        // Is the fill response an instruction reference (or prefetching all
        // request types)?  Is the address not the last entry in a cache line?
        if ((prefetchAllRefTypes || (readMeta.memRefToken.memPath == FUNCP_REGSTATE_MEMPATH_GETINST)) &&
            (w_idx != maxBound))
        begin
            // Prefetch the next word in the line.  The first level cache
            // elements are just words.  The central cache holds lines.
            // A recently read cache in the central cache reduces access
            // time for multiple references to the same line.
            let pref_addr = packAddr(p_idx, l_idx, w_idx + 1);
            newReq <= pref_addr;
            debugLog.record($format(" prefetch try: addr=0x%x (fill=0x%x)", pref_addr, addr));
        end
    endmethod

    method Action prefetchDroppedByBusy(t_CACHE_ADDR addr);
        noAction;
    endmethod

    method Action prefetchDroppedByHit();
        noAction;
    endmethod

    method Action prefetchIllegalReq();
        noAction;
    endmethod

    interface RL_PREFETCH_STATS stats = error("Not implemented");

endmodule
