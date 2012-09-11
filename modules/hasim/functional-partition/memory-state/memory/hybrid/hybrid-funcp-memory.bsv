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

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_services_lib.bsh"
`include "asim/provides/soft_services_deps.bsh"
`include "asim/provides/common_services.bsh"
`include "asim/provides/mem_services.bsh"
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

    // Debugging output stream, useful for getting a stream of status messages
    // when running on an FPGA.
    STDIO#(Bit#(64)) stdio <- mkStdIO_Debug();

    // Links that we expose to the outside world
    Connection_Server#(MEM_REQUEST, MEMSTATE_RESP) linkMemory <- mkConnection_Server("funcp_memory");

    // Connection between the central cache and remote functional memory
    let remoteFuncpMem <- mkRemoteFuncpMem(debugLog);

    // Local functional memory cache and cache prefetcher
    NumTypeParam#(`FUNCP_PVT_CACHE_ENTRIES) num_pvt_entries = ?;
    NumTypeParam#(`FUNCP_PVT_CACHE_PREFETCH_LEARNER_NUM) num_prefetch_learners = ?;
    
//    let prefetcher <- (`FUNCP_PVT_CACHE_PREFETCH_ENABLE == 1)?
//                      mkCachePrefetcher(num_prefetch_learners, True, debugLog):
//                      mkNullCachePrefetcher();
    let prefetcher <- mkCachePrefetcher(num_prefetch_learners, True, debugLog);
    
    CENTRAL_CACHE_CLIENT#(FUNCP_MEM_WORD_PADDR, MEM_VALUE, FUNCP_CACHE_READ_META) cache <-
        mkCentralCacheClient(`VDEV_CACHE_FUNCP_MEMORY,
                             num_pvt_entries,
                             prefetcher,
                             True,
                             remoteFuncpMem.cacheBacking);

    // Private cache and prefetch statistics
    let stats <- mkFuncpMemPvtCacheStats(cache.stats);
    let prefetchStats <- (`FUNCP_PVT_CACHE_PREFETCH_ENABLE == 1)?
                         mkFuncpMemPvtPrefetchStats(num_prefetch_learners, prefetcher.stats):
                         mkNullFuncpMemPvtPrefetchStats(num_prefetch_learners, prefetcher.stats);

    // Dynamic parameters
    PARAMETER_NODE paramNode <- mkDynamicParameterNode();
    Param#(3) cacheMode <- mkDynamicParameter(`PARAMS_FUNCP_MEMORY_FUNCP_MEM_PVT_CACHE_MODE, paramNode);
    Param#(5) prefetchMechanism <- mkDynamicParameter(`PARAMS_FUNCP_MEMORY_FUNCP_MEM_PREFETCHER_MECHANISM, paramNode);
    Param#(3) prefetchLearnerSizeLog <- mkDynamicParameter(`PARAMS_FUNCP_MEMORY_FUNCP_MEM_PREFETCHER_LEARNER_SIZE_LOG, paramNode);

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
        cache.setCacheMode(unpack(cacheMode[1:0]), unpack(cacheMode[2]));
        prefetcher.setPrefetchMode(unpack(prefetchMechanism),unpack(prefetchLearnerSizeLog));
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
                cache.readReq(w_addr, read_meta);

                loadsInFlight.up();
                debugLog.record($format("cache readReq: ctx=%0d, addr=0x%x, w_addr=0x%x", ldinfo.contextId, ldinfo.addr, w_addr));

                stdio.printf(msgLD_Req, list4(zeroExtend(ldinfo.contextId),
                                              zeroExtend(pack(ldinfo.memRefToken)),
                                              zeroExtend(ldinfo.addr),
                                              zeroExtend(w_addr)));
            end
            
            tagged MEM_STORE .stinfo:
            begin
                let w_addr = wordAddrFromByteAddr(stinfo.addr);
                cache.write(w_addr, stinfo.val);
                debugLog.record($format("cache write: ctx=%0d, addr=0x%x, w_addr=0x%x, val=0x%x", stinfo.contextId, stinfo.addr, w_addr, stinfo.val));

                stdio.printf(msgST, list4(zeroExtend(stinfo.contextId),
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
        linkMemory.makeResp(memStateResp(r.readMeta.memRefToken, r.val));

        loadsInFlight.down();
        debugLog.record($format("cache readResp: val=0x%x", r.val));
        stdio.printf(msgLD_Rsp, list2(zeroExtend(pack(r.readMeta.memRefToken)),
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
module [HASIM_MODULE] mkRemoteFuncpMem#(DEBUG_FILE debugLog)
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

    Reg#(Bit#(TLog#(FUNCP_MEM_CACHELINE_WORDS))) stWordIdx <- mkReg(0);
    Reg#(FUNCP_MEM_CACHELINE) stData <- mkRegU();

    //
    // Buffered load state to convert a full line RRR response to word-sized
    // messages to the cache.
    //
    Reg#(Bit#(TLog#(FUNCP_MEM_CACHELINE_WORDS))) ldWordIdx <- mkReg(0);

    //
    // Interface between host functional memory and the central cache.
    //
    interface FUNCP_CENTRAL_CACHE_BACKING cacheBacking;
        //
        // readLineReq --
        //     Request a full line of data.
        //
        method Action readLineReq(FUNCP_MEM_WORD_PADDR wAddr, t_READ_META readMeta);
            let client_meta = readMeta.clientReadMeta;
            let addr = byteAddrFromWordAddr(wAddr);
            debugLog.record($format("back readReq: ctx=%0d, pref=%b, addr=0x%x", client_meta.contextId, readMeta.isPrefetch, addr));
            clientStub.makeRequest_LoadLine(zeroExtend(addr),
                                            zeroExtend(pack(readMeta.isPrefetch)));
        endmethod

        //
        // readResp --
        //     Pick the next word from the line-sized response.
        //
        method ActionValue#(Tuple2#(MEM_VALUE, Bool)) readResp();
            // Pick a word from the current incoming value.  Pop the entry if on
            // the last word.
            OUT_TYPE_LoadLine r;
            if (rdWordIdx == maxBound)
                r <- clientStub.getResponse_LoadLine();
            else
                r = clientStub.peekResponse_LoadLine();

            FUNCP_MEM_CACHELINE line;
            line[0] = r.data0;
            line[1] = r.data1;
            line[2] = r.data2;
            line[3] = r.data3;

            Bool is_cacheable = unpack(r.isCacheable[0]);

            let val = line[rdWordIdx];
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

            if (stWordIdx != maxBound)
            begin
                // Still collecting data
                stData[stWordIdx] <= val;
            end
            else
            begin
                // Send the store
                match {.word_valid_mask, .send_ack, .w_addr} = stCtrlQ.first();
                stCtrlQ.deq();

                clientStub.makeRequest_StoreLine(zeroExtend(pack(word_valid_mask)),
                                                 zeroExtend(pack(send_ack)),
                                                 zeroExtend(byteAddrFromWordAddr(w_addr)),
                                                 stData[0],
                                                 stData[1],
                                                 stData[2],
                                                 val);
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
// mkFuncpMemPvtPrefetchStats --
//     Statistics callbacks from private cache's prefetcher in front of the central cache.
//
module [HASIM_MODULE] mkFuncpMemPvtPrefetchStats#(NumTypeParam#(n_LEARNERS) dummy, RL_PREFETCH_STATS stats)
    // interface:
    ()
    provisos( NumAlias#(TMul#(n_LEARNERS, 4), n_STATS),
              Add#(TMax#(TLog#(n_STATS),1), extraBits, TLog#(`STATS_MAX_VECTOR_LEN)));
	
	STAT_ID prefetchStatIDs[6];
	STAT_ID learnerStatIDs[ valueOf(n_STATS) ];

	prefetchStatIDs[0] = statName("FUNCP_MEMORY_PVT_PREFETCH_HIT", 
                                  "FUNCP Mem: Prefetch hits");
	prefetchStatIDs[1] = statName("FUNCP_MEMORY_PVT_PREFETCH_DROP_BUSY", 
                                  "FUNCP Mem: Prefetch reqs dropped by busy");
	prefetchStatIDs[2] = statName("FUNCP_MEMORY_PVT_PREFETCH_DROP_HIT", 
                                  "FUNCP Mem: Prefetch reqs dropped by hit");
	prefetchStatIDs[3] = statName("FUNCP_MEMORY_PVT_PREFETCH_LATE", 
                                  "FUNCP Mem: Late prefetch reqs");
	prefetchStatIDs[4] = statName("FUNCP_MEMORY_PVT_PREFETCH_USELESS", 
                                  "FUNCP Mem: Uesless prefetch reqs");
	prefetchStatIDs[5] = statName("FUNCP_MEMORY_PVT_PREFETCH_ISSUE", 
                                  "FUNCP Mem: Prefetch reqs issued");
	
	for (Integer i = 0; i < valueOf(n_LEARNERS); i = i+1)
	begin
	    learnerStatIDs[0+4*i] = statName("FUNCP_MEMORY_PVT_PREFETCH_L"+integerToString(i)+"_HIT",
                                         "FUNCP Mem: Prefetch learner "+integerToString(i)+" hits");
	    learnerStatIDs[1+4*i] = statName("FUNCP_MEMORY_PVT_PREFETCH_L"+integerToString(i)+"_ISSUE", 
                                         "FUNCP Mem: Prefetch reqs from learner "+integerToString(i));
	    learnerStatIDs[2+4*i] = statName("FUNCP_MEMORY_PVT_PREFETCH_L"+integerToString(i)+"_STRIDE", 
                                         "FUNCP Mem: Prefetch stride from learner "+integerToString(i));
	    learnerStatIDs[3+4*i] = statName("FUNCP_MEMORY_PVT_PREFETCH_L"+integerToString(i)+"_LA_DIST", 
                                         "FUNCP Mem: Prefetch lookahead dist from learner "+integerToString(i));
	end

    STAT_VECTOR#(6)       prefetchSv <- mkStatCounter_Vector(prefetchStatIDs);
    STAT_VECTOR#(n_STATS) learnerSv  <- mkStatCounter_Vector(learnerStatIDs);
    
    rule prefetchHit (stats.prefetchHit());
        prefetchSv.incr(0);
    endrule

    rule prefetchDroppedByBusy (stats.prefetchDroppedByBusy());
        prefetchSv.incr(1);
    endrule

    rule prefetchDroppedByHit (stats.prefetchDroppedByHit());
        prefetchSv.incr(2);
    endrule

    rule prefetchLate (stats.prefetchLate());
        prefetchSv.incr(3);
    endrule

    rule prefetchUseless (stats.prefetchUseless());
        prefetchSv.incr(4);
    endrule
    
    rule prefetchIssued (stats.prefetchIssued());
        prefetchSv.incr(5);
    endrule
	
	rule hitLearnerUpdate (stats.hitLearnerInfo() matches tagged Valid .s);
        learnerSv.incr(0+resize(s.idx)*4); //hitLeanerIdx
        if (s.isActive)                    //the learner is issuing prefetch request
        begin
            learnerSv.incr(1+resize(s.idx)*4);                         //activeLearnerIdx
            learnerSv.incrBy(2+resize(s.idx)*4, signExtend(s.stride)); //activeLearnerStride
            learnerSv.incrBy(3+resize(s.idx)*4, zeroExtend(s.laDist)); //activeLearnerLaDist
        end
	endrule
    
endmodule


module [HASIM_MODULE] mkNullFuncpMemPvtPrefetchStats#(NumTypeParam#(n_LEARNERS) dummy, RL_PREFETCH_STATS stats)
    // interface:
    ();
endmodule
