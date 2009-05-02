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

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/platform_interface.bsh"
`include "asim/provides/rrr.bsh"
`include "asim/provides/channelio.bsh"

`include "asim/rrr/remote_client_stub_FUNCP_MEMORY.bsh"
`include "asim/rrr/remote_server_stub_FUNCP_MEMORY.bsh"

`include "asim/dict/VDEV_CACHE.bsh"

// Can't include hasim_isa.bsh here or it causes a loop
typedef MEM_VALUE ISA_ADDRESS;

//
// Temporary until RRR has a real type system
//
typedef Bit#(64) FUNCP_PADDR_RRR;

//
// REF_INFO passed through the cache.
typedef struct
{
    CONTEXT_ID contextId;
    FUNCP_MEMREF_TOKEN memRefToken;
}
FUNCP_CACHE_REF_INFO
    deriving (Eq, Bits);


//
// Define the interface for the module that communicates with the host.
//

typedef CENTRAL_CACHE_CLIENT_BACKING#(FUNCP_MEM_WORD_PADDR, MEM_VALUE, FUNCP_CACHE_REF_INFO) FUNCP_CENTRAL_CACHE_BACKING;

//
// One sub-interface is used for passing invalidation requests to the top level
// of the local functional memory cache.
//
interface FUNCP_MEM_INVAL_IFC;
    method ActionValue#(Tuple3#(CONTEXT_ID, MEM_ADDRESS, Bool)) getReq();
    method Action sendResp();
endinterface: FUNCP_MEM_INVAL_IFC

//
// Full host communication interface.
//
interface FUNCP_MEM_HOST_IFC;
    // Cache backing storage interface
    interface FUNCP_CENTRAL_CACHE_BACKING cacheBacking;

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

    // Links that we expose to the outside world
    Connection_Server#(MEM_REQUEST, MEMSTATE_RESP) linkMemory <- mkConnection_Server("funcp_memory");

    // Connection between the central cache and remote functional memory
    FUNCP_MEM_HOST_IFC remoteFuncpMem <- mkRemoteFuncpMem(debugLog);

    // Local functional memory cache
    CENTRAL_CACHE_CLIENT#(FUNCP_MEM_WORD_PADDR, MEM_VALUE, FUNCP_CACHE_REF_INFO) cache <-
        mkCentralCacheClient(`VDEV_CACHE_FUNCP_MEM, remoteFuncpMem.cacheBacking);


    //
    // handleMemReq --
    //     Service memory requests from the model.
    //
    rule handleMemReq (True);
        let req = linkMemory.getReq();
        linkMemory.deq();
        
        case (req) matches
            tagged MEM_LOAD .ldinfo:
            begin
                let ref_info = FUNCP_CACHE_REF_INFO { contextId: ldinfo.contextId,
                                                      memRefToken: ldinfo.memRefToken };
                let w_addr = wordAddrFromByteAddr(ldinfo.addr);
                cache.readReq(w_addr, ref_info);
                debugLog.record($format("cache readReq: ctx=%0d, addr=0x%x, w_addr=0x%x", ldinfo.contextId, ldinfo.addr, w_addr));
            end
            
            tagged MEM_STORE .stinfo:
            begin
                let ref_info = FUNCP_CACHE_REF_INFO { contextId: stinfo.contextId,
                                                      memRefToken: ? };
                let w_addr = wordAddrFromByteAddr(stinfo.addr);
                cache.write(w_addr, stinfo.val, ref_info);
                debugLog.record($format("cache write: ctx=%0d, addr=0x%x, w_addr=0x%x, val=0x%x", stinfo.contextId, stinfo.addr, w_addr, stinfo.val));
            end
        endcase

    endrule
  
    //
    // getMemResp --
    //     Return load response from the cache to the model.
    //
    rule getMemResp (True);
        let r <- cache.readResp();
        linkMemory.makeResp(memStateResp(r.refInfo.memRefToken, r.val));

        debugLog.record($format("cache readResp: val=0x%x", r.val));
    endrule


    //
    // Pass invalidate and flush requests from the software side to local FPGA
    // caches.
    //
    rule getInvalidateReq (True);
        let r <- remoteFuncpMem.inval.getReq();
        match {.ctx_id, .addr, .only_flush} = r;

        let ref_info = FUNCP_CACHE_REF_INFO { contextId: ctx_id, memRefToken: ? };
        let w_addr = wordAddrFromByteAddr(addr);
        
        if (only_flush)
        begin
            cache.flushReq(w_addr, True, ref_info);
            debugLog.record($format("cache flush: ctx=%0d, addr=0x%x, w_addr=0x%x", ctx_id, addr, w_addr));
        end
        else
        begin
            cache.invalReq(w_addr, True, ref_info);
            debugLog.record($format("cache inval: ctx=%0d, addr=0x%x, w_addr=0x%x", ctx_id, addr, w_addr));
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
    (FUNCP_MEM_HOST_IFC);
    
    // Stubs for host functional memory communication.
    ServerStub_FUNCP_MEMORY serverStub <- mkServerStub_FUNCP_MEMORY();
    ClientStub_FUNCP_MEMORY clientStub <- mkClientStub_FUNCP_MEMORY();

    //
    // Interface between host functional memory and the central cache.
    //
    interface FUNCP_CENTRAL_CACHE_BACKING cacheBacking;
        //
        // readLineReq --
        //     Request a full line of data.
        //
        method Action readLineReq(FUNCP_MEM_WORD_PADDR wAddr, FUNCP_CACHE_REF_INFO refInfo);
            let addr = byteAddrFromWordAddr(wAddr);
            debugLog.record($format("back readReq: ctx=%0d, addr=0x%x", refInfo.contextId, addr));
            clientStub.makeRequest_LoadLine(contextIdToRRR(refInfo.contextId),
                                            zeroExtend(addr));
        endmethod

        //
        // readResp --
        //     Pipelined load response.  Words come in, and are expected
        //     by the cache, one at a time.
        //
        method ActionValue#(MEM_VALUE) readResp();
            let val <- serverStub.acceptRequest_LoadData();
            debugLog.record($format("back readResp: val=0x%x", val));
            return val;
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
                                   FUNCP_CACHE_REF_INFO refInfo,
                                   Bool sendAck);
            let addr = byteAddrFromWordAddr(wAddr);
            debugLog.record($format("back writeCtrl: ctx=%0d, addr=0x%x, valid=0x%x, ack=%d", refInfo.contextId, addr, pack(wordValidMask), pack(sendAck)));
            clientStub.makeRequest_StoreLine(contextIdToRRR(refInfo.contextId),
                                             zeroExtend(pack(wordValidMask)),
                                             zeroExtend(pack(sendAck)),
                                             zeroExtend(addr));
        endmethod

        //
        // writeData --
        //     Forward data associated with writeLineReq() above.
        //
        method Action writeData(MEM_VALUE val);
            debugLog.record($format("back writeData: val=0x%x", val));
            clientStub.makeRequest_StoreData(val);
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
        method ActionValue#(Tuple3#(CONTEXT_ID, MEM_ADDRESS, Bool)) getReq();
            let r <- serverStub.acceptRequest_Invalidate();
            return tuple3(contextIdFromRRR(r.ctxId),
                          truncate(r.addr),
                          unpack(truncate(r.onlyFlush)));
        endmethod

        // ACK that flush is complete
        method Action sendResp();
            serverStub.sendResponse_Invalidate(?);
        endmethod
    endinterface
endmodule
