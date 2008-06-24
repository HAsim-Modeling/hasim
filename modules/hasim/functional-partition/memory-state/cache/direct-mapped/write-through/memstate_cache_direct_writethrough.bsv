//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
//

// memstate_cache_direct_writethrough

// A simple direct-mapped cache which writes stores through to memory.

// * Direct-Mapped (Needs multiple ways)
// * Write-Through (Needs dirty bits)
// * Allocate on Write (Needs a policy)
// * Blocking (Needs a completion buffer)

// Library imports.

import FIFO::*;
import Vector::*;

// Project foundation imports.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"

// The memory virtual device

`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"

`include "asim/dict/PARAMS_FUNCP_MEMSTATE_CACHE.bsh"

// mkFUNCP_Cache

// A direct-mapped write-through cache.

typedef Bit#(TLog#(TDiv#(`FUNCP_ISA_INT_REG_SIZE,8)))                           WORD_OFFSET;
typedef Bit#(TLog#(TDiv#(`FUNCP_CACHELINE_BITS,`FUNCP_ISA_INT_REG_SIZE)))       CACHELINE_OFFSET;
typedef Bit#(`FUNCP_CACHE_IDX_BITS)                                             CACHE_IDX;
typedef Bit#(TSub#(`FUNCP_ISA_ADDR_SIZE,TAdd#(`FUNCP_CACHE_IDX_BITS,TLog#(TDiv#(`FUNCP_CACHELINE_BITS,8))))) CACHE_TAG;

//
// Request structure passed along the cache.
//
typedef struct
{
    MEM_REQUEST req;
    CACHE_IDX   idx;
}
  CACHE_ACCESS
    deriving(Eq, Bits);

//
// cacheIdx --
//
function CACHE_IDX cacheIdx(MEM_ADDRESS addr);

    Tuple4#(CACHE_TAG,CACHE_IDX,CACHELINE_OFFSET,WORD_OFFSET) tup = unpack(addr);
    match { .tag, .idx, .cloff, .woff } = tup;
    return idx;

endfunction

function CACHE_TAG cacheTag(MEM_ADDRESS addr);

    Tuple4#(CACHE_TAG,CACHE_IDX,CACHELINE_OFFSET,WORD_OFFSET) tup = unpack(addr);
    match { .tag, .idx, .cloff, .woff } = tup;
    return tag;

endfunction

function CACHELINE_OFFSET cacheLineOffset(MEM_ADDRESS addr);

    Tuple4#(CACHE_TAG,CACHE_IDX,CACHELINE_OFFSET,WORD_OFFSET) tup = unpack(addr);
    match { .tag, .idx, .cloff, .woff } = tup;
    return cloff;

endfunction

function WORD_OFFSET cacheWordOffset(MEM_ADDRESS addr);

    Tuple4#(CACHE_TAG,CACHE_IDX,CACHELINE_OFFSET,WORD_OFFSET) tup = unpack(addr);
    match { .tag, .idx, .cloff, .woff } = tup;
    return woff;

endfunction

function MEM_ADDRESS getLineAddr (MEM_ADDRESS addr);

    CACHELINE_OFFSET cloff_0 = 0;
    WORD_OFFSET      woff_0  = 0;
    return pack(tuple4(cacheTag(addr),cacheIdx(addr),cloff_0,woff_0));

endfunction

module [HASIM_MODULE] mkFUNCP_Cache ()
    provisos(Bits#(CACHE_IDX, cache_SZ));

    // ***** Soft Connections ***** //

    Connection_Server#(MEM_REQUEST, MEM_VALUE)   link_memstate               <- mkConnection_Server("mem_cache");
    Connection_Client#(MEM_REQUEST, MEM_REPLY)   link_funcp_memory           <- mkConnection_Client("funcp_memory");
    Connection_Receive#(MEM_ADDRESS)             link_funcp_memory_inval     <- mkConnection_Receive("funcp_memory_invalidate");
    Connection_Receive#(Bit#(0))                 link_funcp_memory_inval_all <- mkConnection_Receive("funcp_memory_invalidate_all");

    BRAM#(cache_SZ, Maybe#(CACHE_TAG))                   cache_tags <- mkBramInitialized(tagged Invalid);
    Vector#(CACHELINE_WORDS, BRAM#(cache_SZ, MEM_VALUE)) cache_data <- replicateM(mkBramInitialized(?));

    FIFO#(CACHE_ACCESS) pendingQ <- mkFIFO1; // size=1 -> blocking. we'll need a searchable fifo for size >=2.
    Reg#(Bool)          waiting  <- mkReg(False);

    Reg#(Bool)      invalidating_all <- mkReg(True); // at reset, invalidate the blockrams.
    Reg#(CACHE_IDX) invalidate_iter  <- mkReg(0);

    Param#(1) enableCacheParam <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_CACHE_ENABLE_FUNCP_MEM_CACHE);
    function Bool enableCache() = (enableCacheParam == 1);

    // ***** Rules ***** //

    //
    // handleReq --
    //     Main entry point for load/store requests.
    //
    rule handleReq (!invalidating_all);
        let req = link_memstate.getReq();
        link_memstate.deq();
        let addr = case (req) matches
                      tagged MEM_LOAD  .a: a;
                      tagged MEM_STORE .s: s.addr;
                      tagged MEM_INVALIDATE_CACHELINE .a: a;
                   endcase;

        let idx = cacheIdx(addr);

        CACHE_ACCESS access;
        access.req = req;
        access.idx = idx;
        pendingQ.enq(access);

        cache_tags.readReq(idx);
        cache_data[cacheLineOffset(addr)].readReq(idx);
    endrule


    //
    // handleInval --
    //     Invalidate a single line if it matches the tag.
    //
    rule handleInval (pendingQ.first.req matches tagged MEM_INVALIDATE_CACHELINE .addr);
        let mtag <- cache_tags.readResp();
        let val  <- cache_data[cacheLineOffset(addr)].readResp();
        let idx = pendingQ.first.idx;

        if (mtag matches tagged Valid .tag &&& tag == cacheTag(addr))
        begin
            cache_tags.write(idx, tagged Invalid);
        end
    endrule


    //
    // handleLoad --
    //     Load request.  Either return valid data from the cache or request
    //     the line from memory.
    //
    rule handleLoad (!waiting &&& pendingQ.first.req matches tagged MEM_LOAD .addr);
        let mtag <- cache_tags.readResp();
        let val  <- cache_data[cacheLineOffset(addr)].readResp();

        if (enableCache &&& mtag matches tagged Valid .tag &&& tag == cacheTag(addr)) begin
            link_memstate.makeResp(val);
            pendingQ.deq();
        end
        else begin
            link_funcp_memory.makeReq(tagged MEM_LOAD_CACHELINE getLineAddr(addr));
            waiting <= True;
        end
    endrule


    //
    // handleStore --
    //
    rule handleStore (!waiting &&& pendingQ.first.req matches tagged MEM_STORE .st_info);
        let addr = st_info.addr;
        let idx = pendingQ.first.idx;
        let mtag <- cache_tags.readResp();
        let val  <- cache_data[cacheLineOffset(addr)].readResp();

        if (enableCache &&& mtag matches tagged Valid .tag &&& tag == cacheTag(addr)) begin
            cache_data[cacheLineOffset(addr)].write(idx, st_info.val);
            link_funcp_memory.makeReq(tagged MEM_STORE st_info);
            pendingQ.deq();
        end
        else begin
            link_funcp_memory.makeReq(tagged MEM_LOAD_CACHELINE getLineAddr(addr));
            waiting <= True;
        end
    endrule


    //
    // handleFill --
    //    Update the cache with requested data coming back from memory.
    //
    rule handleFill (waiting &&& link_funcp_memory.getResp() matches tagged MEM_REPLY_LOAD_CACHELINE .v);
        link_funcp_memory.deq();
        waiting <= False;
        pendingQ.deq();

        let idx = pendingQ.first.idx;

        case (pendingQ.first.req) matches
            tagged MEM_LOAD .addr:
            begin
                link_memstate.makeResp(v[cacheLineOffset(addr)]);
                for (Integer i = 0; i < valueof(CACHELINE_WORDS); i = i + 1)
                    cache_data[i].write(idx, v[i]);
                cache_tags.write(idx, tagged Valid cacheTag(addr));
            end

            tagged MEM_STORE .st_info:
            begin
                let addr = st_info.addr;
                for (Integer i = 0; i < valueof(CACHELINE_WORDS); i = i + 1) begin
                    if (fromInteger(i) == cacheLineOffset(addr))
                        cache_data[i].write(idx, st_info.val);
                    else
                        cache_data[i].write(idx, v[i]);
                end
                cache_tags.write(idx, tagged Valid cacheTag(addr));
                link_funcp_memory.makeReq(tagged MEM_STORE st_info);
            end
        endcase
    endrule


    //
    // invalidate --
    //     Handle requests from hybrid memory (software-side) to invalidate
    //     a specific address.
    //    
    rule invalidate (!invalidating_all);
        MEM_ADDRESS addr = link_funcp_memory_inval.receive();
        link_funcp_memory_inval.deq();

        let idx = cacheIdx(addr);

        CACHE_ACCESS access;
        access.req = tagged MEM_INVALIDATE_CACHELINE addr;
        access.idx = idx;

        cache_tags.readReq(idx);
        cache_data[cacheLineOffset(addr)].readReq(idx);
    endrule


    //
    // invalidate all --
    //     Invalidate entire cache using a FSM.
    //
    rule invalidate_all_start (!waiting);
        link_funcp_memory_inval_all.deq();
        invalidating_all <= True;
    endrule

    rule invalidate_all (invalidating_all);
        invalidate_iter <= invalidate_iter + 1;
        cache_tags.write(invalidate_iter, tagged Invalid);
        if (invalidate_iter == maxBound)
            invalidating_all <= False;
    endrule

endmodule
