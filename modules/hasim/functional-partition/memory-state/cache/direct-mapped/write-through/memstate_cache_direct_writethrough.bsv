// memstate_cache_direct_writethrough

// A simple direct-mapped cache which writes stores through to memory.
// Chosen for ease of implementation rather than superiority.


// * Direct-Mapped (Needs multiple ways)
// * Write-Through (Needs dirty bits)
// * Allocate on Write (Needs a policy)
// * Blocking (Needs a completion buffer)
// * Cache line == 1 value (Needs a line size, and the ability to issue multiple loads in a fill)

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

typedef Bit#(TLog#(TDiv#(`FUNCP_ISA_INT_REG_SIZE,8)))                                                  WORD_OFFSET;
typedef Bit#(TLog#(TDiv#(`FUNCP_CACHELINE_BITS,`FUNCP_ISA_INT_REG_SIZE)))                              CACHELINE_OFFSET;
typedef Bit#(`CACHE_IDX_BITS)                                                                          CACHE_IDX;
typedef Bit#(TSub#(`FUNCP_ISA_ADDR_SIZE,TAdd#(`CACHE_IDX_BITS,TLog#(TDiv#(`FUNCP_CACHELINE_BITS,8))))) CACHE_TAG;

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

module [HASIM_MODULE] mkFUNCP_Cache ();

  // ***** Soft Connections ***** //

  Connection_Server#(MEM_REQUEST, MEM_VALUE)   link_memstate               <- mkConnection_Server("mem_cache");
  Connection_Client#(MEM_REQUEST, MEM_REPLY)   link_funcp_memory           <- mkConnection_Client("funcp_memory");
  Connection_Receive#(MEM_ADDRESS)             link_funcp_memory_inval     <- mkConnection_Receive("funcp_memory_invalidate");
  Connection_Receive#(Bit#(0))                 link_funcp_memory_inval_all <- mkConnection_Receive("funcp_memory_invalidate_all");

  BRAM#(CACHE_IDX, Maybe#(CACHE_TAG))                   cache_tags <- mkBRAM_Full();
  Vector#(CACHELINE_WORDS, BRAM#(CACHE_IDX, MEM_VALUE)) cache_data <- replicateM(mkBRAM_Full());

  FIFO#(MEM_REQUEST) pendingQ <- mkFIFO1; // size=1 -> blocking. we'll need a searchable fifo for size >=2.
  Reg#(Bool)         waiting  <- mkReg(False);

  Reg#(Bool)      invalidating_all <- mkReg(True); // at reset, invalidate the blockrams.
  Reg#(CACHE_IDX) invalidate_iter  <- mkReg(0);

  Param#(1) enableCacheParam <- mkDynamicParameter(`PARAMS_FUNCP_MEMSTATE_CACHE_ENABLE_FUNCP_MEM_CACHE);
  function Bool enableCache() = (enableCacheParam == 1);

  // ***** Rules ***** //

  rule handleReq (!invalidating_all);
      let req = link_memstate.getReq();
      link_memstate.deq();
      pendingQ.enq(req);
      let addr = case (req) matches
                    tagged MEM_LOAD  .a: a;
                    tagged MEM_STORE .s: s.addr;
                 endcase;
      cache_tags.read_req(cacheIdx(addr));
      cache_data[cacheLineOffset(addr)].read_req(cacheIdx(addr));
  endrule

  rule handleLoad (!waiting &&& pendingQ.first matches tagged MEM_LOAD .addr);
      let mtag <- cache_tags.read_resp();
      let val  <- cache_data[cacheLineOffset(addr)].read_resp();

      if (enableCache &&& mtag matches tagged Valid .tag &&& tag == cacheTag(addr)) begin
          link_memstate.makeResp(val);
          pendingQ.deq();
      end
      else begin
          link_funcp_memory.makeReq(tagged MEM_LOAD_CACHELINE getLineAddr(addr));
          waiting <= True;
      end
  endrule

  rule handleStore (!waiting &&& pendingQ.first matches tagged MEM_STORE .st_info);
      let addr = st_info.addr;
      let mtag <- cache_tags.read_resp();
      let val  <- cache_data[cacheLineOffset(addr)].read_resp();

      if (enableCache &&& mtag matches tagged Valid .tag &&& tag == cacheTag(addr)) begin
          cache_data[cacheLineOffset(addr)].write(cacheIdx(addr), st_info.val);
          link_funcp_memory.makeReq(tagged MEM_STORE st_info);
          pendingQ.deq();
      end
      else begin
          link_funcp_memory.makeReq(tagged MEM_LOAD_CACHELINE getLineAddr(addr));
          waiting <= True;
      end
  endrule

  rule handleFill (waiting &&& link_funcp_memory.getResp() matches tagged MEM_REPLY_LOAD_CACHELINE .v);
    link_funcp_memory.deq();
    waiting <= False;
    pendingQ.deq();
    case (pendingQ.first) matches
        tagged MEM_LOAD .addr:
            begin
                link_memstate.makeResp(v[cacheLineOffset(addr)]);
                for (Integer i = 0; i < valueof(CACHELINE_WORDS); i = i + 1)
                    cache_data[i].write(cacheIdx(addr), v[i]);
                cache_tags.write(cacheIdx(addr), tagged Valid cacheTag(addr));
            end
        tagged MEM_STORE .st_info:
            begin
                let addr = st_info.addr;
                for (Integer i = 0; i < valueof(CACHELINE_WORDS); i = i + 1) begin
                    if (fromInteger(i) == cacheLineOffset(addr))
                        cache_data[i].write(cacheIdx(addr), st_info.val);
                    else
                        cache_data[i].write(cacheIdx(addr), v[i]);
                end
                cache_tags.write(cacheIdx(addr), tagged Valid cacheTag(addr));
                link_funcp_memory.makeReq(tagged MEM_STORE st_info);
            end
    endcase
  endrule

  // invalidate
  
  // When:   When main memory sends an invalidate.
  // Effect: Invalidate the line unconditionally.
  //         A smarter cache would check that the line was actually present.
    
  rule invalidate (True);
  
    MEM_ADDRESS a = link_funcp_memory_inval.receive();
    link_funcp_memory_inval.deq();

    cache_tags.write(cacheIdx(a), tagged Invalid);
  endrule

  // invalidate all; starts an FSM to invalidate every entry.

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
