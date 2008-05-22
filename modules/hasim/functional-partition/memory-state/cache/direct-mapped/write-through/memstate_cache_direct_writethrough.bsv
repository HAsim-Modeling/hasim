// memstate_cache_direct_writethrough

// A simple direct-mapped cache which writes stores through to memory.
// Chosen for ease of implementation rather than superiority.


// * Direct-Mapped (Needs multiple ways)
// * Write-Through (Needs dirty bits)
// * Allocate on Write (Needs a policy)
// * Blocking (Needs a completion buffer)
// * Cache line == 1 value (Needs a line size, and the ability to issue multiple loads in a fill)

// Library imports.

import FIFOF::*;

// Project foundation imports.

`include "hasim_common.bsh"
`include "soft_connections.bsh"
`include "fpga_components.bsh"

// The memory virtual device

`include "funcp_base_types.bsh"
`include "funcp_memory.bsh"

// mkFUNCP_Cache

// A direct-mapped write-through cache.

// one cache-line = one word.

typedef Bit#(`CACHE_IDX_BITS) CACHE_IDX;
typedef Bit#(TLog#(TDiv#(`FUNCP_ISA_INT_REG_SIZE,8))) WORD_OFFSET;
typedef Bit#(TSub#(`FUNCP_ISA_ADDR_SIZE,TAdd#(`CACHE_IDX_BITS,TLog#(TDiv#(`FUNCP_ISA_INT_REG_SIZE,8))))) CACHE_TAG;

function CACHE_IDX cacheIdx(MEM_ADDRESS addr);

  Tuple3#(CACHE_TAG,CACHE_IDX,WORD_OFFSET) tup = unpack(addr);
  match { .tag, .idx, .woff } = tup;
  // assert woff == 0
  return idx;

endfunction

function CACHE_TAG cacheTag(MEM_ADDRESS addr);

  Tuple3#(CACHE_TAG,CACHE_IDX,WORD_OFFSET) tup = unpack(addr);
  match { .tag, .idx, .woff } = tup;
  // assert woff == 0
  return tag;

endfunction

module [HASIM_MODULE] mkFUNCP_Cache ();

  // ***** Soft Connections ***** //

  Connection_Server#(MEM_REQUEST, MEM_VALUE)   link_memstate          <- mkConnection_Server("mem_cache");
  Connection_Client#(MEM_REQUEST, MEM_VALUE)   link_funcp_memory       <- mkConnection_Client("funcp_memory");
  Connection_Receive#(MEM_ADDRESS)             link_funcp_memory_inval <- mkConnection_Receive("funcp_memory_invalidate");
  Connection_Receive#(Bit#(0))                 link_funcp_memory_inval_all <- mkConnection_Receive("funcp_memory_invalidate_all");

  BRAM#(CACHE_IDX, Maybe#(CACHE_TAG)) cache_tags <- mkBRAM_Full();
  BRAM#(CACHE_IDX, MEM_VALUE) cache_data <- mkBRAM_Full();
  FIFOF#(MEM_ADDRESS) readQ <- mkFIFOF();
  FIFOF#(MEM_ADDRESS) fillQ <- mkFIFOF();

  Reg#(Bool)      invalidating_all <- mkReg(False);
  Reg#(CACHE_IDX) invalidate_iter  <- mkReg(0);

  // ***** Rules ***** //

  // handleLoad1

  // When:   When the mem state requests a load.
  // Effect: Pass the request on to the rams so we can see if it's a hit.

  rule handleLoad1 (!invalidating_all &&& link_memstate.getReq() matches tagged MEM_LOAD .addr);
  
    // Deq the request.
    link_memstate.deq();
    
    // Read the rams.
    cache_tags.read_req(cacheIdx(addr));
    cache_data.read_req(cacheIdx(addr));
    
    // Pass the address on to the next stage.
    readQ.enq(addr);
    
  endrule
  
  // handleLoad2

  // When:   After handleLoad1 and we're not blocking.
  // Effect: If it's a hit, return the data, otherwise send the request to main memory.

  rule handleLoad2 (!fillQ.notEmpty());
  
    // Get the address from the previous stage.
    let addr = readQ.first();
    readQ.deq();
    
    // Get the responses from the rams.
    let mtag <- cache_tags.read_resp();
    let val <- cache_data.read_resp();
    
    // Is it a hit?
    if (mtag matches tagged Valid .tag &&& tag == cacheTag(addr))
    begin
        // It's a hit, return the value.
        link_memstate.makeResp(val);
    end
    else
    begin
        // It's a miss, send to main memory.
        link_funcp_memory.makeReq(tagged MEM_LOAD addr);
        // Send the address to the final stage.
        fillQ.enq(addr);
    end
  endrule

  // handleLoad3
  
  // When:   When a load response come back from the main memory.
  // Effect: Fill the cache line and pass the response back to the mem state.
  
  rule handleLoad3 (True);
  
    // Get the response from the memory.
    MEM_VALUE v = link_funcp_memory.getResp();
    link_funcp_memory.deq();
    
    // Get the address from the previous stage.
    let addr = fillQ.first();
    fillQ.deq();
    
    // Write into the RAMs. 
    cache_tags.write(cacheIdx(addr), tagged Valid cacheTag(addr));
    cache_data.write(cacheIdx(addr), v);
    
    // Send the response
    link_memstate.makeResp(v);
        
  endrule
  
  // handleStore
  
  // When:   When a store request comes from the mem state.
  // Effect: Write the data into the cache and write it through to main memory.
  
  rule handleStore (!invalidating_all &&& link_memstate.getReq() matches tagged MEM_STORE .st_info);
  
      // Deq the request.
      link_memstate.deq();
     
      // Update the RAMs.
      cache_tags.write(cacheIdx(st_info.addr), tagged Valid cacheTag(st_info.addr));
      cache_data.write(cacheIdx(st_info.addr), st_info.val);
        
      // Send it on to main memory.
      link_funcp_memory.makeReq(tagged MEM_STORE st_info);

  endrule
  
  // invalidate
  
  // When:   When main memory sends an invalidate.
  // Effect: Invalidate the line unconditionally.
  //         A smarter cache would check that the line was actually present.
    
  rule invalidate (True);
  
    MEM_VALUE a = link_funcp_memory_inval.receive();
    link_funcp_memory_inval.deq();

    cache_tags.write(cacheIdx(a), tagged Invalid);
        
  endrule

  // invalidate all; starts an FSM to invalidate every entry.

  rule invalidate_all_start (True);
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
