// memstate_manager_macro_ops

// Tracks memory state with a cache and a store buffer,
// using the macro-operation design pattern.

// Library imports

import FIFO::*;
import Vector::*;

// Project Foundation Imports

`include "hasim_common.bsh"
`include "fpga_components.bsh"
`include "soft_connections.bsh"
`include "funcp_memory.bsh"

// Memstate imports

`include "funcp_memstate_storebuffer.bsh"
`include "funcp_memstate_cache.bsh"

// MEMSTATE_REQ

// Request to memstate data memory.

typedef union tagged 
{
    struct {TOKEN token; MEM_ADDRESS addr;                            } MEMSTATE_REQ_LOAD;

    // "num" uniquely identifies the store number for a given token.  References
    // to unaligned addresses generate two stores, each getting unique store
    // numbers.
    struct {TOKEN token; Bit#(1) num; MEM_ADDRESS addr; MEM_VALUE val;} MEMSTATE_REQ_STORE;

    TOKEN                                                               MEMSTATE_REQ_COMMIT;
    struct {TOKEN_INDEX rewind_tok; TOKEN_INDEX youngest;             } MEMSTATE_REQ_REWIND;
}
    MEMSTATE_REQ 
          deriving
                   (Eq, Bits);

// LOAD_PATH

typedef union tagged
{
  MEM_VALUE PATH_SB;
  void PATH_CACHE;
}
  LOAD_PATH
    deriving (Eq, Bits);

// mkFUNCP_MemStateManager

// The module which encapsulates Loads and Stores,
// using macro-operations to refer to the store buffer and cache.

module [HASIM_MODULE] mkFUNCP_MemStateManager ();

    // ***** Submodules ***** //

    // Instantiate the Store Buffer
    let stBuffer <- mkFUNCP_StoreBuffer();
    // Instantiate the Cache
    let cache     <- mkFUNCP_Cache();

    // ***** Soft Connections ***** //

    // Links to the functional partition register state.
    Connection_Server#(MEMSTATE_REQ, MEM_VALUE)              linkRegState      <- mkConnection_Server("funcp_memstate");

    // Link to the Store Buffer
    Connection_Client#(MEMSTATE_SBUFFER_REQ, MEMSTATE_SBUFFER_RSP) linkStoreBuffer <- mkConnection_Client("mem_storebuf");
    // Link to the Cache
    Connection_Client#(MEM_REQUEST, MEM_VALUE)  linkCache     <- mkConnection_Client("mem_cache");

    FIFO#(LOAD_PATH) loadQ <- mkSizedFIFO(16);

    // ***** Rules ***** //

    // memStore
    // 1-stage macro-operation.
    
    // When:   Any time we get a Store request from the register state.
    // Effect: We record the store in the store buffer but don't actually change the cache.
    //         This change will take effect when the store is committed.
    // Parameters: MEMSTATE_REQ (MEMSTATE_REQ_STORE)
    // Returns:    None.
    

    rule memStore (linkRegState.getReq() matches tagged MEMSTATE_REQ_STORE .stInfo);

        // Pop the request from the register state.
        linkRegState.deq();
        
        // Place the value in store buffer, but don't actually change the cache.
        linkStoreBuffer.makeReq(tagged SBUFFER_REQ_INSERT {tok: stInfo.token, num: stInfo.num, addr: stInfo.addr, value: stInfo.val});

    endrule

    // memLoad
    
    // 2- or 3-stage macro-operation. (Depending on if store buffer hits.)
 
    // When:   Any time we get a Load request from the register state.
    // Effect: We check the store buffer, then the cache (if needed), and return the response.
    // Parameters: MEMSTATE_REQ (MEMSTATE_REQ_LOAD)
    // Returns:    MEMSTATE_RSP (MEMSTATE_RSP_LOAD)

    // memLoad1

    // When:   Any time we get a Load request from the register state.
    // Effect: Convert the address and send it to the store buffer.

    rule memLoad1 (linkRegState.getReq() matches tagged MEMSTATE_REQ_LOAD .ldInfo);

        // Pop the request from the register state.
        linkRegState.deq();

        // Send it on to the store buffer.
        linkStoreBuffer.makeReq(tagged SBUFFER_REQ_LOOKUP {tok: ldInfo.token, addr: ldInfo.addr});
        
    endrule

    // memLoad2

    // When:   Some time after memLoad1.
    // Effect: If the store buffer was a hit then buffer it for response.
    //         Otherwise pass it to the cache.

    rule memLoad2 (linkStoreBuffer.getResp() matches tagged SBUFFER_RSP_LOOKUP {tok: .tok, addr: .addr, mresult: .mnew_val});

        // Pop the response from the store buffer.  
        linkStoreBuffer.deq();

        case (mnew_val) matches
          tagged Invalid: // A miss in the store buffer.
          begin

              // Send it on to the cache.
              linkCache.makeReq(tagged MEM_LOAD addr);
              
              // Record that the answer is coming from the cache.
              loadQ.enq(tagged PATH_CACHE);
              
          end
          tagged Valid .val: // A hit in the store buffer.
          begin
              
              // Send it on to the next stage so things don't get out-of-order.
              loadQ.enq(tagged PATH_SB val);

          end
        endcase

    endrule
     
    // memLoad3SB
    
    // When:   Some time after memLoad2 gets a hit in the store buffer.
    // Effect: Returns the hit to the register state, but ensures that it doesn't pass any other loads.
    
    rule memLoad3SB (loadQ.first() matches tagged PATH_SB .val);

        loadQ.deq();

        // Respond to regstate. End of macro-operation (path 1).
        linkRegState.makeResp(val);

    endrule
    
    // memLoad3Cache
    
    // When:   Some time after memLoad2 makes a request to the cache.
    // Effect: Get the result from the cache and return it to the register state.
    
    rule memLoad3Cache (loadQ.first() matches tagged PATH_CACHE);

        loadQ.deq();

        // Get the response from the cache.
        let val = linkCache.getResp();
        linkCache.deq();

        // Put in completion buffer. End of macro-operation (path 2).
        linkRegState.makeResp(val);

    endrule
  
    // commit
    
    // 2-stage macro-operation
    // When:   When the register state requests a committed store.
    // Effect: Remove a value from the store buffer and send it on to the cache.
    // Parameters: TOKEN
    // Returns:    N/A
    
    // commit1
    
    // When:   When the register state requests a committed store.
    // Effect: Retrieve the value from the store buffer.

    rule commit1 (linkRegState.getReq() matches tagged MEMSTATE_REQ_COMMIT .tok);

        // Get the input from the register state. Begin macro-operation.
        linkRegState.deq();

        // Send the request on to the store buffer.
        linkStoreBuffer.makeReq(tagged SBUFFER_REQ_COMMIT tok);

    endrule
    
    // commit2
    
    // When:   Some time after commit1
    // Effect: Send the store on to update the cache/memory.

    rule commit2 (linkStoreBuffer.getResp() matches tagged SBUFFER_RSP_COMMIT {addr: .a, hasMore: .hasMore, value: .v, tok: .t});

      // Pop the response from the store buffer.
      linkStoreBuffer.deq();

      // Send the actual store to the cache.
      linkCache.makeReq(tagged MEM_STORE MEM_STORE_INFO {addr:a, val: v});

    endrule

    // rewind
    
    // 1-stage macro-operation
    
    // When:   When the register state requests a rollback.
    // Effect: Pass the request on to the store buffer.
    // Parameters: TOKEN, TOKEN
    // Returns:    N/A

    rule rewind (linkRegState.getReq() matches tagged MEMSTATE_REQ_REWIND .rew);
      
      linkRegState.deq();

      // Pass the request on to the store buffer.
      linkStoreBuffer.makeReq(tagged SBUFFER_REQ_REWIND {rewind: rew.rewind_tok, youngest: rew.youngest});

    endrule

endmodule
