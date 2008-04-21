// memstate_manager_macro_ops

// Tracks memory state with a cache and a store buffer,
// using the macro-operation design pattern.

// Library imports

import RegFile::*;
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
    struct {TOKEN token; MEM_ADDRESS addr;               } MEMSTATE_REQ_LOAD;
    struct {TOKEN token; MEM_ADDRESS addr; MEM_VALUE val;} MEMSTATE_REQ_STORE;
}
    MEMSTATE_REQ 
          deriving
                   (Eq, Bits);

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
    Connection_Receive#(TOKEN)                               linkCommit        <- mkConnection_Receive("funcp_mem_commit");
    Connection_Receive#(Tuple2#(TOKEN_INDEX, TOKEN_INDEX))   linkRewindToToken <- mkConnection_Receive("funcp_mem_rewind");

    // Link to the Store Buffer
    Connection_Client#(MEMSTATE_SBUFFER_REQ, MEMSTATE_SBUFFER_RSP) linkStoreBuffer <- mkConnection_Client("mem_storebuf");
    // Link to the Cache
    Connection_Client#(MEM_REQUEST, MEM_VALUE)  linkCache     <- mkConnection_Client("mem_cache");

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
        linkStoreBuffer.makeReq(tagged SBUFFER_REQ_INSERT {tok: stInfo.token, addr: stInfo.addr, value: stInfo.val});

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
    // Effect: If the store buffer was a hit then return it to the register state.
    //         Otherwise pass it to the cache.

    rule memLoad2 (linkStoreBuffer.getResp() matches tagged SBUFFER_RSP_LOOKUP {tok: .tok, addr: .addr, mresult: .mnew_val});

        // Pop the response from the store buffer.  
        linkStoreBuffer.deq();

        case (mnew_val) matches
          tagged Invalid: // A miss in the store buffer.
          begin

              // Send it on to the cache.
              linkCache.makeReq(tagged MEM_LOAD addr);
              
          end
          tagged Valid .val: // A hit in the store buffer.
          begin
              
              // Send the response back to the register state. End of macro-operation (path 1).
              linkRegState.makeResp(val);

          end
        endcase

    endrule
     
    // memLoad3
    
    // When:   Some time after memLoad2
    // Effect: Get the result from the cache and return it to the register state.
    
    rule memLoad3 (True);

        // Get the response from the cache.
        let val = linkCache.getResp();
        linkCache.deq();

        // Send the response to the register state. End of macro-operation (path 2).
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

    rule commit1 (True);

        // Get the input from the register state. Begin macro-operation.
        TOKEN tok = linkCommit.receive();
        linkCommit.deq();

        // Send the request on to the store buffer.
        linkStoreBuffer.makeReq(tagged SBUFFER_REQ_COMMIT tok);

    endrule
    
    // commit2
    
    // When:   Some time after commit1
    // Effect: Send the store on to update the cache/memory.

    rule commit2 (linkStoreBuffer.getResp() matches tagged SBUFFER_RSP_COMMIT {addr: .a, unused: .*, value: .v, tok: .t});

      // Pop the response from the store buffer.
      linkStoreBuffer.deq();

      // Send the actual store to the cache.
      linkCache.makeReq(tagged MEM_STORE {addr:a, val: v});

    endrule

    // rewind
    
    // 1-stage macro-operation
    
    // When:   When the register state requests a rollback.
    // Effect: Pass the request on to the store buffer.
    // Parameters: TOKEN, TOKEN
    // Returns:    N/A

    rule rewind (True);
      
      // Get the input from the register state.
      match {.rewind_tok, .youngest} = linkRewindToToken.receive();
      linkRewindToToken.deq();

      // Pass the request on to the store buffer.
      linkStoreBuffer.makeReq(tagged SBUFFER_REQ_REWIND {rewind: rewind_tok, youngest: youngest});

    endrule

endmodule
