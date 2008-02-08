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
`include "memory.bsh"

`include "hasim_isa.bsh"

// Memstate imports

`include "funcp_memstate_storebuffer.bsh"
`include "funcp_memstate_cache.bsh"

// MEMSTATE_REQ

// Request to memstate data memory.

typedef union tagged 
{
    struct {TOKEN token; ISA_ADDRESS addr; ISA_LOAD_TYPE load_type;                 } MEMSTATE_REQ_LOAD;
    struct {TOKEN token; ISA_ADDRESS addr; ISA_STORE_TYPE store_type; ISA_VALUE val;} MEMSTATE_REQ_STORE;
}
    MEMSTATE_REQ 
          deriving
                   (Eq, Bits);


// MEMSTATE_RSP

// Response from memstate data memory.

typedef union tagged
{
    ISA_VALUE MEMSTATE_RSP_LOAD;
    void      MEMSTATE_RSP_STORE;
}
    MEMSTATE_RSP
            deriving
                     (Eq, Bits);

// MEMSTATE_PATH

// Track if responses are from the cache go to IMem or DMem.

typedef enum
{
  PATH_IMEM, PATH_DMEM
}
  MEMSTATE_PATH deriving (Eq, Bits);

module [HASIM_MODULE] mkFUNCP_MemStateManager ();

    // ***** Local State ***** //

    // Track where responses should go.
    FIFO#(MEMSTATE_PATH) path_q <- mkFIFO();

    // ***** Submodules ***** //

    // Instantiate the Store Buffer
    let st_buffer <- mkFUNCP_StoreBuffer();
    // Instantiate the Cache
    let cache     <- mkFUNCP_Cache();

    // ***** Soft Connections ***** //

    // Links to the functional partition register state.
    Connection_Server#(ISA_ADDRESS, ISA_INSTRUCTION)   link_imem      <- mkConnection_Server("funcp_mem_imem");
    Connection_Server#(MEMSTATE_REQ, MEMSTATE_RSP)     link_dmem      <- mkConnection_Server("funcp_mem_dmem");
    Connection_Receive#(TOKEN)                               link_commit    <- mkConnection_Receive("funcp_mem_commit");
    Connection_Receive#(Tuple2#(TOKEN_INDEX, TOKEN_INDEX))   link_rewindToToken <- mkConnection_Receive("funcp_mem_rewind");

    // Link to the Store Buffer
    Connection_Client#(MEMSTATE_SBUFFER_REQ, MEMSTATE_SBUFFER_RSP) link_stbuffer <- mkConnection_Client("mem_storebuf");
    // Link to the Cache
    Connection_Client#(MEM_Request, MEM_Value)  link_cache     <- mkConnection_Client("mem_cache");

    // ***** Rules ***** //

    // imemLoad
    
    // 2-stage macro-operation.
    
    // When:   Any time we get a request from the register state.
    // Effect: Do the load and return the result
    //         Note: We assume no self-modifying code, otherwise we would send it to the store buffer.
    //         Note: We currently assume all instructions fit in one block.
    // Parameters: FUNCP_MEM_ADDRESS
    // Return:     ISA_INSTRUCTION
    
    // imemLoad1
    // When:   When the register state makes a request.
    // Effect: Transform the request from ISA to FUNCP and send it on to the memory system.

    rule imemLoad1 (True);

        // Get the request from the register state.
        ISA_ADDRESS a = link_imem.getReq();
        link_imem.deq();

        // Transform the Address from an ISA address to an actual address 
        // using the ISA-provided conversion function.
        MEM_Addr mem_addr = isaAddressToMemAddress(a);

        // Send the request to the cache.
        // We assume no self-modifying code, otherwise we would go to the store buffer here.
        link_cache.makeReq(tagged MEM_Load a);

        // Record that the request goes to the IMem.
        path_q.enq(tagged PATH_IMEM);

    endrule

    // imemLoad2
    
    // When:   Some time after a imemLoad1.
    // Effect: Cast the memory to the register state.

    rule imemLoad2 (path_q.first() matches tagged PATH_IMEM);
    
        // Pop the path_q.
        path_q.deq();

        // Get the response from the cache.
        MEM_Value v = link_cache.getResp();
        link_cache.deq();

        // Convert the value to an instruction using the ISA-provided conversion
        // function.
        ISA_INSTRUCTION i = isaInstructionFromMemValue(v);

        // Send the response to the register state.
        link_imem.makeResp(i);

    endrule

    // dmemStore
    // 1-stage macro-operation.
    
    // When:   Any time we get a DMem Store request from the register state.
    // Effect: We record the store in the store buffer but don't actually change the cache.
    //         This change will take effect when the store is committed.
    //         Note: We fast-forward the response back to the register state.
    //               This is safe because the store buffer will keep our requests in-order.
    // Parameters: MEMSTATE_REQ (MEMSTATE_REQ_STORE)
    // Returns:    MEMSTATE_RSP (MEMSTATE_RSP_STORE)
    

    rule dmemStore (link_dmem.getReq() matches tagged MEMSTATE_REQ_STORE .stInfo);

        // Pop the request from the register state.
        link_dmem.deq();
        
        // Convert the address using the ISA-provided conversion function.
        MEM_Addr mem_addr = isaAddressToMemAddress(stInfo.addr);

        // Convert the value using the ISA-provided conversion function.
        MEM_Value mem_val = isaValueToMemValue(stInfo.val);

        // Place the value in store buffer, but don't actually change the cache.
        link_stbuffer.makeReq(tagged SBUFFER_REQ_INSERT {value: mem_val, addr: mem_addr, tok: stInfo.token});

        // Fast-forward the response before we get an ack from the store buffer.
        link_dmem.makeResp(tagged MEMSTATE_RSP_STORE);

    endrule

    
    // dmemLoad
    
    // 2- or 3-stage macro-operation. (Depending on if store buffer hits.)
 
    // When:   Any time we get a DMem Load request from the register state.
    // Effect: We check the store buffer, then the cache, and return the response.
    // Parameters: MEMSTATE_REQ (MEMSTATE_REQ_LOAD)
    // Returns:    MEMSTATE_RSP (MEMSTATE_RSP_LOAD)

    // dmemLoad1

    // When:   Any time we get a DMem Load request from the register state.
    // Effect: Convert the address and send it to the store buffer.

    rule dmemLoad1 (link_dmem.getReq() matches tagged MEMSTATE_REQ_LOAD .ldInfo);

        // Pop the request from the register state.
        link_dmem.deq();

        // Convert the address using the ISA-provided conversion function.
        MEM_Addr mem_addr = isaAddressToMemAddress(ldInfo.addr);

        // Send it on to the store buffer.
        link_stbuffer.makeReq(tagged SBUFFER_REQ_LOOKUP {addr: mem_addr, tok: ldInfo.token});

    endrule

    // dmemLoad2

    // When:   Some time after dmemLoad1.
    // Effect: If the store buffer was a hit then return it to the register state.
    //         Otherwise pass it to the cache.

    rule dmemLoad2 (link_stbuffer.getResp() matches tagged SBUFFER_RSP_LOOKUP {addr: .addr, mresult: .mnewVal, tok: .tok});

        // Pop the response from the store buffer.  
        link_stbuffer.deq();

        case (mnewVal) matches
          tagged Invalid: // A miss in the store buffer.
          begin
              // Send it on to the cache.
              link_cache.makeReq(tagged MEM_Load addr);
              // Pass relevent data on to the next stage (currently none).
              path_q.enq(tagged PATH_DMEM);
          end
          tagged Valid .newVal: // A hit in the store buffer.
          begin
              
              // Send the response back to the register state. End of macro-operation (path 1).
              link_dmem.makeResp(tagged MEMSTATE_RSP_LOAD newVal);

          end
        endcase

    endrule
     
    // dmemLoad3
    
    // When:   Some time after dmemLoad2
    // Effect: Get the result from the cache and return it to the register state.
    
    rule dmemLoad3 (path_q.first() matches tagged PATH_DMEM);

        // Pop the path_q.
        path_q.deq();

        // Get the response from the cache.
        let v = link_cache.getResp();
        link_cache.deq();

        // Convert the response into value using the ISA-provided function.
        let finalVal = isaValueFromMemValue(v);

        // Send the response to the register state. End of macro-operation (path 2).
        link_dmem.makeResp(tagged MEMSTATE_RSP_LOAD finalVal);

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
        TOKEN tok = link_commit.receive();
        link_commit.deq();

        // Send the request on to the store buffer.
        link_stbuffer.makeReq(tagged SBUFFER_REQ_COMMIT tok);

    endrule
    
    // commit2
    
    // When:   Some time after commit1
    // Effect: Send the store on to update the cache/dmem.

    rule commit2 (link_stbuffer.getResp() matches tagged SBUFFER_RSP_COMMIT {addr: .a, unused: .*, value: .v, tok: .t});

      // Pop the response from the store buffer.
      link_stbuffer.deq();

      // Send the actual store to the dmem/cache.
      link_cache.makeReq(tagged MEM_Store {addr:a, val: v});

    endrule

    // rewind
    
    // 1-stage macro-operation
    
    // When:   When the register state requests a rollback.
    // Effect: Pass the request on to the store buffer.
    // Parameters: TOKEN, TOKEN
    // Returns:    N/A

    rule handleRewind (True);
      
      // Get the input from the register state.
      match {.rewind_tok, .youngest} = link_rewindToToken.receive();
      link_rewindToToken.deq();

      // Pass the request on to the store buffer.
      link_stbuffer.makeReq(tagged SBUFFER_REQ_REWIND {rewind: rewind_tok, youngest: youngest});

    endrule

endmodule
