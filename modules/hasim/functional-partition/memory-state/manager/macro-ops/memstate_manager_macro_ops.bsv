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

//
// Request from functional partition to memstate manager and store buffer.
//

typedef struct
{
    TOKEN tok;
    MEM_ADDRESS addr;
    MEM_VALUE value;
}
MEMSTATE_REQ_STORE
    deriving (Eq, Bits);


typedef struct
{
    TOKEN tok;
    MEM_ADDRESS addr;
    Bool iStream;        // True iff load is fetching an instruction
}
MEMSTATE_REQ_LOAD
    deriving (Eq, Bits);


typedef struct
{
    TOKEN tok;
}
MEMSTATE_REQ_COMMIT
    deriving (Eq, Bits);


typedef struct
{
    TOKEN_INDEX rewind_to;
    TOKEN_INDEX rewind_from;
}
MEMSTATE_REQ_REWIND
    deriving (Eq, Bits);

typedef union tagged
{
    MEMSTATE_REQ_LOAD    REQ_LOAD;
    MEMSTATE_REQ_STORE   REQ_STORE;
    MEMSTATE_REQ_COMMIT  REQ_COMMIT;
    MEMSTATE_REQ_REWIND  REQ_REWIND;
}
MEMSTATE_REQ
    deriving (Eq, Bits);


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

    DEBUG_FILE debugLog <- mkDebugFile(`FUNCP_MEMSTATE_LOGFILE_NAME);

    // ***** Submodules ***** //

    // Instantiate the Store Buffer
    MEMSTATE_SBUFFER stBuffer <- mkFUNCP_StoreBuffer(debugLog);

    // Instantiate the Cache
    let cache     <- mkFUNCP_Cache();

    // ***** Soft Connections ***** //

    // Links to the functional partition register state.
    Connection_Server#(MEMSTATE_REQ, MEM_VALUE) linkRegState <- mkConnection_Server("funcp_memstate");

    // Link to the Cache
    Connection_Client#(MEM_REQUEST, MEM_VALUE)  linkCache    <- mkConnection_Client("mem_cache");

    // ***** Rules ***** //

    // memStore
    // 1-stage macro-operation.
    
    // When:   Any time we get a Store request from the register state.
    // Effect: We record the store in the store buffer but don't actually change the cache.
    //         This change will take effect when the store is committed.
    // Parameters: MEMSTATE_REQ (MEMSTATE_REQ_STORE)
    // Returns:    None.
    

    rule memStore (linkRegState.getReq() matches tagged REQ_STORE .stInfo);

        // Pop the request from the register state.
        linkRegState.deq();
        
        debugLog.record($format("STORE: tok=%d, addr=0x%x, value=0x%x", stInfo.tok.index, stInfo.addr, stInfo.value));

        // Place the value in store buffer, but don't actually change the cache.
        stBuffer.insertReq(stInfo.tok.index, stInfo.addr, stInfo.value);

    endrule

    // memLoad
    
    // 2- or 3-stage macro-operation. (Depending on if store buffer hits.)
 
    // When:   Any time we get a Load request from the register state.
    // Effect: We check the store buffer and the cache and return the response.
    // Parameters: MEMSTATE_REQ (MEMSTATE_REQ_LOAD)
    // Returns:    MEMSTATE_RSP (MEMSTATE_RSP_LOAD)

    // memLoad1

    // When:   Any time we get a Load request from the register state.
    // Effect: Convert the address and send it to the store buffer.

    rule memLoad1 (linkRegState.getReq() matches tagged REQ_LOAD .ldInfo);

        // Pop the request from the register state.
        linkRegState.deq();

        debugLog.record($format("LOAD: tok=%d, addr=0x%x", ldInfo.tok.index, ldInfo.addr));

        // Send it on to the store buffer and the cache in parallel.
        // Since most loads miss in the store buffer there isn't much point
        // in waiting for the response.
        stBuffer.lookupReq(ldInfo.tok.index, ldInfo.addr);
        linkCache.makeReq(funcpMemLoadReq(ldInfo.addr, ldInfo.iStream));

    endrule

    // memLoad2

    // When:   Some time after memLoad1.
    // Effect: If the store buffer was a hit then buffer it for response.
    //         Otherwise pass it to the cache.

    rule memLoad2 (True);

        // Get the responses from the store buffer and the cache.
        let sb_rsp <- stBuffer.lookupResp();

        let cache_val = linkCache.getResp();
        linkCache.deq();

        case (sb_rsp.mvalue) matches
          tagged Invalid:
          begin
              // A miss in the store buffer.  Return memory value.
              debugLog.record($format("  LOAD from mem: value=0x%x", cache_val));
              linkRegState.makeResp(cache_val);
          end
          tagged Valid .sb_val:
          begin
              // A hit in the store buffer.              
              // Send it on to the next stage so things don't get out-of-order.
              debugLog.record($format("  LOAD SB Hit: value=0x%x", sb_val));
              linkRegState.makeResp(sb_val);
          end
        endcase

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

    rule commit1 (linkRegState.getReq() matches tagged REQ_COMMIT .req);

        // Get the input from the register state. Begin macro-operation.
        linkRegState.deq();

        debugLog.record($format("COMMIT: tok=%d", req.tok.index));

        // Send the request on to the store buffer.
        stBuffer.commitReq(req.tok.index);

    endrule
    
    // commit2
    
    // When:   Some time after commit1
    // Effect: Send the store on to update the cache/memory.

    rule commit2 (True);

        // Get the response from the store buffer.
        let rsp <- stBuffer.commitResp();

        debugLog.record($format("  COMMIT resp: addr=0x%x, value=0x%x, more=%d", rsp.addr, rsp.value, rsp.hasMore));

        // Send the actual store to the cache.
        linkCache.makeReq(funcpMemStoreReq(rsp.addr, rsp.value));

    endrule

    // rewind
    
    // 1-stage macro-operation
    
    // When:   When the register state requests a rollback.
    // Effect: Pass the request on to the store buffer.
    // Parameters: TOKEN, TOKEN
    // Returns:    N/A

    rule rewind (linkRegState.getReq() matches tagged REQ_REWIND .rew);
      
        linkRegState.deq();

        debugLog.record($format("REWIND: rewind_to=%d, rewind_from=%d", rew.rewind_to, rew.rewind_from));

        // Pass the request on to the store buffer.
        stBuffer.rewindReq(rew.rewind_to, rew.rewind_from);

    endrule

endmodule
