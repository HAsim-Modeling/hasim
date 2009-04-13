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

// memory_virtual_device_datatypes

// A template for datatype definitions for the Memory Virtual Device.

// You probably won't need to change these.

`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_regstate_base_types.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"

import Vector::*;

// ***** Datatype definitions *****

// MEM_CACHELINE

// This is defined here because size of the cache-line must be known outside of
// the cache.

typedef TDiv#(`FUNCP_CACHELINE_BITS,`FUNCP_ISA_INT_REG_SIZE) CACHELINE_WORDS;
typedef Vector#(CACHELINE_WORDS, MEM_VALUE) MEM_CACHELINE;
typedef Vector#(CACHELINE_WORDS, Bool) MEM_CACHELINE_WORD_VALID_MASK;

// MEM_REQUEST

// A request to the memory virtual device is either a load or a store.

typedef struct
{
    CONTEXT_ID  contextId;
    MEM_ADDRESS addr;
    Bool        iStream;        // True iff load is fetching an instruction
    FUNCP_MEMREF_TOKEN memRefToken;
}
MEM_LOAD_INFO
    deriving (Eq, Bits);

typedef struct
{
    CONTEXT_ID  contextId;
    MEM_ADDRESS addr;
    MEM_VALUE   val;
}
MEM_STORE_INFO
    deriving (Eq, Bits);


typedef struct
{
    CONTEXT_ID  contextId;
    // Partial writes are legal.  Writer must set a valid bit for every
    // word in val to be written to memory.
    MEM_CACHELINE_WORD_VALID_MASK wordValidMask;
    MEM_ADDRESS addr;
    MEM_CACHELINE val;
}
MEM_STORE_CACHELINE_INFO
    deriving (Eq, Bits);


typedef struct
{
    CONTEXT_ID  contextId;
    MEM_ADDRESS addr;
}
MEM_INVAL_INFO
    deriving (Eq, Bits);


typedef union tagged 
{
    MEM_LOAD_INFO  MEM_LOAD;
    MEM_STORE_INFO MEM_STORE;

    MEM_LOAD_INFO MEM_LOAD_CACHELINE;
    MEM_STORE_CACHELINE_INFO MEM_STORE_CACHELINE;      // Store with no ACK from server
    MEM_STORE_CACHELINE_INFO MEM_STORE_CACHELINE_SYNC; // Store with ACK from server

    MEM_INVAL_INFO MEM_INVALIDATE_CACHELINE;
    MEM_INVAL_INFO MEM_FLUSH_CACHELINE;
}
MEM_REQUEST
    deriving (Eq, Bits);


function MEM_REQUEST funcpMemLoadReq(CONTEXT_ID ctxId, MEM_ADDRESS addr, Bool iStream, FUNCP_MEMREF_TOKEN memRefToken);
    return tagged MEM_LOAD MEM_LOAD_INFO { contextId: ctxId, addr: addr, iStream: iStream, memRefToken: memRefToken };
endfunction

function MEM_REQUEST funcpMemLoadCacheLineReq(CONTEXT_ID ctxId, MEM_ADDRESS addr);
    return tagged MEM_LOAD_CACHELINE MEM_LOAD_INFO { contextId: ctxId, addr: addr, iStream: False, memRefToken: ? };
endfunction

function MEM_REQUEST funcpMemStoreReq(CONTEXT_ID ctxId, MEM_ADDRESS addr, MEM_VALUE value);
    return tagged MEM_STORE MEM_STORE_INFO { contextId: ctxId, addr: addr, val: value };
endfunction


typedef union tagged 
{
    MEMSTATE_RESP MEM_REPLY_LOAD;
    MEM_CACHELINE MEM_REPLY_LOAD_CACHELINE;
    Bool          MEM_REPLY_STORE_CACHELINE_ACK;
}
MEM_REPLY
    deriving (Eq, Bits);


//
// Message coming from software to FPGA-side functional caches.
//
typedef struct
{
    CONTEXT_ID  contextId;
    Bool onlyFlush;             // Don't have to invalidate -- just flush stores
    UInt#(8) nLines;
    MEM_ADDRESS addr;
}
MEM_INVAL_FUNCP_CACHE_SERVICE_INFO
    deriving (Eq, Bits);


// ***** RRR Datatype definitions *****

typedef Bit#(64) MEM_ADDRESS_RRR;
