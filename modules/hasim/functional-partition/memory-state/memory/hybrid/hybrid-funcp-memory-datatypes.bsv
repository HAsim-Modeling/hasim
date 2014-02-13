//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_regstate_base_types.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"
`include "asim/provides/funcp_memory.bsh"

import Vector::*;


// ========================================================================
//
// Memory data type definitions.
//
// ========================================================================

//
// Sizes
//
typedef TDiv#(`FUNCP_CACHELINE_BITS, `FUNCP_ISA_INT_REG_SIZE) FUNCP_MEM_CACHELINE_WORDS;
typedef Vector#(FUNCP_MEM_CACHELINE_WORDS, MEM_VALUE) FUNCP_MEM_CACHELINE;
typedef Vector#(FUNCP_MEM_CACHELINE_WORDS, Bool) FUNCP_MEM_CACHELINE_WORD_VALID_MASK;


//
// MEM_REQUEST
//
// A request to the memory virtual device is either a load or a store.
//

typedef struct
{
    CONTEXT_ID contextId;
    MEM_ADDRESS addr;
    Bool iStream;        // True iff load is fetching an instruction
    FUNCP_MEMREF_TOKEN memRefToken;
}
MEM_LOAD_INFO
    deriving (Eq, Bits);

typedef struct
{
    CONTEXT_ID contextId;
    MEM_ADDRESS addr;
    MEM_VALUE val;
}
MEM_STORE_INFO
    deriving (Eq, Bits);


typedef union tagged 
{
    MEM_LOAD_INFO  MEM_LOAD;
    MEM_STORE_INFO MEM_STORE;
}
MEM_REQUEST
    deriving (Eq, Bits);


function MEM_REQUEST funcpMemLoadReq(CONTEXT_ID ctxId, MEM_ADDRESS addr, Bool iStream, FUNCP_MEMREF_TOKEN memRefToken);
    return tagged MEM_LOAD MEM_LOAD_INFO { contextId: ctxId, addr: addr, iStream: iStream, memRefToken: memRefToken };
endfunction

function MEM_REQUEST funcpMemStoreReq(CONTEXT_ID ctxId, MEM_ADDRESS addr, MEM_VALUE value);
    return tagged MEM_STORE MEM_STORE_INFO { contextId: ctxId, addr: addr, val: value };
endfunction
