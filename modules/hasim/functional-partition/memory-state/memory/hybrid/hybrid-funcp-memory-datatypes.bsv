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
