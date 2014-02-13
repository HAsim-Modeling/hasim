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

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_regstate_base_types.bsh"

//
// Base types for memory state in the functional partition
//


//
// MEM_ADDRESS
//
// The address space the memory virtual device uses. A parameter by default.
//
typedef FUNCP_PADDR MEM_ADDRESS;
typedef FUNCP_PADDR_SIZE MEM_ADDRESS_SIZE;


//
// MEM_VALUE
//
// The type of values stored in memory. A parameter by default.
//
typedef FUNCP_INT_REG MEM_VALUE;

//
// MEM_OFFSET
//
// The part that's left over after you align an address. 
// TODO: Should be derived from MEM_VALUE size rather than hardcoded.
//
typedef Bit#(3) MEM_OFFSET;


// ===================================================================
//
// Memory types and functions for indexing by words instead of bytes.
//
// ===================================================================

typedef TLog#(TDiv#(`FUNCP_ISA_INT_REG_SIZE, 8)) FUNCP_MEM_ISA_WORD_OFFSET_BITS;
typedef Bit#(FUNCP_MEM_ISA_WORD_OFFSET_BITS)     FUNCP_MEM_ISA_WORD_OFFSET;

typedef Bit#(TSub#(`FUNCP_ISA_P_ADDR_SIZE, FUNCP_MEM_ISA_WORD_OFFSET_BITS)) MEM_WORD_ADDRESS;

function MEM_WORD_ADDRESS wordAddrFromByteAddr(MEM_ADDRESS addr);
    Tuple2#(MEM_WORD_ADDRESS, FUNCP_MEM_ISA_WORD_OFFSET) a = unpack(addr);
    return tpl_1(a);
endfunction

function MEM_ADDRESS byteAddrFromWordAddr(MEM_WORD_ADDRESS addr);
    FUNCP_MEM_ISA_WORD_OFFSET w = 0;
    return { addr, w };
endfunction


// ===================================================================
//
//   MEMSTATE_REQ -- requests for memory operations from the
//                   functional partition.
//
// ===================================================================


typedef struct
{
    TOKEN tok;
    MEM_ADDRESS addr;
    MEM_VALUE value;
    FUNCP_MEMREF_TOKEN memRefToken;
    // Request exclusive access to the location until the token is committed to
    // global memory.
    Bool reqExclusive;
}
MEMSTATE_REQ_STORE
    deriving (Eq, Bits);

function MEMSTATE_REQ_STORE memStateReqStore(TOKEN tok,
                                             MEM_ADDRESS addr,
                                             MEM_VALUE value,
                                             Bool reqExclusive);
    // The memory reference token is typically set late, so it is left undefined here.
    return MEMSTATE_REQ_STORE { tok: tok,
                                addr: addr,
                                value: value,
                                memRefToken: ?,
                                reqExclusive: reqExclusive };
endfunction


typedef struct
{
    TOKEN tok;
    MEM_ADDRESS addr;
    Bool iStream;        // True iff load is fetching an instruction
    FUNCP_MEMREF_TOKEN memRefToken;
    Bool reqLock;        // Request a lock on the address region (e.g. Alpha ldq_l)
}
MEMSTATE_REQ_LOAD
    deriving (Eq, Bits);

function MEMSTATE_REQ_LOAD memStateReqInstr(TOKEN tok, MEM_ADDRESS addr);
    // The memory reference token is typically set late, so it is left undefined here.
    return MEMSTATE_REQ_LOAD { tok: tok,
                               addr: addr,
                               iStream: True,
                               memRefToken: ?,
                               reqLock: False };
endfunction

function MEMSTATE_REQ_LOAD memStateReqLoad(TOKEN tok, MEM_ADDRESS addr, Bool reqLock);
    // The memory reference token is typically set late, so it is left undefined here.
    return MEMSTATE_REQ_LOAD { tok: tok,
                               addr: addr,
                               iStream: False,
                               memRefToken: ?,
                               reqLock: reqLock };
endfunction


//
// MEMSTATE_REQ_COMMIT --
//     Store instruction is committing but store buffer should not be flushed.
//     Token will be released.  All future references to the store will be
//     with the store token.
//
typedef struct
{
    TOKEN tok;
    STORE_TOKEN storeTok;
}
MEMSTATE_REQ_COMMIT
    deriving (Eq, Bits);

function MEMSTATE_REQ_COMMIT memStateReqCommit(TOKEN tok, STORE_TOKEN storeTok);
    return MEMSTATE_REQ_COMMIT { tok: tok,
                                 storeTok: storeTok };
endfunction


//
// MEMSTATE_REQ_WRITE_BACK --
//     Move store data from store buffer to memory.
//
typedef struct
{
    STORE_TOKEN storeTok;
}
MEMSTATE_REQ_WRITE_BACK
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
    MEMSTATE_REQ_LOAD        REQ_LOAD;
    MEMSTATE_REQ_STORE       REQ_STORE;
    MEMSTATE_REQ_COMMIT      REQ_COMMIT;
    MEMSTATE_REQ_WRITE_BACK  REQ_WRITE_BACK;
    MEMSTATE_REQ_REWIND      REQ_REWIND;
}
MEMSTATE_REQ
    deriving (Eq, Bits);



// ===================================================================
//
//   MEMSTATE_RESP -- Responses from memory operations in the
//                    functional partition.
//
// ===================================================================

typedef struct
{
     FUNCP_MEMREF_TOKEN memRefToken;
     // Value has meaning only for loads
     MEM_VALUE value;
     // Operation succeeded.  Not used by loads, but is used by almost everything
     // else.  (E.g. store conditional and commit doesn't violate a memory lock.)
     Bool success;
}
MEMSTATE_RESP
    deriving (Eq, Bits);


function MEMSTATE_RESP memStateRespLoad(FUNCP_MEMREF_TOKEN memRefTok,
                                        MEM_VALUE value);
    return MEMSTATE_RESP { memRefToken: memRefTok, value: value, success: True };
endfunction

function MEMSTATE_RESP memStateRespStatus(FUNCP_MEMREF_TOKEN memRefTok,
                                          Bool success);
    return MEMSTATE_RESP { memRefToken: memRefTok, value: ?, success: success };
endfunction


// ===================================================================
//
//   TLB
//
// ===================================================================

//
// Query passed to TLB service.  Normal queries and faults have the same
// payload, but different semantics.  Normal queries may fail is the page
// is not mapped.  Fault requests allocate the page and return no response.
//
typedef struct
{
    FUNCP_VADDR va;
    CONTEXT_ID contextId;

    // Set when querying multiple pages to satisfy a single model request.
    // E.g. Address range spans a page boundary.
    Bool notLastQuery;
}
FUNCP_TLB_QUERY
    deriving (Eq, Bits);

typedef FUNCP_TLB_QUERY FUNCP_TLB_FAULT;

//
// Helper functions for constructing FUNCP_TLB_QUERY
//
function FUNCP_TLB_QUERY normalTLBQuery(CONTEXT_ID ctx, FUNCP_VADDR va);
    return FUNCP_TLB_QUERY { va: va, contextId: ctx, notLastQuery: False };
endfunction

function FUNCP_TLB_FAULT handleTLBPageFault(CONTEXT_ID ctx, FUNCP_VADDR va);
    return FUNCP_TLB_FAULT { va: va, contextId: ctx, notLastQuery: False };
endfunction


//
// Response from TLB service.  When pageFault is clear, pa holds the valid
// translation.  When pageFault is set the translation failed.  The functional
// model should service page faults on attempts to commit the token.
//
// Note:  even when pageFault is set, the pa may still be used for references.
//        In this case, pa is set to the guard page.  This allows simple timing
//        models to proceed with minimal knowledge of exception handling.
//
typedef struct
{
    Bool ioSpace;          // Memory mapped I/O.
    Bool pageFault;        // Translation failed.  Raised a page fault.
    MEM_ADDRESS pa;
}
FUNCP_TLB_RESP
    deriving (Eq, Bits);


//
// TLB type (instruction or data)
//
typedef enum
{
    FUNCP_ITLB,
    FUNCP_DTLB
}
FUNCP_TLB_TYPE
    deriving (Eq, Bits);
