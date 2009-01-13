//
// Copyright (C) 2009 Intel Corporation
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

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/funcp_base_types.bsh"

//
// Base types for memory state in the functional partition
//


//
// MEM_ADDRESS
//
// The address space the memory virtual device uses. A parameter by default.
//
typedef FUNCP_PADDR MEM_ADDRESS;


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
//   MEMSTATE_REQ -- requests for memory operations from the
//                   functional partition.
//
// ===================================================================


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
    TOKEN tok;
}
FUNCP_TLB_QUERY
    deriving (Eq, Bits);

typedef FUNCP_TLB_QUERY FUNCP_TLB_FAULT;

//
// Helper functions for constructing FUNCP_TLB_QUERY
//
function FUNCP_TLB_QUERY normalTLBQuery(TOKEN tok, FUNCP_VADDR va);
    return FUNCP_TLB_QUERY { va: va, tok: tok };
endfunction

function FUNCP_TLB_FAULT handleTLBPageFault(TOKEN tok, FUNCP_VADDR va);
    return FUNCP_TLB_FAULT { va: va, tok: tok };
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