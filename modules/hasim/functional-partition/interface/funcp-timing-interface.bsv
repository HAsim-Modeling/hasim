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

//
// Interface from a timing model to the functional partition.
//

// Project foundation includes

`include "asim/provides/hasim_common.bsh"

//
// Functional partition includes.  BEWARE!  Timing models include this file.
// Do not include anything but functional partition data definitions.
//
`include "asim/provides/funcp_base_types.bsh"
`include "asim/provides/funcp_memstate_base_types.bsh"

// ISA includes

`include "asim/provides/hasim_isa.bsh"

// FUNCP_REQ_NEW_IN_FLIGHT

typedef struct
{
    CONTEXT_ID context_id;
}
    FUNCP_REQ_NEW_IN_FLIGHT
        deriving (Eq, Bits);


// FUNCP_RSP_NEW_IN_FLIGHT

typedef struct
{
    TOKEN newToken;
}
    FUNCP_RSP_NEW_IN_FLIGHT
        deriving (Eq, Bits);


// FUNCP_REQ_DO_ITRANSLATE

typedef struct
{
    CONTEXT_ID  contextId;
    ISA_ADDRESS virtualAddress;  // Virtual address to translate
}
    FUNCP_REQ_DO_ITRANSLATE
        deriving (Eq, Bits);


// FUNCP_RSP_DO_ITRANSLATE

typedef struct
{
    CONTEXT_ID  contextId;
    MEM_ADDRESS physicalAddress;  // Result of translation.
    MEM_OFFSET  offset;           // Offset of the instruction.
    Bool        fault;            // Translation failure:  fault will be raised on
                                  //   attempts to commit this token.  physicalAddress
                                  //   is on the guard page, so it can still be used
                                  //   in order to simplify timing model logic.
    Bool        hasMore;          // More translations coming? (IE the fetch spans two memory addresses.)
}
    FUNCP_RSP_DO_ITRANSLATE
        deriving (Eq, Bits);


// FUNCP_REQ_GET_INSTRUCTION

typedef struct
{
    CONTEXT_ID  contextId;
    MEM_ADDRESS physicalAddress; // The address to fetch from.
    MEM_OFFSET  offset;          // The offset into the chunk.
    Bool        hasMore;         // If the instruction spans chunks, then make a second request.
}
    FUNCP_REQ_GET_INSTRUCTION
        deriving (Eq, Bits);


// FUNCP_RSP_GET_INSTRUCTION

typedef struct
{
    CONTEXT_ID contextId;
    ISA_INSTRUCTION instruction; // The instruction at that physical address.
}
    FUNCP_RSP_GET_INSTRUCTION
        deriving (Eq, Bits);


// FUNCP_REQ_GET_DEPENDENCIES

typedef struct
{
    CONTEXT_ID contextId;
    Bool dummy;
    ISA_INSTRUCTION instruction;
    ISA_ADDRESS virtualAddress;
}
    FUNCP_REQ_GET_DEPENDENCIES
        deriving (Eq, Bits);


// FUNCP_RSP_GET_DEPENDENCIES

typedef struct
{
    TOKEN               token;      // The token that refers to this instruction from now on.
    ISA_SRC_MAPPING     srcMap;     // The mapping from architectural sources to physical sources.
    ISA_DST_MAPPING     dstMap;     // The mapping from architectural dests to physical dests.
}
    FUNCP_RSP_GET_DEPENDENCIES
        deriving (Eq, Bits);


// FUCNP_REQ_GET_RESULTS

typedef struct
{
    TOKEN token;
}
    FUNCP_REQ_GET_RESULTS
        deriving (Eq, Bits);


// FUCNP_RSP_GET_RESULTS

typedef struct
{
    TOKEN                token;
    ISA_ADDRESS          instructionAddress;     // Address of the executed instruction
    Bit#(4)              instructionSize;        // Size of the executed instruction
    FUNCP_ISA_EXECUTION_RESULT result;           // Result from ALU
}
    FUNCP_RSP_GET_RESULTS
        deriving (Eq, Bits);


// FUNCP_REQ_DO_DTRANSLATE

typedef struct
{
    TOKEN       token; 
}
    FUNCP_REQ_DO_DTRANSLATE
        deriving (Eq, Bits);


// FUNCP_RSP_DO_DTRANSLATE

typedef struct
{
    TOKEN       token; 
    MEM_ADDRESS physicalAddress;  // Result of translation.
    Bool        fault;            // Translation failure:  fault will be raised on
                                  //   attempts to commit this token.  physicalAddress
                                  //   is on the guard page, so it can still be used
                                  //   in order to simplify timing model logic.
    Bool        hasMore;          // More translations coming? (IE the request spans two memory addresses.)
}
    FUNCP_RSP_DO_DTRANSLATE
        deriving (Eq, Bits);


// FUNCP_REQ_DO_LOADS

typedef struct
{
    TOKEN token;
}
    FUNCP_REQ_DO_LOADS
        deriving (Eq, Bits);


// FUNCP_RSP_DO_LOADS

typedef struct
{
    TOKEN token;
}
    FUNCP_RSP_DO_LOADS
        deriving (Eq, Bits);


// FUNCP_REQ_DO_STORES

typedef struct
{
    TOKEN token;
}
    FUNCP_REQ_DO_STORES
        deriving (Eq, Bits);


// FUNCP_RSP_DO_STORES

typedef struct
{
    TOKEN token;
}
    FUNCP_RSP_DO_STORES
        deriving (Eq, Bits);


// FUNCP_REQ_COMMIT_RESULTS

typedef struct
{
    TOKEN token;
    Bool abort;
}
    FUNCP_REQ_COMMIT_RESULTS
        deriving (Eq, Bits);


// FUNCP_RSP_COMMIT_RESULTS

typedef struct
{
    TOKEN token;

    // At commit the handle to a store switches from a TOKEN to a STORE_TOKEN.
    // The TOKEN is now dead.  The STORE_TOKEN will be used to write the
    // store back to memory.
    Maybe#(STORE_TOKEN) storeToken;
    // If a fault occurred then this will be the address the timing model should redirect to.
    // The token was not actually committed, so the timing model should later rewind to the 
    // token before this one.
    Maybe#(ISA_ADDRESS) faultRedirect;
}
    FUNCP_RSP_COMMIT_RESULTS
        deriving (Eq, Bits);


// FUNCP_REQ_COMMIT_STORES

typedef struct
{
    STORE_TOKEN storeToken;
}
    FUNCP_REQ_COMMIT_STORES
        deriving (Eq, Bits);


// FUNCP_RSP_COMMIT_STORES

typedef struct
{
    STORE_TOKEN storeToken;
}
    FUNCP_RSP_COMMIT_STORES
        deriving (Eq, Bits);


// FUNCP_REQ_HANDLE_FAULT

typedef struct
{
    TOKEN token;
}
    FUNCP_REQ_HANDLE_FAULT
        deriving (Eq, Bits);


// FUNCP_RSP_HANDLE_FAULT

typedef struct
{
    TOKEN token;
    ISA_ADDRESS nextInstructionAddress;  // Resume pipeline here
}
    FUNCP_RSP_HANDLE_FAULT
        deriving (Eq, Bits);


// FUNCP_REQ_REWIND_TO_TOKEN

typedef struct
{
    TOKEN token;
}
    FUNCP_REQ_REWIND_TO_TOKEN
        deriving (Eq, Bits);


// FUNCP_RSP_REWIND_TO_TOKEN

typedef struct
{
    TOKEN token;
}
    FUNCP_RSP_REWIND_TO_TOKEN
        deriving (Eq, Bits);
