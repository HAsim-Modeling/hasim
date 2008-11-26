// memory_virtual_device_datatypes

// A template for datatype definitions for the Memory Virtual Device.

// You probably won't need to change these.

`include "funcp_base_types.bsh"
`include "funcp_memory.bsh"

import Vector::*;

// ***** Datatype definitions *****

// MEM_ADDRESS

// The address space the memory virtual device uses. A parameter by default.

typedef FUNCP_PADDR MEM_ADDRESS;


// MEM_VALUE

// The type of values stored in memory. A parameter by default.

typedef FUNCP_INT_REG MEM_VALUE;

// MEM_OFFSET

// The part that's left over after you align an address. 
// TODO: Should be derived from MEM_VALUE size rather than hardcoded.

typedef Bit#(3) MEM_OFFSET;

// MEM_CACHELINE

// This is defined here because size of the cache-line must be known outside of
// the cache.

typedef TDiv#(`FUNCP_CACHELINE_BITS,`FUNCP_ISA_INT_REG_SIZE) CACHELINE_WORDS;
typedef Vector#(CACHELINE_WORDS, MEM_VALUE) MEM_CACHELINE;

// MEM_REQUEST

// A request to the memory virtual device is either a load or a store.

typedef struct
{
    MEM_ADDRESS addr;
    Bool        iStream;        // True iff load is fetching an instruction
}
MEM_LOAD_INFO
    deriving
        (Eq, Bits);

typedef struct
{
    MEM_ADDRESS addr;
    MEM_VALUE   val;
}
MEM_STORE_INFO
    deriving
        (Eq, Bits);


typedef struct
{
    MEM_ADDRESS addr;
    MEM_CACHELINE val;
}
MEM_STORE_CACHELINE_INFO
    deriving
        (Eq, Bits);


typedef struct
{
    Bool onlyFlush;             // Don't have to invalidate -- just flush stores
    UInt#(8) nLines;
    MEM_ADDRESS addr;
}
MEM_INVAL_CACHELINE_INFO
    deriving
        (Eq, Bits);


typedef union tagged 
{
    MEM_LOAD_INFO  MEM_LOAD;
    MEM_STORE_INFO MEM_STORE;

    MEM_ADDRESS MEM_LOAD_CACHELINE;
    MEM_STORE_CACHELINE_INFO MEM_STORE_CACHELINE;      // Store with no ACK from server
    MEM_STORE_CACHELINE_INFO MEM_STORE_CACHELINE_SYNC; // Store with ACK from server

    MEM_ADDRESS MEM_INVALIDATE_CACHELINE;
    MEM_ADDRESS MEM_FLUSH_CACHELINE;
}
MEM_REQUEST
    deriving
        (Eq, Bits);


function MEM_REQUEST funcpMemLoadReq(MEM_ADDRESS addr, Bool iStream);
    return tagged MEM_LOAD MEM_LOAD_INFO { addr: addr, iStream: iStream };
endfunction

function MEM_REQUEST funcpMemStoreReq(MEM_ADDRESS addr, MEM_VALUE value);
    return tagged MEM_STORE MEM_STORE_INFO { addr: addr, val: value };
endfunction


typedef union tagged 
{
    MEM_VALUE     MEM_REPLY_LOAD;
    MEM_CACHELINE MEM_REPLY_LOAD_CACHELINE;
    Bool          MEM_REPLY_STORE_CACHELINE_ACK;
}
MEM_REPLY
    deriving
        (Eq, Bits);



// ***** RRR Datatype definitions *****

//
// Until RRR can deal with complex types that aren't aligned on 32 bit boundaries
// we will use these types for passing data between HW and SW.  The types above
// are compact for use on the FPGA.  The types below are expanded to simplify
// data transfer.
//

typedef Bit#(64) MEM_ADDRESS_RRR;

typedef struct
{
    MEM_ADDRESS_RRR addr;
    MEM_VALUE val;
}
MEM_STORE_INFO_RRR
    deriving
        (Eq, Bits);


typedef struct
{
    MEM_ADDRESS_RRR addr;
    MEM_CACHELINE val;
}
MEM_STORE_CACHELINE_INFO_RRR
    deriving
        (Eq, Bits);


typedef struct
{
    Bit#(23) unused;
    Bool onlyFlush;             // Don't have to invalidate -- just flush stores
    UInt#(8) nLines;
    MEM_ADDRESS_RRR addr;
}
MEM_INVAL_CACHELINE_INFO_RRR
    deriving
        (Eq, Bits);
