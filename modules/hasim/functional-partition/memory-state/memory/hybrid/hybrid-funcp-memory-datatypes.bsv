// memory_virtual_device_datatypes

// A template for datatype definitions for the Memory Virtual Device.

// You probably won't need to change these.

`include "funcp_base_types.bsh"
`include "funcp_memory.bsh"

import Vector::*;

// ***** Datatype definitions *****

// MEM_ADDRESS

// The address space the memory virtual device uses. A parameter by default.

typedef FUNCP_ADDR MEM_ADDRESS;


// MEM_VALUE

// The type of values stored in memory. A parameter by default.

typedef FUNCP_INT_REG MEM_VALUE;

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

typedef union tagged 
{
    MEM_ADDRESS MEM_LOAD;
    MEM_STORE_INFO MEM_STORE;

    MEM_ADDRESS MEM_LOAD_CACHELINE;
    MEM_STORE_CACHELINE_INFO MEM_STORE_CACHELINE;

    MEM_ADDRESS MEM_INVALIDATE_CACHELINE;
}
MEM_REQUEST
    deriving
        (Eq, Bits);

typedef union tagged 
{
    MEM_VALUE     MEM_REPLY_LOAD;
    MEM_CACHELINE MEM_REPLY_LOAD_CACHELINE;
}
MEM_REPLY
    deriving
        (Eq, Bits);

