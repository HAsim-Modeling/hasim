// memory_virtual_device_datatypes

// A template for datatype definitions for the Memory Virtual Device.

// You probably won't need to change these.


// ***** Datatype definitions *****


// MEM_ADDRESS

// The address space the memory virtual device uses. A parameter by default.

typedef Bit#(`MEMORY_ADDR_SIZE) MEM_ADDRESS;


// MEM_VALUE

// The type of values stored in memory. A parameter by default.

typedef Bit#(`MEMORY_VALUE_SIZE) MEM_VALUE;


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

typedef union tagged 
{
  MEM_ADDRESS MEM_LOAD;
  struct {MEM_ADDRESS addr; MEM_VALUE val; } MEM_STORE;
}
  MEM_REQUEST
    deriving
            (Eq, Bits);

