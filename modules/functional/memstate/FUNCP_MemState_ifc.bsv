
import hasim_common::*;

import hasim_isa::*;

/************* Memory System Interface *************/


// Data Memory request

typedef union tagged 
{
  struct {Token token; Addr addr;            } Ld;
  struct {Token token; Addr addr; Value val; } St;
}
  MemReq 
    deriving
            (Eq, Bits);


// Data Memory Response

typedef union tagged {
  Value LdResp;
  void  StResp;
}
  MemResp 
    deriving
            (Eq, Bits);


// Memory System Interface

// The memory system consists of two major parts: the IMem and DMem.
// The IMem is a simple Server (Address, Instruction)
// The DMem uses the above MemReq/MemResp types
// Additionally requests can be committed or killed in the DMem
// They are committed by Global Commit and killed by killToken

// For now the memory also has a "magic" link for the controller to
// load the test case. This may disappear in the future.


