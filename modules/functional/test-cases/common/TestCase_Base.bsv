
import ISA::*;

//XXX For performance reasons we're using primitive arrays here
//    In the future maybe could use Vectors.

typedef PrimArray#(Inst) Program;

typedef PrimArray#(Value) MemoryState;

typedef struct
{
  Program     imem_init;
  MemoryState dmem_init;
  MemoryState dmem_exp;
}
  TestCase;
