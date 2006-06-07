
//XXX For performance reasons we're using primitive arrays here
//    In the future maybe could use Vectors.

typedef PrimArray#(inst_T) Program#(type inst_T);

typedef PrimArray#(val_T) MemoryState#(type val_T);

typedef struct
{
  Program#(inst_T) imem_init;
  MemoryState#(val_T) dmem_init;
  MemoryState#(val_T) dmem_exp;
}
  TestCase#(type inst_T, type val_T);


function TestCase#(inst_T, val_T) 
  mkTestCase (Program#(inst_T) prog, 
              MemoryState#(val_T) dmem_i,
	      MemoryState#(val_T) dmem_e);

  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i, 
	   dmem_exp: dmem_e
	 };
	 
endfunction
