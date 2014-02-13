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

//
// Base types for functional partition
//

//
// Integer register size.  For now also used as the fundamental access size
// for functional hybrid memory.
//
typedef Bit#(`FUNCP_ISA_INT_REG_SIZE) FUNCP_INT_REG;

//
// Virtual address size.
//
typedef Bit#(`FUNCP_ISA_V_ADDR_SIZE) FUNCP_VADDR;

//
// Physical address size.
//
typedef `FUNCP_ISA_P_ADDR_SIZE FUNCP_PADDR_SIZE;
typedef Bit#(`FUNCP_ISA_P_ADDR_SIZE) FUNCP_PADDR;

//
// Page of memory (FUNCP_PAGE) and offset in a page.
//
typedef Bit#(TSub#(`FUNCP_ISA_V_ADDR_SIZE, `FUNCP_ISA_PAGE_SHIFT)) FUNCP_V_PAGE;
typedef Bit#(TSub#(`FUNCP_ISA_P_ADDR_SIZE, `FUNCP_ISA_PAGE_SHIFT)) FUNCP_P_PAGE;
typedef Bit#(`FUNCP_ISA_PAGE_SHIFT) FUNCP_PAGE_OFFSET;

//
// Break the PC into bits with interesting values and bits that must be 0.
//
typedef Bit#(`FUNCP_ISA_PC_MIN_ALIGN) FUNCP_PC_ALIGN_PART;
typedef Bit#(TSub#(`FUNCP_ISA_V_ADDR_SIZE, `FUNCP_ISA_PC_MIN_ALIGN)) FUNCP_PC_IDX_PART;

//
// pcAddrWithoutAlignmentBits --
//   Remove the low alignment bits in the PC that are either always 0 or aren't
//   used to point to an instruction.
//
function FUNCP_PC_IDX_PART pcAddrWithoutAlignmentBits(FUNCP_VADDR va);
    Tuple2#(FUNCP_PC_IDX_PART, FUNCP_PC_ALIGN_PART) p = unpack(va);
    return tpl_1(p);
endfunction

//
// pcAddAlignmentBits --
//   Restore a shorted PC returned by pcAddrWithoutAlignmentBits() to a full
//   virtual address.
//
function FUNCP_VADDR pcAddAlignmentBits(FUNCP_PC_IDX_PART idx);
    FUNCP_PC_ALIGN_PART a = 0;
    return { idx, a };
endfunction
