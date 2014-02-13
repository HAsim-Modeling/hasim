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
#include "asim/provides/hasim_common.h"

//
// Integer register size.  For now also used as the fundamental access size
// for functional hybrid memory.
//
#if (FUNCP_ISA_INT_REG_SIZE == 32)
typedef UINT32 FUNCP_INT_REG;
#else
typedef UINT64 FUNCP_INT_REG;
#endif

typedef double FUNCP_FP_REG;

//
// Union of all possible register value types
//
typedef union
{
    FUNCP_INT_REG intReg;
    FUNCP_FP_REG fpReg;
}
FUNCP_REG;

//
// Address size.  Also defines the PC size.
//
#if (FUNCP_ISA_V_ADDR_SIZE == 32)
typedef UINT32 FUNCP_VADDR;
#else
typedef UINT64 FUNCP_VADDR;
#endif

#if (FUNCP_ISA_P_ADDR_SIZE == 32)
typedef UINT32 FUNCP_PADDR;
#else
typedef UINT64 FUNCP_PADDR;
#endif

// More RRR hacks.  For now a physical address is always passed as 64 bits.
typedef UINT64 FUNCP_PADDR_RRR;
#define AppendFUNCP_PADDR_RRR AppendUINT64
#define ExtractFUNCP_PADDR_RRR ExtractUINT64


typedef FUNCP_INT_REG MEM_VALUE;

#if (FUNCP_ISA_INT_REG_SIZE == 32)
#define AppendMEM_VALUE AppendUINT32
#define ExtractMEM_VALUE ExtractUINT32
#else
#define AppendMEM_VALUE AppendUINT64
#define ExtractMEM_VALUE ExtractUINT64
#endif

//
// Context ID as passed through RRR.  The #define of AppendCONTEXT_ID_RRR is
// a horrible hack but works with the current version of RRR.  This will
// eventually go away.
//
#if (CONTEXT_ID_BITS_RRR <= 8)

typedef UINT8 CONTEXT_ID_RRR;
#define AppendCONTEXT_ID_RRR AppendUINT8
#define ExtractCONTEXT_ID_RRR ExtractUINT8

#elif (CONTEXT_ID_BITS_RRR <= 16)

typedef UINT16 CONTEXT_ID_RRR;
#define AppendCONTEXT_ID_RRR AppendUINT16
#define ExtractCONTEXT_ID_RRR ExtractUINT16

#elif (CONTEXT_ID_BITS_RRR <= 32)

typedef UINT32 CONTEXT_ID_RRR;
#define AppendCONTEXT_ID_RRR AppendUINT32
#define ExtractCONTEXT_ID_RRR ExtractUINT32

#elif (CONTEXT_ID_BITS_RRR <= 64)

typedef UINT64 CONTEXT_ID_RRR;
#define AppendCONTEXT_ID_RRR AppendUINT64
#define ExtractCONTEXT_ID_RRR ExtractUINT64

#endif

typedef CONTEXT_ID_RRR CONTEXT_ID;

#define NUM_CONTEXTS (1 << CONTEXT_ID_BITS)
