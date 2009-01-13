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


//
// Context ID as passed through RRR.  The #define of AppendCONTEXT_ID_RRR is
// a horrible hack but works with the current version of RRR.  This will
// eventually go away.
//
#if (CONTEXT_ID_BITS_RRR == 8)

typedef UINT8 CONTEXT_ID_RRR;
#define AppendCONTEXT_ID_RRR AppendUINT8
#define ExtractCONTEXT_ID_RRR ExtractUINT8

#elif (CONTEXT_ID_BITS_RRR == 16)

typedef UINT16 CONTEXT_ID_RRR;
#define AppendCONTEXT_ID_RRR AppendUINT16
#define ExtractCONTEXT_ID_RRR ExtractUINT16

#elif (CONTEXT_ID_BITS_RRR == 32)

typedef UINT32 CONTEXT_ID_RRR;
#define AppendCONTEXT_ID_RRR AppendUINT32
#define ExtractCONTEXT_ID_RRR ExtractUINT32

#elif (CONTEXT_ID_BITS_RRR == 64)

typedef UINT64 CONTEXT_ID_RRR;
#define AppendCONTEXT_ID_RRR AppendUINT64
#define ExtractCONTEXT_ID_RRR ExtractUINT64

#endif

typedef CONTEXT_ID_RRR CONTEXT_ID;

#define NUM_CONTEXTS (1 << CONTEXT_ID_BITS)
