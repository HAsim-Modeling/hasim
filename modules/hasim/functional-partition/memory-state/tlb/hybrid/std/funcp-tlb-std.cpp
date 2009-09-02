//
// Copyright (C) 2009 Intel Corporation
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
// Functional TLB.
//

#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"

#include "asim/provides/funcp_memory.h"
#include "asim/provides/funcp_memstate_tlb.h"
#include "asim/rrr/service_ids.h"


// service instantiation
FUNCP_TLB_SERVER_CLASS FUNCP_TLB_SERVER_CLASS::instance;

// constructor
FUNCP_TLB_SERVER_CLASS::FUNCP_TLB_SERVER_CLASS()
{
    SetTraceableName("funcp_tlb");

    // instantiate stubs
    serverStub = new FUNCP_TLB_SERVER_STUB_CLASS(this);

    char fmt[16];

    sprintf(fmt, "0%dx", sizeof(FUNCP_PADDR) * 2);
    fmt_addr = Format("0x", fmt);

    sprintf(fmt, "0%dx", sizeof(MEM_VALUE) * 2);
    fmt_data = Format("0x", fmt);
}

// destructor
FUNCP_TLB_SERVER_CLASS::~FUNCP_TLB_SERVER_CLASS()
{
    Cleanup();
}

// init
void
FUNCP_TLB_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    // set parent pointer
    parent = p;
}

// uninit: override
void
FUNCP_TLB_SERVER_CLASS::Uninit()
{
    // cleanup
    Cleanup();
    
    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
FUNCP_TLB_SERVER_CLASS::Cleanup()
{
}

// poll
void
FUNCP_TLB_SERVER_CLASS::Poll()
{
    // do nothing
}



//
// RRR requests
//

// request
OUT_TYPE_VtoP
FUNCP_TLB_SERVER_CLASS::VtoP(CONTEXT_ID ctxId, MEM_VALUE va, UINT8 reqWordIdx)
{
    // Get a pointer to simulated memory from the memory server.  This
    // should be fixed to have a common parent allocate the memory.
    FUNCP_SIMULATED_MEMORY memory = FUNCP_MEMORY_SERVER_CLASS::GetMemoryHandle();
    ASSERTX(memory != NULL);

    //
    // The allocate on fault bit is stored in the low bit of the request
    // to save space.
    //
    bool alloc_on_fault = false;
    if (va & 1)
    {
        alloc_on_fault = true;
        va ^= 1;    // Clear low bit
    }

    //
    // Client expects to cache a group of translations in the central cache.
    // Central cache entries have 4 lines.  There is currently no good way
    // to describe a variable number of elements in RRR, so the number of
    // entries is hard wired here.
    //
    FUNCP_PADDR xlate[4];
    for (int i = 0; i < 4; i++)
    {
        // Translate one page.  The allocate on fault bit applies only to the
        // requested entry, indicated by reqWordIdx.
        //
        bool do_alloc = alloc_on_fault && (reqWordIdx == i);
        MEM_VALUE xlate_va = va + (1 << FUNCP_ISA_PAGE_SHIFT) * i;

        FUNCP_MEM_VTOP_RESP vtop = memory->VtoP(ctxId, xlate_va, do_alloc);

        if (do_alloc)
        {
            T1("\tfuncp_memory: VtoP VA " << fmt_data(xlate_va) << " -- allocate on fault");
        }

        T1("\tfuncp_memory: VtoP CTX " << UINT64(ctxId) << " VA " << fmt_data(xlate_va) << " -> PA " << fmt_addr(vtop.pa) <<
           (vtop.ioSpace ? " [I/O SPACE]" : "") <<
           (vtop.pageFault ? " [PAGE FAULT]" : ""));

        // Flags will be sent in the low two bits.  They must be 0 in
        // the translation.
        ASSERTX((vtop.pa & 3) == 0);

        // Pass the ioSpace and pageFault bits in the low bits of the PA to
        // save space.
        xlate[i] = vtop.pa | (vtop.ioSpace ? 2 : 0) | (vtop.pageFault ? 1 : 0);
    }

    //
    // Return translations as named fields instead of an array due to RRR
    // limitations.
    //
    OUT_TYPE_VtoP resp;
    resp.pa0 = xlate[0];
    resp.pa1 = xlate[1];
    resp.pa2 = xlate[2];
    resp.pa3 = xlate[3];
    return resp;
}
