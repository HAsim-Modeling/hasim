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
FUNCP_TLB_SERVER_CLASS::FUNCP_TLB_SERVER_CLASS() :
    memory(NULL),
    maxContexts(0),
    dActivateAddrs(NULL),
    iActivateAddrs(NULL)
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

    // Get a pointer to simulated memory from the memory server.  This
    // should be fixed to have a common parent allocate the memory.
    memory = FUNCP_MEMORY_SERVER_CLASS::GetMemoryHandle();
    ASSERTX(memory != NULL);

    // Buckets for storing activation requests
    maxContexts = memory->NumCPUs();
    dActivateAddrs = new MEM_VALUE[maxContexts];
    iActivateAddrs = new MEM_VALUE[maxContexts];
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
    maxContexts = 0;
    delete[] dActivateAddrs;
    delete[] iActivateAddrs;
}


//
// RRR requests
//

// request
OUT_TYPE_VtoP
FUNCP_TLB_SERVER_CLASS::VtoP(CONTEXT_ID ctxId, MEM_VALUE va, UINT8 reqWordIdx)
{
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

        // Has the model forced delayed translation of this address by calling
        // ActivateVAddr?
        if ((xlate_va == iActivateAddrs[ctxId]) ||
            (xlate_va == dActivateAddrs[ctxId]))
        {
            do_alloc = true;
        }

        if (do_alloc)
        {
            T1("\tfuncp_memory: VtoP VA " << fmt_data(xlate_va) << " -- allocate on fault");
        }

        FUNCP_MEM_VTOP_RESP vtop = memory->VtoP(ctxId, xlate_va, do_alloc);

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


void
FUNCP_TLB_SERVER_CLASS::ActivateVAddr(
    CONTEXT_ID ctxId,
    MEM_VALUE va,
    UINT8 isITranslate)
{
    //
    // A request to activate a virtual address for which translation failed.
    // This path typically is reached after a VA->PA fault is raised and
    // handled during commit.
    //
    // Simulation becomes non-deterministic if the memory is allocated during
    // fault handling since there is no order guarantee in the simulator
    // between the fault handler of one context and the translation phase
    // of another context.  Delay allocation until this context requests the
    // translation again.
    //

    const char *i_or_d = isITranslate ? "I" : "D";
    T1("\tfuncp_memory: ActivateAddr " << i_or_d << " CTX " << UINT64(ctxId) << " VA " << fmt_data(va));

    //
    // Each context gets a bucket for instruction and another for data
    // translation.
    //
    VERIFYX(ctxId < maxContexts);
    if (isITranslate)
    {
        iActivateAddrs[ctxId] = va;
    }
    else
    {
        dActivateAddrs[ctxId] = va;
    }
}
