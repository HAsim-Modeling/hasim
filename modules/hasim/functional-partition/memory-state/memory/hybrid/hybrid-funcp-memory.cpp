//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
//
 
#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"

#include "asim/provides/model.h"
#include "asim/provides/funcp_memory.h"
#include "asim/provides/funcp_simulated_memory.h"
#include "asim/provides/rrr.h"

#include "asim/rrr/service_ids.h"

// service instantiation
FUNCP_MEMORY_SERVER_CLASS FUNCP_MEMORY_SERVER_CLASS::instance;

// constructor
FUNCP_MEMORY_SERVER_CLASS::FUNCP_MEMORY_SERVER_CLASS() :
    memory(NULL)
{
    SetTraceableName("funcp_memory");

    // instantiate stubs
    serverStub = new FUNCP_MEMORY_SERVER_STUB_CLASS(this);
    clientStub = new FUNCP_MEMORY_CLIENT_STUB_CLASS(this);

    char fmt[16];

    sprintf(fmt, "0%dx", sizeof(FUNCP_PADDR) * 2);
    fmt_addr = Format("0x", fmt);

    sprintf(fmt, "0%dx", sizeof(MEM_VALUE) * 2);
    fmt_data = Format("0x", fmt);
}

// destructor
FUNCP_MEMORY_SERVER_CLASS::~FUNCP_MEMORY_SERVER_CLASS()
{
    Cleanup();
}

// init
void
FUNCP_MEMORY_SERVER_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;

    memory = new FUNCP_SIMULATED_MEMORY_CLASS();
}

// uninit: override
void
FUNCP_MEMORY_SERVER_CLASS::Uninit()
{
    // cleanup
    Cleanup();
    
    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
FUNCP_MEMORY_SERVER_CLASS::Cleanup()
{
    delete memory;

    // deallocate stubs
    delete serverStub;
    delete clientStub;
}

// poll
void
FUNCP_MEMORY_SERVER_CLASS::Poll()
{
    // do nothing
}


//
// Load --
//     Load one word from memory.
//
MEM_VALUE
FUNCP_MEMORY_SERVER_CLASS::Load(CONTEXT_ID ctxId, FUNCP_PADDR addr)
{
    ASSERTX(memory != NULL);

    MEM_VALUE data;
    memory->Read(ctxId, addr, sizeof(MEM_VALUE), &data);
    T1("\tfuncp_memory: LD CTX " << UINT64(ctxId) << " (" << sizeof(MEM_VALUE) << ") [" << fmt_addr(addr) << "] -> " << fmt_data(data));

    return data;
}

//
// Store --
//     Store one word to memory.
//
void
FUNCP_MEMORY_SERVER_CLASS::Store(
    CONTEXT_ID ctxId,
    FUNCP_PADDR addr,
    MEM_VALUE val)
{
    ASSERTX(memory != NULL);

    T1("\tfuncp_memory: ST CTX " << UINT64(ctxId) << " (" << sizeof(MEM_VALUE) << ") [" << fmt_addr(addr) << "] <- " << fmt_data(val));
    memory->Write(ctxId, addr, sizeof(MEM_VALUE), &val);
}

//
// LoadLine --
//     Request one line from memory.
//
OUT_TYPE_LoadLine
FUNCP_MEMORY_SERVER_CLASS::LoadLine(CONTEXT_ID ctxId, FUNCP_PADDR addr)
{
    ASSERTX(memory != NULL);

    //
    // Read line in simulator
    //
    MEM_CACHELINE line;
    memory->Read(ctxId, addr, sizeof(MEM_CACHELINE), &line);
    if (TRACING(1))
    {
        T1("\tfuncp_memory: LDline CTX " << UINT64(ctxId) << " (" << sizeof(MEM_CACHELINE) << ") [" << fmt_addr(addr) << "] -> line:");
        for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
            T1("\t\t" << fmt_data(line.w[i]));
        }
    }

    OUT_TYPE_LoadLine v;
    v.data0 = line.w[0];
    v.data1 = line.w[1];
    v.data2 = line.w[2];
    v.data3 = line.w[3];
    return v;
}

//
// StoreLine --
//     Store a full data line.  The code assumes 4 words per line and little
//     endian memory.  The data can be made more general after RRR understands
//     types.
//
void
FUNCP_MEMORY_SERVER_CLASS::StoreLine(
    CONTEXT_ID ctxId,
    UINT8 wordValid,
    UINT8 sendAck,
    FUNCP_PADDR addr,
    MEM_VALUE word0, MEM_VALUE word1, MEM_VALUE word2, MEM_VALUE word3)
{
    ASSERTX(memory != NULL);

    // Convert value to a clean data structure
    MEM_CACHELINE stData;
    stData.w[0] = word0;
    stData.w[1] = word1;
    stData.w[2] = word2;
    stData.w[3] = word3;

    if (TRACING(1))
    {
        MEM_CACHELINE_WORD_VALID_MASK mask_pos = 1;

        T1("\tfuncp_memory: STline CTX " << UINT64(ctxId) << (sendAck ? " SYNC" : "") << " (" << sizeof(MEM_CACHELINE) << ") [" << fmt_addr(addr) << "] -> line:");
        for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
            if ((wordValid & mask_pos) != 0)
            {
                T1("\t\t" << fmt_data(stData.w[i]));
            }
            else
            {
                T1("\t\tXXXXXXXX");
            }

            mask_pos <<= 1;
        }
    }

    if (wordValid == ((1 << sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE)) - 1))
    {
        // Entire line is valid.  Write in a single chunk.
        memory->Write(ctxId, addr, sizeof(MEM_CACHELINE), &stData);
    }
    else
    {
        // Write only the valid words in the line.  Assumes little endian
        // addressing.
        MEM_CACHELINE_WORD_VALID_MASK mask_pos = 1;
        FUNCP_PADDR w_addr = addr;
        for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
            if ((wordValid & mask_pos) != 0)
            {
                memory->Write(ctxId, w_addr, sizeof(MEM_VALUE), &stData.w[i]);
            }
            w_addr += sizeof(MEM_VALUE);
            mask_pos <<= 1;
        }
    }

    if (sendAck)
    {
        clientStub->StoreACK(1);
    }
}


//***********************************************************************
//
// FPGA-side cache management.  When simulated memory is modified or
// read by software (e.g. instruction emulation) these routines flush
// or invalidate the FPGA-side cache.
//
//***********************************************************************

void
FUNCP_MEMORY_SERVER_CLASS::NoteSystemMemoryRead(CONTEXT_ID ctxId, FUNCP_PADDR addr, FUNCP_PADDR size)
{
    instance.SystemMemoryRef(ctxId, addr, size, false);
}

void
FUNCP_MEMORY_SERVER_CLASS::NoteSystemMemoryWrite(CONTEXT_ID ctxId, FUNCP_PADDR addr, FUNCP_PADDR size)
{
//    NoteSystemMemoryWriteUnknownAddr();
    instance.SystemMemoryRef(ctxId, addr, size, true);
}

void
FUNCP_MEMORY_SERVER_CLASS::SystemMemoryRef(CONTEXT_ID ctxId, FUNCP_PADDR addr, UINT64 size, bool isWrite)
{
    T1("\tfuncp_memory: Note system memory " << (isWrite ? "WRITE" : "READ") << " (Addr=" << fmt_addr(addr) << " bytes=" << fmt_addr(size) << ")");

    FUNCP_PADDR endAddr = addr + size + sizeof(MEM_CACHELINE) - 1;

    // Cache-line align the start and end address
    addr &= ~ FUNCP_PADDR(sizeof(MEM_CACHELINE) - 1);
    endAddr &= ~ FUNCP_PADDR(sizeof(MEM_CACHELINE) - 1);

    UINT64 nLines = (endAddr - addr) / sizeof(MEM_CACHELINE);

    while (nLines > 0)
    {
        if (isWrite)
        {
            // Flush pending stores & invalidate in preparation for writes.
            T1("\tfuncp_memory: INVAL Addr=" << fmt_addr(addr));
        }
        else
        {
            // Just flush pending stores for reads.  No need to invalidate.
            T1("\tfuncp_memory: FLUSH Addr=" << fmt_addr(addr));
        }

        clientStub->Invalidate(ctxId, addr, isWrite ? 0 : 0xff);

        addr += sizeof(MEM_CACHELINE);
        nLines -= 1;
    }
}



//***********************************************************************
//
// Expose the handle to simulated memory to any other services that may
// need it.  This should be fixed to have a common parent construct
// the memory.
//
//***********************************************************************

FUNCP_SIMULATED_MEMORY
FUNCP_MEMORY_SERVER_CLASS::GetMemoryHandle()
{
    return instance.memory;
}
