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
    memory(NULL),
    stWordIdx(0)
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
//     Request one line from memory.  Words from the requested line are sent
//     as separate messages to the LoadData remote server method.
//
void
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

    //
    // Send each word in the line to the FPGA
    //
    for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++)
    {
        clientStub->LoadData(line.w[i]);
    }
}

//
// StoreLine --
//     Control message for a store line request.  Data will follow on the
//     StoreData method.
//
void
FUNCP_MEMORY_SERVER_CLASS::StoreLine(
    CONTEXT_ID ctxId,
    UINT8 wordValid,
    UINT8 sendAck,
    FUNCP_PADDR addr)
{
    ASSERTX(memory != NULL);
    // Hardware MUST send all store data before requesting a new store line
    ASSERT(stWordIdx == 0, "New store line request before last line's data complete");

    stWordIdx = 1;
    stCtxId = ctxId;
    stWordValid = wordValid;
    stSendAck = sendAck;
    stAddr = addr;
}

//
// StoreData --
//     Data associated with a StoreLine request.  Data arrives one word at a
//     time and is written to memory once the entire line arrives.
//
void
FUNCP_MEMORY_SERVER_CLASS::StoreData(MEM_VALUE val)
{
    ASSERTX(stWordIdx <= (sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE)));

    stData.w[stWordIdx - 1] = val;
    if (stWordIdx != (sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE)))
    {
        // Not done with line
        stWordIdx += 1;
        return;
    }

    // Done with line.  Write it to memory.
    stWordIdx = 0;

    if (TRACING(1))
    {
        MEM_CACHELINE_WORD_VALID_MASK mask_pos = 1;

        T1("\tfuncp_memory: STline CTX " << UINT64(stCtxId) << (stSendAck ? " SYNC" : "") << " (" << sizeof(MEM_CACHELINE) << ") [" << fmt_addr(stAddr) << "] -> line:");
        for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
            if ((stWordValid & mask_pos) != 0)
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

    if (stWordValid == ((1 << sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE)) - 1))
    {
        // Entire line is valid.  Write in a single chunk.
        memory->Write(stCtxId, stAddr, sizeof(MEM_CACHELINE), &stData);
    }
    else
    {
        // Write only the valid words in the line.  Assumes little endian
        // addressing.
        MEM_CACHELINE_WORD_VALID_MASK mask_pos = 1;
        FUNCP_PADDR w_addr = stAddr;
        for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
            if ((stWordValid & mask_pos) != 0)
            {
                memory->Write(stCtxId, w_addr, sizeof(MEM_VALUE), &stData.w[i]);
            }
            w_addr += sizeof(MEM_VALUE);
            mask_pos <<= 1;
        }
    }

    if (stSendAck)
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
