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

#include "asim/provides/model.h"
#include "asim/provides/funcp_memory.h"
#include "asim/provides/funcp_simulated_memory.h"
#include "asim/provides/rrr.h"

#include "asim/rrr/service_ids.h"

#define METHOD_ID_Invalidate     0

// service instantiation
FUNCP_MEMORY_SERVER_CLASS FUNCP_MEMORY_SERVER_CLASS::instance;

// constructor
FUNCP_MEMORY_SERVER_CLASS::FUNCP_MEMORY_SERVER_CLASS() :
    memory(NULL)
{
    SetTraceableName("funcp_memory");

    // instantiate stubs
    serverStub = new FUNCP_MEMORY_SERVER_STUB_CLASS(this);

    char fmt[16];

    sprintf(fmt, "0%dx", sizeof(MEM_ADDRESS) * 2);
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
}

// poll
void
FUNCP_MEMORY_SERVER_CLASS::Poll()
{
    // do nothing
}

// request
UMF_MESSAGE
FUNCP_MEMORY_SERVER_CLASS::Request(
    UMF_MESSAGE req)
{
    bool need_response;
    MEM_ADDRESS addr;
    MEM_VALUE   data;
    MEM_VALUE   va;
    MEM_CACHELINE line;
    MEM_CACHELINE_WORD_VALID_MASK wordValid;
    CONTEXT_ID ctx_id;

    UMF_MESSAGE resp;

    ASSERTX(memory != NULL);

    // decode command
    switch (req->GetMethodID())
    {
      case METHOD_ID_Load:
        addr = MEM_ADDRESS(req->ExtractUINT64());
        ctx_id = CONTEXT_ID(req->ExtractUINT(sizeof(ctx_id)));

        // free
        req->Delete();

        memory->Read(ctx_id, addr, sizeof(MEM_VALUE), &data);
        T1("\tfuncp_memory: LD CTX " << UINT64(ctx_id) << " (" << sizeof(MEM_VALUE) << ") [" << fmt_addr(addr) << "] -> " << fmt_data(data));

        // create response message
        resp = UMF_MESSAGE_CLASS::New();
        resp->SetLength(sizeof(MEM_VALUE));
        resp->SetMethodID(METHOD_ID_Load);
        resp->AppendUINT(data, sizeof(MEM_VALUE));

        // return response
        return resp;

        break;

      case METHOD_ID_LoadCacheLine:
        addr = MEM_ADDRESS(req->ExtractUINT64());
        ctx_id = CONTEXT_ID(req->ExtractUINT(sizeof(ctx_id)));

        // free
        req->Delete();

        memory->Read(ctx_id, addr, sizeof(MEM_CACHELINE), &line);
        if (TRACING(1))
        {
            T1("\tfuncp_memory: LDline CTX " << UINT64(ctx_id) << " (" << sizeof(MEM_CACHELINE) << ") [" << fmt_addr(addr) << "] -> line:");
            for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
                MEM_VALUE v = ((MEM_VALUE *)&line) [i];
                T1("\t\t" << fmt_data(v));
            }
        }

        // create response message
        resp = UMF_MESSAGE_CLASS::New();
        resp->SetLength(sizeof(MEM_CACHELINE));
        resp->SetMethodID(METHOD_ID_LoadCacheLine);
        resp->AppendBytes(sizeof(MEM_CACHELINE), (unsigned char *) &line);

        // return response
        return resp;

        break;

      case METHOD_ID_Store:
        // extract data
        data = MEM_VALUE(req->ExtractUINT(sizeof(MEM_VALUE)));
        addr = MEM_ADDRESS(req->ExtractUINT64());
        ctx_id = CONTEXT_ID(req->ExtractUINT(sizeof(ctx_id)));

        T1("\tfuncp_memory: ST CTX " << UINT64(ctx_id) << " (" << sizeof(MEM_VALUE) << ") [" << fmt_addr(addr) << "] <- " << fmt_data(data));
        memory->Write(ctx_id, addr, sizeof(MEM_VALUE), &data);

        // free
        req->Delete();

        // no response
        return NULL;
 
        break;

      case METHOD_ID_StoreCacheLine:
      case METHOD_ID_StoreCacheLine_Sync:
        need_response = (req->GetMethodID() == METHOD_ID_StoreCacheLine_Sync);

        // extract data
        req->ExtractBytes(sizeof(MEM_CACHELINE), (unsigned char *) &line);
        addr = MEM_ADDRESS(req->ExtractUINT64());
        wordValid = MEM_CACHELINE_WORD_VALID_MASK(req->ExtractUINT8());
        ctx_id = CONTEXT_ID(req->ExtractUINT(sizeof(ctx_id)));

        if (TRACING(1))
        {
            MEM_CACHELINE_WORD_VALID_MASK mask_pos = 1;

            T1("\tfuncp_memory: STline CTX " << UINT64(ctx_id) << (need_response ? " SYNC" : "") << " (" << sizeof(MEM_CACHELINE) << ") [" << fmt_addr(addr) << "] -> line:");
            for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
                MEM_VALUE v = ((MEM_VALUE *)&line) [i];
                if ((wordValid & mask_pos) != 0)
                {
                    T1("\t\t" << fmt_data(v));
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
            memory->Write(ctx_id, addr, sizeof(MEM_CACHELINE), &line);
        }
        else
        {
            // Write only the valid words in the line.  Assumes little endian
            // addressing.
            MEM_CACHELINE_WORD_VALID_MASK mask_pos = 1;
            MEM_ADDRESS w_addr = addr;
            for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
                MEM_VALUE v = ((MEM_VALUE *)&line) [i];
                if ((wordValid & mask_pos) != 0)
                {
                    memory->Write(ctx_id, w_addr, sizeof(MEM_VALUE), &v);
                }
                w_addr += sizeof(MEM_VALUE);
                mask_pos <<= 1;
            }
        }

        // free
        req->Delete();

        if (! need_response)
        {
            // no response
            return NULL;
        }
        else
        {
            // create response message
            resp = UMF_MESSAGE_CLASS::New();
            resp->SetLength(4);
            resp->SetMethodID(METHOD_ID_StoreCacheLine_Sync);
            resp->AppendUINT32(0);

            // return response
            return resp;
        }
 
        break;

      default:
        ASIMWARNING("Invalid command\n");
        parent->CallbackExit(1);
        break;
    }

    return NULL;
}


//***********************************************************************
//
// FPGA-side cache management.  When simulated memory is modified or
// read by software (e.g. instruction emulation) these routines flush
// or invalidate the FPGA-side cache.
//
//***********************************************************************

void
FUNCP_MEMORY_SERVER_CLASS::NoteSystemMemoryRead(CONTEXT_ID ctxId, MEM_ADDRESS addr, MEM_ADDRESS size)
{
    instance.SystemMemoryRef(ctxId, addr, size, false);
}

void
FUNCP_MEMORY_SERVER_CLASS::NoteSystemMemoryWrite(CONTEXT_ID ctxId, MEM_ADDRESS addr, MEM_ADDRESS size)
{
//    NoteSystemMemoryWriteUnknownAddr();
    instance.SystemMemoryRef(ctxId, addr, size, true);
}

void
FUNCP_MEMORY_SERVER_CLASS::SystemMemoryRef(CONTEXT_ID ctxId, MEM_ADDRESS addr, UINT64 size, bool isWrite)
{
    T1("\tfuncp_memory: Note system memory " << (isWrite ? "WRITE" : "READ") << " (Addr=" << fmt_addr(addr) << " bytes=" << fmt_addr(size) << ")");

    MEM_ADDRESS endAddr = addr + size + sizeof(MEM_CACHELINE) - 1;

    // Cache-line align the start and end address
    addr &= ~ MEM_ADDRESS(sizeof(MEM_CACHELINE) - 1);
    endAddr &= ~ MEM_ADDRESS(sizeof(MEM_CACHELINE) - 1);

    UINT64 nLines = (endAddr - addr) / sizeof(MEM_CACHELINE);

    //
    // Number of lines per message is stored in 8 bits in FPGA messages.
    // Generated messages to cover the range.
    //
    while (nLines > 0)
    {
        UINT8 tLines = (nLines < 0xff) ? nLines : 0xff;

        if (isWrite)
        {
            // Flush pending stores & invalidate in preparation for writes.
            T1("\tfuncp_memory: INVAL Addr=" << fmt_addr(addr) << " nLines=" << UINT32(tLines));
        }
        else
        {
            // Just flush pending stores for reads.  No need to invalidate.
            T1("\tfuncp_memory: FLUSH Addr=" << fmt_addr(addr) << " nLines=" << UINT32(tLines));
        }

        // create message for RRR client
        UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
        msg->SetLength(10 + sizeof(CONTEXT_ID_RRR));
        msg->SetServiceID(FUNCP_MEMORY_SERVICE_ID);
        msg->SetMethodID(METHOD_ID_Invalidate);
        msg->AppendUINT8(isWrite ? 0 : 0xff);
        msg->AppendUINT8(tLines);
        msg->AppendUINT64(addr);
        msg->AppendCONTEXT_ID_RRR(ctxId);

        UMF_MESSAGE resp = RRRClient->MakeRequest(msg);

        // Response indicates flush is done
        resp->Delete();

        addr += MEM_ADDRESS(tLines) * sizeof(MEM_CACHELINE);
        nLines -= tLines;
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
