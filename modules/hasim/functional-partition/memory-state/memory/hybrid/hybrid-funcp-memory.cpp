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

#define SERVICE_ID  FUNCP_MEMORY_SERVICE_ID

#define CMD_LOAD                 0
#define CMD_LOAD_CACHELINE       1
#define CMD_STORE                2
#define CMD_STORE_CACHELINE      3
#define CMD_STORE_CACHELINE_SYNC 4
#define CMD_VTOP                 5

#define METHOD_ID_INVALIDATE     0
#define METHOD_ID_INVALIDATE_ALL 1

// DEBUG: temporary link to RRR client
extern RRR_CLIENT globalRRRClient;

// service instantiation
FUNCP_MEMORY_CLASS FUNCP_MEMORY_CLASS::instance;

// constructor
FUNCP_MEMORY_CLASS::FUNCP_MEMORY_CLASS() :
    memory(NULL)
{
    SetTraceableName("funcp_memory");

    ASSERT(MEMORY_STORE_INFO_SIZE == 64 + FUNCP_ISA_INT_REG_SIZE,
           "Awb parameters are inconsistent: MEMORY_STORE_INFO_SIZE != 64 + FUNCP_ISA_INT_REG_SIZE");

    ASSERT(MEMORY_STORE_CACHELINE_INFO_SIZE == 64 + FUNCP_CACHELINE_BITS,
           "Awb parameters are inconsistent: MEMORY_STORE_CACHELINE_INFO_SIZE != 64 + FUNCP_ISA_INT_REG_SIZE");

    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);

    char fmt[16];

    sprintf(fmt, "0%dx", sizeof(MEM_ADDRESS) * 2);
    fmt_addr = Format("0x", fmt);

    sprintf(fmt, "0%dx", sizeof(MEM_VALUE) * 2);
    fmt_data = Format("0x", fmt);
}

// destructor
FUNCP_MEMORY_CLASS::~FUNCP_MEMORY_CLASS()
{
    Cleanup();
}

// init
void
FUNCP_MEMORY_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;

    memory = new FUNCP_SIMULATED_MEMORY_CLASS();
}

// uninit: override
void
FUNCP_MEMORY_CLASS::Uninit()
{
    // cleanup
    Cleanup();
    
    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
FUNCP_MEMORY_CLASS::Cleanup()
{
    delete memory;
}

// poll
void
FUNCP_MEMORY_CLASS::Poll()
{
    // do nothing
}

// request
UMF_MESSAGE
FUNCP_MEMORY_CLASS::Request(
    UMF_MESSAGE req)
{
    bool need_response;
    MEM_ADDRESS addr;
    MEM_VALUE   data;
    MEM_VALUE   va;
    MEM_CACHELINE line;

    UMF_MESSAGE resp;

    ASSERTX(memory != NULL);

    // decode command
    switch (req->GetMethodID())
    {
      case CMD_LOAD:
        addr = MEM_ADDRESS(req->ExtractUINT64());

        // free
        req->Delete();

        memory->Read(addr, sizeof(MEM_VALUE), &data);
        T1("\tfuncp_memory: LD (" << sizeof(MEM_VALUE) << ") [" << fmt_addr(addr) << "] -> " << fmt_data(data));

        // create response message
        resp = UMF_MESSAGE_CLASS::New();
        resp->SetLength(sizeof(MEM_VALUE));
        resp->SetMethodID(CMD_LOAD);
        resp->AppendUINT(data, sizeof(MEM_VALUE));

        // return response
        return resp;

        break;

      case CMD_LOAD_CACHELINE:
        addr = MEM_ADDRESS(req->ExtractUINT64());

        // free
        req->Delete();

        memory->Read(addr, sizeof(MEM_CACHELINE), &line);
        if (TRACING(1))
        {
            T1("\tfuncp_memory: LDline (" << sizeof(MEM_CACHELINE) << ") [" << fmt_addr(addr) << "] -> line:");
            for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
                MEM_VALUE v = ((MEM_VALUE *)&line) [i];
                T1("\t\t" << fmt_data(v));
            }
        }

        // create response message
        resp = UMF_MESSAGE_CLASS::New();
        resp->SetLength(sizeof(MEM_CACHELINE));
        resp->SetMethodID(CMD_LOAD_CACHELINE);
        resp->AppendBytes(sizeof(MEM_CACHELINE), (unsigned char *) &line);

        // return response
        return resp;

        break;

      case CMD_STORE:
        // extract data
        data = MEM_VALUE(req->ExtractUINT(sizeof(MEM_VALUE)));
        addr = MEM_ADDRESS(req->ExtractUINT64());

        memory->Write(addr, sizeof(MEM_VALUE), &data);
        T1("\tfuncp_memory: ST (" << sizeof(MEM_VALUE) << ") [" << fmt_addr(addr) << "] <- " << fmt_data(data));

        // free
        req->Delete();

        // no response
        return NULL;
 
        break;

      case CMD_STORE_CACHELINE:
      case CMD_STORE_CACHELINE_SYNC:
        need_response = (req->GetMethodID() == CMD_STORE_CACHELINE_SYNC);

        // extract data
        req->ExtractBytes(sizeof(MEM_CACHELINE), (unsigned char *) &line);
        addr = MEM_ADDRESS(req->ExtractUINT64());

        if (TRACING(1))
        {
            T1("\tfuncp_memory: STline" << (need_response ? " SYNC " : "") << " (" << sizeof(MEM_CACHELINE) << ") [" << fmt_addr(addr) << "] -> line:");
            for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
                MEM_VALUE v = ((MEM_VALUE *)&line) [i];
                T1("\t\t" << fmt_data(v));
            }
        }

        memory->Write(addr, sizeof(MEM_CACHELINE), &line);

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
            resp->SetMethodID(CMD_STORE_CACHELINE_SYNC);
            resp->AppendUINT32(0);

            // return response
            return resp;
        }
 
        break;

      case CMD_VTOP: {
        // extract data.  VA comes in as a register sized MEM_VALUE.
        va = MEM_VALUE(req->ExtractUINT(sizeof(MEM_VALUE)));
        req->Delete();

        MEM_ADDRESS pa = memory->VtoP(va);

        T1("\tfuncp_memory: VtoP VA " << fmt_data(va) << " -> PA " << fmt_addr(pa));

        // create response message
        resp = UMF_MESSAGE_CLASS::New();
        resp->SetLength(8);
        resp->SetMethodID(CMD_VTOP);
        resp->AppendUINT64(pa);

        // return response
        return resp;
      }
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
FUNCP_MEMORY_CLASS::NoteSystemMemoryWriteUnknownAddr()
{
    instance.InvalidateAllCaches();
}

void
FUNCP_MEMORY_CLASS::InvalidateAllCaches()
{
    T1("\tfuncp_memory: INVAL ALL");

    // create message for RRR client
    UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
    msg->SetLength(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_INVALIDATE_ALL);
    msg->AppendUINT32(0);

    UMF_MESSAGE resp = RRRClient->MakeRequest(msg);

    // Response indicates flush is done
    resp->Delete();
}


void
FUNCP_MEMORY_CLASS::NoteSystemMemoryRead(MEM_ADDRESS addr, MEM_ADDRESS size)
{
    instance.SystemMemoryRef(addr, size, false);
}

void
FUNCP_MEMORY_CLASS::NoteSystemMemoryWrite(MEM_ADDRESS addr, MEM_ADDRESS size)
{
//    NoteSystemMemoryWriteUnknownAddr();
    instance.SystemMemoryRef(addr, size, true);
}

void
FUNCP_MEMORY_CLASS::SystemMemoryRef(MEM_ADDRESS addr, UINT64 size, bool isWrite)
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
        UINT8 tLines = (nLines < 0xffff) ? nLines : 0xffff;

        // create message for RRR client
        UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
        msg->SetLength(12);
        msg->SetServiceID(SERVICE_ID);
        msg->SetMethodID(METHOD_ID_INVALIDATE);
        msg->AppendUINT64(addr);

        // Number of lines is encoded as a byte, but passed as 32 bits to keep
        // RRR happy.  Whether only flushing is required (without invalidating)
        // is encoded in bit 8.
        UINT32 lines_and_flush_bit = tLines;

        if (isWrite)
        {
            // Flush pending stores & invalidate in preparation for writes.
            T1("\tfuncp_memory: INVAL Addr=" << fmt_addr(addr) << " nLines=" << UINT32(tLines));
        }
        else
        {
            // Just flush pending stores for reads.  No need to invalidate.
            lines_and_flush_bit |= 0x100;       // Only flush -- no invalidate
            T1("\tfuncp_memory: FLUSH Addr=" << fmt_addr(addr) << " nLines=" << UINT32(tLines));
        }

        // Pass 8 bit tLines as 32 bits to keep channel I/O happy
        msg->AppendUINT32(lines_and_flush_bit);

        UMF_MESSAGE resp = RRRClient->MakeRequest(msg);

        // Response indicates flush is done
        resp->Delete();

        addr += MEM_ADDRESS(tLines) * sizeof(MEM_CACHELINE);
        nLines -= tLines;
    }
}
