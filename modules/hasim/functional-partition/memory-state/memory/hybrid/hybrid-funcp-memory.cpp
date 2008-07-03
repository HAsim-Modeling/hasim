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

// DEBUG: temporary link to RRR client
extern RRR_CLIENT globalRRRClient;

// service instantiation
FUNCP_MEMORY_CLASS FUNCP_MEMORY_CLASS::instance;

// constructor
FUNCP_MEMORY_CLASS::FUNCP_MEMORY_CLASS() :
    memory(NULL)
{
    SetTraceableName("funcp_memory");

    ASSERT(MEMORY_STORE_INFO_SIZE == FUNCP_ISA_ADDR_SIZE + FUNCP_ISA_INT_REG_SIZE,
           "Awb parameters are inconsistent: MEMORY_STORE_INFO_SIZE != FUNCP_ISA_ADDR_SIZE + FUNCP_ISA_INT_REG_SIZE");

    ASSERT(MEMORY_STORE_CACHELINE_INFO_SIZE == FUNCP_ISA_ADDR_SIZE + FUNCP_CACHELINE_BITS,
           "Awb parameters are inconsistent: MEMORY_STORE_INFO_SIZE != FUNCP_ISA_ADDR_SIZE + FUNCP_ISA_INT_REG_SIZE");

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
        addr = MEM_ADDRESS(req->ExtractUINT(sizeof(MEM_ADDRESS)));

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
        addr = MEM_ADDRESS(req->ExtractUINT(sizeof(MEM_ADDRESS)));

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
        addr = MEM_ADDRESS(req->ExtractUINT(sizeof(MEM_ADDRESS)));

        memory->Write(addr, sizeof(MEM_VALUE), &data);
        T1("\tfuncp_memory: ST (" << sizeof(MEM_VALUE) << ") [" << fmt_addr(addr) << "] <- " << fmt_data(data));

        // free
        req->Delete();

        // no response
        return NULL;
 
        break;

      case CMD_STORE_CACHELINE:
        // extract data
        req->ExtractBytes(sizeof(MEM_CACHELINE), (unsigned char *) &line);
        addr = MEM_ADDRESS(req->ExtractUINT(sizeof(MEM_ADDRESS)));

        memory->Write(addr, sizeof(MEM_CACHELINE), &line);
        if (TRACING(1))
        {
            T1("\tfuncp_memory: STline (" << sizeof(MEM_CACHELINE) << ") [" << fmt_addr(addr) << "] -> line:");
            for (int i = 0; i < sizeof(MEM_CACHELINE) / sizeof(MEM_VALUE); i++) {
                MEM_VALUE v = ((MEM_VALUE *)&line) [i];
                T1("\t\t" << fmt_data(v));
            }
        }

        // free
        req->Delete();

        // no response
        return NULL;
 
        break;

      case CMD_VTOP:
        // extract data.  VA comes in as a register sized MEM_VALUE.
        va = MEM_VALUE(req->ExtractUINT(sizeof(MEM_VALUE)));
        req->Delete();

        MEM_ADDRESS pa = memory->VtoP(va);

        T1("\tfuncp_memory: VtoP VA " << fmt_data(va) << " -> PA " << fmt_addr(pa));

        // create response message
        resp = UMF_MESSAGE_CLASS::New();
        resp->SetLength(sizeof(MEM_ADDRESS));
        resp->SetMethodID(CMD_VTOP);
        resp->AppendBytes(sizeof(MEM_ADDRESS), (unsigned char *) &pa);

        // return response
        return resp;

      default:
        ASIMWARNING("Invalid command\n");
        parent->CallbackExit(1);
        break;
    }

    return NULL;
}
