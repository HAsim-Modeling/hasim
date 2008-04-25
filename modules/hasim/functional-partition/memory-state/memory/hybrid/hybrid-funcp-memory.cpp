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

    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
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

    ASSERTX(memory != NULL);

    // decode command
    switch (req->GetMethodID())
    {
      case CMD_LOAD:
        addr = MEM_ADDRESS(req->ExtractUINT(sizeof(MEM_ADDRESS)));

        // free
        delete req;

        memory->Read(addr, sizeof(MEM_VALUE), &data);
        T1("\tfuncp_memory: LD (" << sizeof(MEM_VALUE) << ") [0x" << fmt_x(addr) << "] -> 0x" << fmt_x(data));

        // create response message
        UMF_MESSAGE resp = new UMF_MESSAGE_CLASS(sizeof(MEM_VALUE));
        resp->SetMethodID(CMD_LOAD);
        resp->AppendUINT(data, sizeof(MEM_VALUE));

        // return response
        return resp;

        break;

      case CMD_STORE:
        // extract data
        data = MEM_VALUE(req->ExtractUINT(sizeof(MEM_VALUE)));
        addr = MEM_ADDRESS(req->ExtractUINT(sizeof(MEM_ADDRESS)));

        memory->Write(addr, sizeof(MEM_VALUE), &data);
        T1("\tfuncp_memory: ST (" << sizeof(MEM_VALUE) << ") [0x" << fmt_x(addr) << "] <- 0x" << fmt_x(data));

        // free
        delete req;

        // no response
        return NULL;
 
        break;

      default:
        ASIMWARNING("Invalid command\n");
        parent->CallbackExit(1);
        break;
    }

    return NULL;
}
