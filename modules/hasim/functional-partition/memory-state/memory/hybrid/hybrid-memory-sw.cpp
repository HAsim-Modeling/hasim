/* INTEL CONFIDENTIAL
 * Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
 * copyright license under Intel copyrights to copy and distribute this code 
 * internally only. This code is provided "AS IS" with no support and with no 
 * warranties of any kind, including warranties of MERCHANTABILITY,
 * FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
 * By making any use of this code, Recipient agrees that no other licenses 
 * to any Intel patents, trade secrets, copyrights or other intellectual 
 * property rights are granted herein, and no other licenses shall arise by 
 * estoppel, implication or by operation of law. Recipient accepts all risks 
 * of use.
*/
 
#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "asim/syntax.h"
#include "asim/mesg.h"

#include "hybrid-memory-sw.h"
#include "vmh-utils.h"
#include "asim/provides/rrr.h"
#include "asim/provides/model.h"

#include "asim/rrr/service_ids.h"

#define SERVICE_ID  HYBRID_FUNCP_MEMORY_SERVICE_ID

// DEBUG: temporary link to RRR client
extern RRR_CLIENT globalRRRClient;

// service instantiations
HYBRID_FUNCP_MEMORY_CLASS        HYBRID_FUNCP_MEMORY_CLASS::instance;

// constructor
HYBRID_FUNCP_MEMORY_CLASS::HYBRID_FUNCP_MEMORY_CLASS() :
    memory(NULL)
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
}

// destructor
HYBRID_FUNCP_MEMORY_CLASS::~HYBRID_FUNCP_MEMORY_CLASS()
{
    Cleanup();
}

// init
void
HYBRID_FUNCP_MEMORY_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;

    memory = new FUNCP_SIMULATED_MEMORY_CLASS();
}

// uninit: override
void
HYBRID_FUNCP_MEMORY_CLASS::Uninit()
{
    // cleanup
    Cleanup();
    
    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
HYBRID_FUNCP_MEMORY_CLASS::Cleanup()
{
    delete memory;
}

// poll
void
HYBRID_FUNCP_MEMORY_CLASS::Poll()
{
    // do nothing
}

// request
bool
HYBRID_FUNCP_MEMORY_CLASS::Request(
    UINT32 arg0,
    UINT32 arg1,
    UINT32 arg2,
    UINT32 arg3,
    UINT32 *result)
{
    ASSERTX(memory != NULL);

    UINT64 addr = arg1;

    // decode command
    if (arg0 == CMD_LOAD)
    {
        memory->Read(addr, sizeof(*result), result);
        return true;
    }
    else if (arg0 == CMD_STORE)
    {
        memory->Write(addr, sizeof(arg2), &arg2);
        return false;
    }
    else
    {
        ASIMWARNING("Invalid command\n");
        parent->CallbackExit(1);
    }

    return false;
}
