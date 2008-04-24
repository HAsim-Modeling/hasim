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
FUNCP_MEMORY_CLASS::FUNCP_MEMORY_CLASS()
{
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

    // allocate and zero out memory
    M = new MEM_VALUE[MEM_SIZE];
    bzero(M, MEM_SIZE * sizeof(MEM_VALUE));

    // don't load memory image now; load it
    // only when we actually receive a request
    vmhLoaded = false;
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
    if (M)
    {
        delete [] M;
        M = NULL;
    }
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

    // check to see if our image is ready
    if (vmhLoaded == false)
    {
        char *benchmark = "program.vmh";
        if (globalArgs->FuncPlatformArgc() > 1)
        {
            benchmark = globalArgs->FuncPlatformArgv()[1];
        }
        if (vmh_load_image(benchmark, M, MEM_SIZE) == -1)
        {
            exit(1);
        }
        vmhLoaded = true;
    }

    // decode command
    switch (req->GetMethodID())
    {
        case CMD_LOAD:

            // only word-aligned accesses are allowed in our current implementation
            addr = MEM_ADDRESS(req->ExtractUINT(sizeof(MEM_ADDRESS))) >> 2;

            if (addr >= MEM_SIZE)
            {
                fprintf(stderr, "memory: load address out of bounds: 0x%8x\n", addr);
                parent->CallbackExit(1);
            }

            // free
            delete req;

            // create response message
            UMF_MESSAGE resp = new UMF_MESSAGE_CLASS(sizeof(MEM_VALUE));
            resp->SetMethodID(CMD_LOAD);
            resp->AppendUINT(M[addr], sizeof(MEM_VALUE));

            // return response
            return resp;

            break;

        case CMD_STORE:

            // extract data
            data = MEM_VALUE(req->ExtractUINT(sizeof(MEM_VALUE)));

            // only word-aligned accesses are allowed in our current implementation
            addr = MEM_ADDRESS(req->ExtractUINT(sizeof(MEM_ADDRESS))) >> 2;
            if (addr >= MEM_SIZE)
            {
                fprintf(stderr, "memory: load address out of bounds: 0x%8x\n", addr);
                parent->CallbackExit(1);
            }

            // do the store
            M[addr] = data;

            // free
            delete req;

            // no response
            return NULL;
 
            break;

        default:

            fprintf(stderr, "memory: invalid command\n");
            parent->CallbackExit(1);
            break;
    }

    return NULL;
}
