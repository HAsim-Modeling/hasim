#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <iostream>

#include "asim/rrr/service_ids.h"
#include "asim/provides/command_switches.h"

#include "asim/provides/stats_controller.h"


#include "asim/dict/STATS.h"

#define SERVICE_ID       STATS_SERVICE_ID

// temporary
#define METHOD_ID_PRINT 0
#define METHOD_ID_FLUSH 1

using namespace std;

// ===== service instantiation =====
STATS_CONTROLLER_CLASS STATS_CONTROLLER_CLASS::instance;

// ===== methods =====

// constructor
STATS_CONTROLLER_CLASS::STATS_CONTROLLER_CLASS()
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
}

// destructor
STATS_CONTROLLER_CLASS::~STATS_CONTROLLER_CLASS()
{
}

// init
void
STATS_CONTROLLER_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;
    
    // Open the output file
    string stats_name = string(globalArgs->Workload()) + ".stats";
    statsFile = fopen(stats_name.c_str(), "w+");
}

// uninit: we have to write this explicitly
void
STATS_CONTROLLER_CLASS::Uninit()
{
    fclose(statsFile);
    // simply chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// request
UMF_MESSAGE
STATS_CONTROLLER_CLASS::Request(
    UMF_MESSAGE request)
{

    // extract event ID, data, and modelCC
    UINT32 methodID  = request->GetMethodID();

    // decode
    if (methodID == METHOD_ID_PRINT)
    {
        UINT32 stat_data = request->ExtractUINT32();
        UINT32 stat_id   = request->ExtractUINT32();

        // lookup event name from dictionary
        const char *stat_msg = STATS_DICT::Str(stat_id);
        if (stat_msg == NULL)
        {
            cerr << "stats: " << STATS_DICT::Str(stat_id)
                 << ": invalid stat_id: " << stat_id << endl << flush;
            CallbackExit(1);
        }
        
        // write to file
        fprintf(statsFile, "%s: %u\n", stat_msg, stat_data);
        
        // free
        delete request;

        // no RRR response
        return NULL;
    }
    else if (methodID == METHOD_ID_FLUSH)
    {
        // flush
        fflush(statsFile);

        // prepare response
        UMF_MESSAGE response = new UMF_MESSAGE_CLASS(4);
        response->SetMethodID(METHOD_ID_FLUSH);
        response->AppendUINT32(0);

        // free
        delete request;

        // send response
        return response;
    }
    else
    {
        // error
        cerr << "stats: invalid methodID\n" << flush;

        // free
        delete request;

        // exit
        CallbackExit(1);
    }
}

// poll
void
STATS_CONTROLLER_CLASS::Poll()
{
}


