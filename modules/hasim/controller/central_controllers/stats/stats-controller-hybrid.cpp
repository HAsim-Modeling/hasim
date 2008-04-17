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
#include "asim/provides/stats_controller.h"

#include "asim/dict/STATS.h"

#define SERVICE_ID       STATS_SERVICE_ID

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
    statsFile = fopen("hasim_stats.out", "w+");
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
bool
STATS_CONTROLLER_CLASS::Request(
    UINT32 arg0,
    UINT32 arg1,
    UINT32 arg2,
    UINT32 arg3,
    UINT32 *result)
{

    // extract event ID, data, and modelCC
    UINT32 unused = arg0; // Reserved to be methodID later if needed.
    UINT32 stat_id = arg1;
    UINT32 stat_data = arg2;
    UINT32 unused2 = arg3;

    // lookup event name from dictionary
    const char *stat_msg = STATS_DICT::Str(stat_id);
    if (stat_msg == NULL)
    {
        cerr << "streams: " << STATS_DICT::Str(stat_id)
             << ": invalid stat_id: " << stat_id << endl;
        CallbackExit(1);
    }

    // write to file
    fprintf(statsFile, "%s: %u\n", stat_msg, stat_data);

    // no RRR response
    return false;
}

// poll
void
STATS_CONTROLLER_CLASS::Poll()
{
}


