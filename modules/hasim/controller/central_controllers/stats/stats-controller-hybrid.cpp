#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <iostream>

#include "asim/rrr/rrr_service_ids.h"
#include "asim/provides/stats_controller.h"

// TEMPORARY:: Commented out until we migrate stats.
// include "asim/dict/STATS.h"

#define SERVICE_ID       STATS_SERVICE_ID

using namespace std;

// ===== service instantiation =====
STATS_CONTROLLER_CLASS STATS_CONTROLLER_CLASS::instance;

// ===== methods =====

// constructor
STATS_CONTROLLER_CLASS::STATS_CONTROLLER_CLASS()
{
    // register with server's map table
    // TEMPORARY:: Commented out until we migrate stats.
    // RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
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

    // TEMPORARY:: Commented out until we migrate stats
/*
    // extract event ID, data, and modelCC
    UINT32 unused = arg0; // Reserved to be methodID later if needed.
    UINT32 stat_id = arg1;
    UINT32 fpga_cc = arg2;
    UINT32 severity = arg3;

    // lookup event name from dictionary
    const char *assert_msg = STATS_DICT::Str(assert_id);
    if (assert_msg == NULL)
    {
        cerr << "streams: " << STATS_DICT::Str(assert_id)
             << ": invalid assert_id: " << event_id << endl;
        CallbackExit(1);
    }

    // write to file
    fprintf(assertFile, "[%010u]: %s\n", fpga_cc, assert_msg);
    
    // if severity is great, end the simulation.
    if (severity > 1)
    {
        printf(STDERR, "ERROR: Fatal HAsim assertion failure.\n");
        printf(STDERR, "MESSAGE: %s\n", assert_msg);
	CallbackExit(1);
    }
*/
    // no RRR response
    return false;
}

// poll
void
STATS_CONTROLLER_CLASS::Poll()
{
}


