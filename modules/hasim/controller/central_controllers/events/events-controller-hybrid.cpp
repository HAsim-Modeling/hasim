#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <iostream>

#include "asim/provides/events_controller.h"
#include "asim/rrr/service_ids.h"

#include "asim/dict/EVENTS.h"

#define SERVICE_ID       EVENTS_SERVICE_ID

using namespace std;

// ===== service instantiation =====
EVENTS_CONTROLLER_CLASS EVENTS_CONTROLLER_CLASS::instance;

// ===== methods =====

// constructor
EVENTS_CONTROLLER_CLASS::EVENTS_CONTROLLER_CLASS()
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
}

// destructor
EVENTS_CONTROLLER_CLASS::~EVENTS_CONTROLLER_CLASS()
{
}

// init
void
EVENTS_CONTROLLER_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;
    
    // Open the output file
    eventFile = fopen("hasim_events.out", "w+");
}

// uninit: we have to write this explicitly
void
EVENTS_CONTROLLER_CLASS::Uninit()
{
    // simply chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// request
bool
EVENTS_CONTROLLER_CLASS::Request(
    UINT32 arg0,
    UINT32 arg1,
    UINT32 arg2,
    UINT32 arg3,
    UINT32 *result)
{
    // extract event ID, data, and modelCC
    UINT32 unused = arg0; // Reserved to be methodID later if needed.
    UINT32 event_id = arg1;
    UINT32 event_data = arg2;
    UINT32 model_cc = arg3;

    // lookup event name from dictionary
    const char *event_name = EVENTS_DICT::Str(event_id);
    if (event_name == NULL)
    {
        cerr << "streams: " << EVENTS_DICT::Str(event_id)
             << ": invalid event_id: " << event_id << endl;
        CallbackExit(1);
    }

    // write to file
    // eventually this will be replaced with calls to DRAL.
    fprintf(eventFile, "[%010u]: %s: %u\n", model_cc, event_name, event_data);

    // no RRR response
    return false;
}

// poll
void
EVENTS_CONTROLLER_CLASS::Poll()
{
}

