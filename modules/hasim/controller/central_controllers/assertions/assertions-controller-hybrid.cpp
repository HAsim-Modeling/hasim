#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <iostream>

#include "asim/provides/assertions_controller.h"
#include "asim/rrr/service_ids.h"

#include "asim/dict/ASSERTIONS.h"

#define SERVICE_ID       ASSERTIONS_SERVICE_ID

using namespace std;

// ===== service instantiation =====
ASSERTIONS_CONTROLLER_CLASS ASSERTIONS_CONTROLLER_CLASS::instance;

// ===== methods =====

// constructor
ASSERTIONS_CONTROLLER_CLASS::ASSERTIONS_CONTROLLER_CLASS()
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
}

// destructor
ASSERTIONS_CONTROLLER_CLASS::~ASSERTIONS_CONTROLLER_CLASS()
{
}

// init
void
ASSERTIONS_CONTROLLER_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;
    
    // Open the output file
    assertionsFile = fopen("hasim_assertions.out", "w+");
}

// uninit: we have to write this explicitly
void
ASSERTIONS_CONTROLLER_CLASS::Uninit()
{
    fclose(assertionsFile);

    // simply chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// request
bool
ASSERTIONS_CONTROLLER_CLASS::Request(
    UINT32 arg0,
    UINT32 arg1,
    UINT32 arg2,
    UINT32 arg3,
    UINT32 *result)
{

    // TEMPORARY:: Commented out until we migrate assertions.

    // extract event ID, data, and modelCC
    UINT32 unused = arg0; // Reserved to be methodID later if needed.
    UINT32 assert_id = arg1;
    UINT32 fpga_cc = arg2;
    UINT32 severity = arg3;

    // lookup event name from dictionary
    const char *assert_msg = ASSERTIONS_DICT::Str(assert_id);
    if (assert_msg == NULL)
    {
        cerr << "streams: " << ASSERTIONS_DICT::Str(assert_id)
             << ": invalid assert_id: " << assert_id << endl;
        CallbackExit(1);
    }

    // write to file
    fprintf(assertionsFile, "[%010u]: %s\n", fpga_cc, assert_msg);
    fflush(assertionsFile);
    
    // if severity is great, end the simulation.
    if (severity > 1)
    {
        cerr << "ERROR: Fatal HAsim assertion failure.\n";
        cerr << "MESSAGE: " << assert_msg << "\n";
	CallbackExit(1);
    }

    // no RRR response
    return false;
}

// poll
void
ASSERTIONS_CONTROLLER_CLASS::Poll()
{
}


