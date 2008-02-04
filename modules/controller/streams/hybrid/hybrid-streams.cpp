#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <iostream>

#include "asim/provides/streams.h"
#include "asim/rrr/rrr_service_ids.h"

#define SERVICE_ID  STREAMS_SERVICE_ID

using namespace std;

// ===== service instantiation =====
STREAMS_CLASS STREAMS_CLASS::instance;

// ===== dictionary =====

// messages
const int MSG_TYPES = 7;
char MSG_TABLE[MSG_TYPES][256] =
{
    "[%12d]: controller: program started.\n",
    "[%12d]: controller: test program finished succesfully.\n",
    "[%12d]: controller: test program finished, one or more failures occurred.\n",
    "[%12d]: controller: ERROR: unexpected timing partition response: ",
    "0x%x\n",
    "[%12d]: controller: model cycles completed: ",
    "%9u\n"
};

// events
const int EVENT_TYPES = 6;
char EVENT_TABLE[EVENT_TYPES][256] =
{
    "[%9d]: 1 FET: ",
    "[%9d]: 2   DEC: ",
    "[%9d]: 3     EXE: ",
    "[%9d]: 4       MEM: ",
    "[%9d]: 5         WBK: ",
    "%d\n"
};

// stats
const int STAT_TYPES = 6;
char STAT_TABLE[STAT_TYPES][256] =
{
    "instructions committed = %u\n",
    "dcache misses          = %u\n",
    "branch mispredicts     = %u\n",
    "icache misses          = %u\n",
    "instructions fetched   = %u\n",
    "total cycles           = %u\n"
};

// asserts
const int ASSERT_TYPES = 2;
char ASSERT_TABLE[ASSERT_TYPES][256] =
{
    "FUNCP: Ran out of Tokens!\n",
    "FUNCP: Ran out of physical registers!\n"
};

// ===== methods =====

// constructor
STREAMS_CLASS::STREAMS_CLASS()
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
}

// destructor
STREAMS_CLASS::~STREAMS_CLASS()
{
}

// init
void
STREAMS_CLASS::Init(
    HASIM_MODULE     p)
{
    // set parent pointer
    parent = p;

    // open events and stats files
    eventfile = fopen("software_events.out", "w+");
    statfile = fopen("software_stats.out", "w+");
}

// uninit
void
STREAMS_CLASS::Uninit()
{
    // close stat and event files
    fclose(eventfile);
    fclose(statfile);
}

// request
bool
STREAMS_CLASS::Request(
    UINT32 arg0,
    UINT32 arg1,
    UINT32 arg2,
    UINT32 *result)
{
    bool retval = false;

    // decode request
    switch(arg0)
    {
        case 0: // print message
            PrintMessage(arg1, arg2);
            retval = false;
            break;

        case 1: // print event
            PrintEvent(arg1, arg2);
            retval = false;
            break;

        case 2: // print stat
            PrintStat(arg1, arg2);
            retval = false;
            break;

        case 3: // print assert
            PrintAssert(arg1, arg2);
            retval = false;
            break;

        default:
            cerr << "console: invalid request" << endl;
            CallbackExit(1);
            break;
    }

    return retval;
}

// poll
void
STREAMS_CLASS::Poll()
{
}

// ===== internal print methods =====

// print message
void
STREAMS_CLASS::PrintMessage(
    UINT32 msgclass,
    UINT32 payload)
{
    if (msgclass >= MSG_TYPES)
    {
        cerr << "console: invalid message class: "
             << msgclass << endl;
        CallbackExit(1);
    }

    char *fmtstr = MSG_TABLE[msgclass];
    printf(fmtstr, payload);
    fflush(stdout);
}

// print event
void
STREAMS_CLASS::PrintEvent(
    UINT32 eventtype,
    UINT32 payload)
{
    if (eventtype >= EVENT_TYPES)
    {
        cerr << "console: invalid event type: "
             << eventtype << endl;
        CallbackExit(1);
    }

    char *fmtstr = EVENT_TABLE[eventtype];
    fprintf(eventfile, fmtstr, payload);
}

// print stat
void
STREAMS_CLASS::PrintStat(
    UINT32 stattype,
    UINT32 value)
{
    if (stattype >= STAT_TYPES)
    {
        cerr << "console: invalid stat type: "
             << stattype << endl;
        CallbackExit(1);
    }

    char *fmtstr = STAT_TABLE[stattype];
    fprintf(statfile, fmtstr, value);
}

// print assert
void
STREAMS_CLASS::PrintAssert(
    UINT32 asserttype,
    UINT32 severity)
{
    if (asserttype >= ASSERT_TYPES)
    {
        cerr << "console: invalid assert type: "
             << asserttype << endl;
        CallbackExit(1);
    }

    char *fmtstr = ASSERT_TABLE[asserttype];
    printf(fmtstr);

    if (severity > 1)
    {
        CallbackExit(1);
    }
}
