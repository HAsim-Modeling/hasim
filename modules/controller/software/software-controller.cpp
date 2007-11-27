#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "software-controller.h"

using namespace std;

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

// constructor
SOFTWARE_CONTROLLER_CLASS::SOFTWARE_CONTROLLER_CLASS()
{
    // open events and stats files
    eventfile = fopen("software_events.out", "w+");
    statfile = fopen("software_stats.out", "w+");
}

// destructor
SOFTWARE_CONTROLLER_CLASS::~SOFTWARE_CONTROLLER_CLASS()
{
    Uninit();

    // close stat and event files
    fclose(eventfile);
    fclose(statfile);
}

// uninit
void
SOFTWARE_CONTROLLER_CLASS::Uninit()
{
    // destroy submodules
    if (rrrServer)
    {
        delete rrrServer;
        rrrServer = NULL;
    }

    if (channelio)
    {
        delete channelio;
        channelio = NULL;
    }
}

// scheduler loop
void
SOFTWARE_CONTROLLER_CLASS::SchedulerLoop()
{
    while (true)
    {
        // poll submodules
        channelio->Poll();
        rrrServer->Poll();
    }
}

// print message
void
SOFTWARE_CONTROLLER_CLASS::PrintMessage(
    UINT32 msgclass,
    UINT32 payload)
{
    if (msgclass >= MSG_TYPES)
    {
        cerr << "software controller: invalid message class: "
             << msgclass << endl;
        CallbackExit(1);
    }

    char *fmtstr = MSG_TABLE[msgclass];
    printf(fmtstr, payload);
    fflush(stdout);
}

// print event
void
SOFTWARE_CONTROLLER_CLASS::PrintEvent(
    UINT32 eventtype,
    UINT32 payload)
{
    if (eventtype >= EVENT_TYPES)
    {
        cerr << "software controller: invalid event type: "
             << eventtype << endl;
        CallbackExit(1);
    }

    char *fmtstr = EVENT_TABLE[eventtype];
    fprintf(eventfile, fmtstr, payload);
}

// print stat
void
SOFTWARE_CONTROLLER_CLASS::PrintStat(
    UINT32 stattype,
    UINT32 value)
{
    if (stattype >= STAT_TYPES)
    {
        cerr << "software controller: invalid stat type: "
             << stattype << endl;
        CallbackExit(1);
    }

    char *fmtstr = STAT_TABLE[stattype];
    fprintf(statfile, fmtstr, value);
}

// print assert
void
SOFTWARE_CONTROLLER_CLASS::PrintAssert(
    UINT32 asserttype,
    UINT32 severity)
{
    if (asserttype >= ASSERT_TYPES)
    {
        cerr << "software controller: invalid assert type: "
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

// callback-exit
void
SOFTWARE_CONTROLLER_CLASS::CallbackExit(
    int exitcode)
{
    // chain-uninit, then exit
    Uninit();
    exit(exitcode);
}
