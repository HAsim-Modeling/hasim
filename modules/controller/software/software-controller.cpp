#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "software-controller.h"

using namespace std;

// messages
const int MSG_TYPES = 5;
char MSG_TABLE[MSG_TYPES][256] =
{
    "[%0d]: controller: program started.\n",
    "[%0d]: controller: test program finished succesfully.\n",
    "[%0d]: controller: test program finished, one or more failures occurred.\n",
    "[%0d]: controller: ERROR: unexpected timing partition response: ",
    "0x%x\n"
};

// events
const int EVENT_TYPES = 6;
char EVENT_TABLE[EVENT_TYPES][256] =
{
    "[%0d]: 1 FET: ",
    "[%0d]: 2   DEC: ",
    "[%0d]: 3     EXE: ",
    "[%0d]: 4       MEM: ",
    "[%0d]: 5         WBK: ",
    "%d\n"
};

// stats
const int STAT_TYPES = 100;
char STAT_TABLE[STAT_TYPES][256];

// constructor
SOFTWARE_CONTROLLER_CLASS::SOFTWARE_CONTROLLER_CLASS()
{
    // fill in stat table with artifical stat names for now
    for (int i = 0; i < STAT_TYPES; i++)
    {
        sprintf(STAT_TABLE[i], "STAT %3d: \%d\n", i);
    }

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

// callback-exit
void
SOFTWARE_CONTROLLER_CLASS::CallbackExit(
    int exitcode)
{
    // chain-uninit, then exit
    Uninit();
    exit(exitcode);
}
