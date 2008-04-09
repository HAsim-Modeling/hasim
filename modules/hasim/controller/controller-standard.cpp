#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "controller-standard.h"
#include "asim/provides/central_controllers.h"

using namespace std;

// constructor
CONTROLLER_CLASS::CONTROLLER_CLASS(
    LLPI l) :
        PLATFORMS_MODULE_CLASS(NULL)
{
    // setup link to LLPI
    llpi = l;

    // setup link to starter client
    starter = STARTER_CLASS::GetInstance();

    // setup central controllers
    centralControllers = CENTRAL_CONTROLLERS_CLASS::GetInstance();

    // default streams behavior is to route messages to stdout
    STREAMS streams = STREAMS_CLASS::GetInstance();

    // map stats
    statfile = fopen("software_stats.out", "w+");
    streams->MapStream(STREAMID_STAT, statfile);

    // map assertions
    streams->RegisterCallback(STREAMID_ASSERT, this);
}

// destructor
CONTROLLER_CLASS::~CONTROLLER_CLASS()
{
    Cleanup();
}

// uninit: override
void
CONTROLLER_CLASS::Uninit()
{
    // cleanup
    Cleanup();

    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
CONTROLLER_CLASS::Cleanup()
{
    // close open files
    fclose(statfile);
}

// controller's main()
int
CONTROLLER_CLASS::Main()
{
    // send "start" signal to the hardware partition.
    starter->Run();

    // go into the main scheduler loop
    SchedulerLoop();

    // we should never reach here
    starter->Pause();
    starter->Sync();

    return 0;
}

// scheduler loop
void
CONTROLLER_CLASS::SchedulerLoop()
{
    while (true)
    {
        // FIXME: directly poll LLPI
        llpi->Poll();
    }
}

// callback from streams
void
CONTROLLER_CLASS::StreamsCallback(
    UINT32 stringID,
    UINT32 payload0,
    UINT32 payload1)
{
    // payload0 = severity
    if (payload0 > 1)
    {
        CallbackExit(1);
    }
}
