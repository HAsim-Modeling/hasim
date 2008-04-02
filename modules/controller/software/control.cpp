#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "asim/provides/hasim_software_controller.h"
#include "asim/provides/streams.h"

using namespace std;

// constructor
CONTROLLER_CLASS::CONTROLLER_CLASS(
    LLPI l) :
        HASIM_MODULE_CLASS(NULL)
{
    // setup link to LLPI
    llpi = l;

    // setup link to starter client
    starter = STARTER_CLASS::GetInstance();

    // ================= Streams Registration =============== //
    //                   --------------------                 //
    // This code is sharead by the hybrid standard controller //
    // (which needs to register with streams using known      //
    // streamIDs) and the hybrid null controller (which does  //
    // not know any streamIDs. We use an AWB param to         //
    // distinguish the two behaviors.                         //
    // ====================================================== //

#if (REGISTER_STREAMS==1)
    // default streams behavior is to route messages to stdout
    STREAMS streams = STREAMS_CLASS::GetInstance();

    // map events
    eventfile = fopen("software_events.out", "w+");
    streams->MapStream(STREAMID_EVENT, eventfile);

    // map stats
    statfile = fopen("software_stats.out", "w+");
    streams->MapStream(STREAMID_STAT, statfile);

    // map assertions
    streams->RegisterCallback(STREAMID_ASSERT, this);
#endif
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
    HASIM_MODULE_CLASS::Uninit();
}

// cleanup
void
CONTROLLER_CLASS::Cleanup()
{
#if (REGISTER_STREAMS == 1)
    // close open files
    fclose(eventfile);
    fclose(statfile);
#endif
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
