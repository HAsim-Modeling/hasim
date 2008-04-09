#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "controller-null.h"

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

