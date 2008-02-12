#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "control.h"

using namespace std;

// constructor
CONTROLLER_CLASS::CONTROLLER_CLASS() :
    channelio(this),                    // initializes hardware
    rrrServer(this, &channelio),
    rrrClient(this, &channelio)
{
    RRRClient = &rrrClient;

    // setup link to starter client
    starter = STARTER_CLASS::GetInstance();
}

// destructor
CONTROLLER_CLASS::~CONTROLLER_CLASS()
{
    Uninit();
}

// uninit
void
CONTROLLER_CLASS::Uninit()
{
    channelio.Uninit();
    rrrServer.Uninit();
}

// controller's main()
int
CONTROLLER_CLASS::Main()
{
    // send "start" signal to the hardware partition.
    starter->StartHardware();

    // go into the main scheduler loop
    SchedulerLoop();

    // end of simulation... cleanup and exit
    starter->StopHardware();

    return 0;
}

// scheduler loop
void
CONTROLLER_CLASS::SchedulerLoop()
{
    while (true)
    {
        // poll submodules
        channelio.Poll();
        rrrServer.Poll();
    }
}

// callback-exit
void
CONTROLLER_CLASS::CallbackExit(
    int exitcode)
{
    // chain-uninit, then exit
    Uninit();
    exit(exitcode);
}

