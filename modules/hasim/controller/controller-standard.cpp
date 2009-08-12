#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "controller-standard.h"
#include "asim/provides/starter.h"
#include "asim/provides/command_switches.h"
#include "asim/provides/software_system.h"

using namespace std;

// globally-visible threadID of system thread
pthread_t monitorThreadID;

// constructor
CONTROLLER_CLASS::CONTROLLER_CLASS(
    LLPI   l,
    SYSTEM s) :
        PLATFORMS_MODULE_CLASS(NULL)
{
    // setup links
    llpi = l;
    system = s;
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
void
CONTROLLER_CLASS::Main()
{
    // send all dynamic parameters to the hardware
    PARAMS_CONTROLLER_CLASS::GetInstance()->SendAllParams();

    // Tell model which contexts are enabled.  Clearly this will need to change.
    for (int c = 0; c < globalArgs->NumContexts(); c++)
    {
        STARTER_CLASS::GetInstance()->EnableContext(c);
    }

    // send "start" signal to the hardware partition.
    STARTER_CLASS::GetInstance()->Run();

    // transfer control to System
    system->Main();

    // system's Main() exited => end simulation

    // stop hardware
    // STARTER_CLASS::GetInstance()->Pause();
    // STARTER_CLASS::GetInstance()->Sync();
    
    return;
}
