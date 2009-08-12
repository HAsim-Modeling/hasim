#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "asim/provides/software_system.h"
#include "asim/provides/params_controller.h"
#include "controller-basic.h"


using namespace std;

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
