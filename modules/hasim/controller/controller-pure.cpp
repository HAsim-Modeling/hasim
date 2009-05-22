#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "asim/provides/hasim_controller.h"


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

// *** trampolene function for LLPI's Main() ***
void * LLPI_Main(void *argv)
{
    LLPI instance = LLPI(argv);
    instance->Main();
    return NULL;
}

// controller's main()
void
CONTROLLER_CLASS::Main()
{
    // spawn off Monitor/Service thread by calling LLPI's Main()
    if (pthread_create(&monitorThreadID,
                       NULL,
                       LLPI_Main,
                       (void *)llpi) != 0)
    {
        perror("pthread_create");
        exit(1);
    }

    //
    // I am now the System thread
    //

    // send "start" signal to the hardware partition.
    STARTER_CLASS::GetInstance()->Run();

    // transfer control to System
    system->Main();

    // system's Main() exited => end simulation

    // stop hardware
    STARTER_CLASS::GetInstance()->Pause();
    STARTER_CLASS::GetInstance()->Sync();

    CallbackExit(0);
}
