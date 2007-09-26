#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "software-controller.h"

using namespace std;

// constructor
SOFTWARE_CONTROLLER_CLASS::SOFTWARE_CONTROLLER_CLASS()
{
}

// destructor
SOFTWARE_CONTROLLER_CLASS::~SOFTWARE_CONTROLLER_CLASS()
{
    Uninit();
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

// print string
void
SOFTWARE_CONTROLLER_CLASS::PrintString(
    char buf[])
{
    cout << buf << endl;
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
