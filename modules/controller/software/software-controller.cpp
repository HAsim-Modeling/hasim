#include <stdio.h>
#include <stdlib.h>

#include "software-controller.h"

// constructor
SOFTWARE_CONTROLLER_CLASS::SOFTWARE_CONTROLLER_CLASS()
{
    Init();
}

// destructor
SOFTWARE_CONTROLLER_CLASS::~SOFTWARE_CONTROLLER_CLASS()
{
    Uninit();
}

// init
void
SOFTWARE_CONTROLLER_CLASS::Init()
{
    // instantiate submodules
    channelio = new CHANNELIO_CLASS(this);
    rrrServer = new RRR_SERVER_CLASS(this, channelio);
}

// destructor
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

// poll
void
SOFTWARE_CONTROLLER_CLASS::Poll()
{
    // clock submodules
    channelio->Poll();
    rrrServer->Poll();
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
