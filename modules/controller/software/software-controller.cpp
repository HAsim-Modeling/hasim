#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "software-controller.h"

using namespace std;

// map table of messages
const int MSG_TYPES = 5;
char MSG_TABLE[MSG_TYPES][256] =
{
    "[%0d]: controller: program started.\n",
    "[%0d]: controller: test program finished succesfully.\n",
    "[%0d]: controller: test program finished, one or more failures occurred.\n",
    "[%0d]: controller: ERROR: unexpected timing partition response: ",
    "0x%x\n"
};

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
