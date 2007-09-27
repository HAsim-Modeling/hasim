#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <iostream>

#include "swcon-service.h"
#include "software-controller.h"

#define HWSTATE_IDLE    0
#define HWSTATE_RUNNING 1
#define HWSTATE_STOPPED 2

using namespace std;

// service instantiation
SWCON_SERVICE_CLASS swconServiceInstance;
RRR_SERVICE_CLASS   *SWCON_SERVICE_service = &swconServiceInstance;

// constructor
SWCON_SERVICE_CLASS::SWCON_SERVICE_CLASS()
{
    hwState = HWSTATE_IDLE;
    controller = NULL;
    connected = false;
}

// destructor
SWCON_SERVICE_CLASS::~SWCON_SERVICE_CLASS()
{
    Uninit();
}

// init
void
SWCON_SERVICE_CLASS::Init(
    HASIM_SW_MODULE     p,
    int                 ID)
{
    if (connected == false)
    {
        // cannot call Init without having connected to
        // main controller
        cerr << "controller service: init called before connecting"
             << endl;
        CallbackExit(1);
    }

    // set service ID and parent pointer
    serviceID = ID;
    parent = p;
}

// uninit
void
SWCON_SERVICE_CLASS::Uninit()
{
}

// connect request to main controller
void
SWCON_SERVICE_CLASS::Connect(
    SOFTWARE_CONTROLLER c)
{
    controller = c;
    connected = true;
}

// start hardware
void
SWCON_SERVICE_CLASS::StartHardware()
{
    if (hwState != HWSTATE_IDLE)
    {
        cerr << "controller service: invalid hardware state" << endl;
        CallbackExit(1);
    }

    hwState = HWSTATE_RUNNING;
}

// stop hardware
void
SWCON_SERVICE_CLASS::StopHardware()
{
    hwState = HWSTATE_STOPPED;
}

// request
bool
SWCON_SERVICE_CLASS::Request(
    UINT32 arg0,
    UINT32 arg1,
    UINT32 arg2,
    UINT32 *result)
{
    bool retval = false;

    // decode request
    switch(arg0)
    {
        case 0: // state query
            *result = hwState;
            retval = true;
            break;

        case 1: // print message
            controller->PrintMessage(arg1, arg2);
            retval = false;
            break;

        default:
            cerr << "controller service: invalid request" << endl;
            CallbackExit(1);
            break;
    }

    return retval;
}

// poll
void
SWCON_SERVICE_CLASS::Poll()
{
}