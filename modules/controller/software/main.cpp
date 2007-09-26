#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>

#include "main.h"
#include "software-controller.h"

/***** ----- SOFTWARE CONTROLLER ----- *****

 This is where a Hybrid model begins
 execution. This module:
 - has the software main()
 - instantiates the software module
   hierarchy (including the software
   RRR server)
 - initializes the hardware via RRR
 - also provides some RRR service
   functions (stats, events, exceptions)

 ***** ------------------------------- *****/

// globally visible variables
GlobalArgs globalArgs;

// create actual software controller
SOFTWARE_CONTROLLER_CLASS controller;

// RRR service module
extern SWCON_SERVICE_CLASS swconServiceInstance;

// main
int main(int argc, char *argv[])
{
    // parse args and place in global array
    if (argc == 2)
    {
        strcpy(globalArgs.benchmark, argv[1]);
    }
    else
    {
        strcpy(globalArgs.benchmark, "program.vmh");
    }

    // transfer control to controller
    controller.Main();

    return 0;
}

// ----- controller's Main -----
void
SOFTWARE_CONTROLLER_CLASS::Main()
{
    // connect to my (already running) RRR service
    myService = &swconServiceInstance;
    myService->Connect(this);

    // instantiate channelIO and RRR server.
    // instantiating channelio has the side-effect of
    // initializing the hardware partition
    channelio = new CHANNELIO_CLASS(this);
    rrrServer = new RRR_SERVER_CLASS(this, channelio);

    // send "start" signal to the hardware partition.
    // We currently do it by asking our RRR service to
    // respond "Yes" to a "Should I Start?" RRR request
    myService->StartHardware();

    // go into the main scheduler loop
    SchedulerLoop();

    // end of simulation... cleanup and exit
    Uninit();
}
