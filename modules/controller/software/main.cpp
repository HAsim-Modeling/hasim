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

    // create actual software controller
    SOFTWARE_CONTROLLER_CLASS controller;

    // go into an infinite loop, polling the controller
    // (this will hierarchically poll all sub-modules... we need
    //  to refine this using a proper "poll-server"
    while (true)
    {
        controller.Poll();
    }

    return 0;
}
