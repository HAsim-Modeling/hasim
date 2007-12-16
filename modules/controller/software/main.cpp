#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>
#include <getopt.h>

#include "main.h"
#include "swcon-service.h"
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

void process_options(int argc, char *argv[]);

// main
int main(int argc, char *argv[])
{
    // parse args and place in global array
    process_options(argc, argv);

    // transfer control to controller
    controller.Main();

    return 0;
}

// ----- controller's Main -----
void
SOFTWARE_CONTROLLER_CLASS::Main()
{
    // connect to my (already running) RRR service
    myService = SWCON_SERVICE_CLASS::GetInstance();
    myService->Connect(this);

    // instantiate channelIO and RRR server.
    // instantiating channelio has the side-effect of
    // initializing the hardware partition
    channelio = new CHANNELIO_CLASS(this);
    rrrServer = new RRR_SERVER_CLASS(this, channelio);
    rrrClient = new RRR_CLIENT_CLASS(this, channelio);

    // send "start" signal to the hardware partition.
    // We currently do it by asking our RRR service to
    // respond "Yes" to a "Should I Start?" RRR request
    myService->StartHardware();

    // go into the main scheduler loop
    SchedulerLoop();

    // end of simulation... cleanup and exit
    Uninit();
}

// process command-line options
void process_options(int argc, char *argv[])
{
    int c;

    // first, set default values for globalArgs
    globalArgs.showFrontPanel = true;
    strcpy(globalArgs.benchmark, "program.vmh");

    while (true)
    {
        int this_option_optind = optind ? optind : 1;
        int option_index = 0;
        static struct option long_options[] =
        {
            {"showfp", required_argument, NULL, 0},
            {0, 0, 0, 0}
        };

        c = getopt_long (argc, argv, "", long_options, &option_index);
        if (c == -1)
        {
            break;
        }

        switch (c)
        {
            case 0:
                if (option_index == 0 && optarg && !strcmp(optarg, "0"))
                {
                    globalArgs.showFrontPanel = false;
                }
                break;

            case '?':
                break;

            default:
                fprintf (stderr, "?? getopt returned character code 0%o ??\n", c);
        }
    }

    if (optind < argc)
    {
        strcpy(globalArgs.benchmark, argv[optind++]);
    }
    
}
