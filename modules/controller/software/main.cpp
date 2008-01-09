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
#include "control.h"

// =======================================
//                 MAIN
// =======================================

// globally visible variables
GlobalArgs globalArgs;

void process_options(int argc, char *argv[]);

// main
int main(int argc, char *argv[])
{
    // parse args and place in global array
    process_options(argc, argv);

    // FIXME: we have to dynamically create the controller
    // since the RRR server (which is a submodule of the
    // controller) should only be initialized AFTER all static
    // services have registered themselves with it
    CONTROLLER controller = new CONTROLLER_CLASS();

    // transfer control to controller
    int exitcode = controller->Main();

    // cleanup and exit
    delete controller;
    return exitcode;
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
