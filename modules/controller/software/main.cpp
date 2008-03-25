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
#include "asim/dict/init.h"
#include "asim/provides/low_level_platform_interface.h"

// =======================================
//                 MAIN
// =======================================

// globally visible variables
GlobalArgs globalArgs;

// prototypes
void process_options(int argc, char *argv[]);

// main
int main(int argc, char *argv[])
{
    // parse args and place in global array
    process_options(argc, argv);

    // instantiate:
    // 1. LLPI
    // 2. Controller
    // 3. System
    LLPI       llpi       = new LLPI_CLASS();
    CONTROLLER controller = new CONTROLLER_CLASS(llpi);

    // transfer control to controller
    int exitcode = controller->Main();

    // cleanup and exit
    delete controller;
    delete llpi;

    return exitcode;
}

// process command-line options
void process_options(int argc, char *argv[])
{
    int c;

    // first, set default values for globalArgs
    globalArgs.showFrontPanel = false;
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
                if (option_index == 0 && optarg)
                {
                    if (!strcmp(optarg, "0"))
                    {
                        globalArgs.showFrontPanel = false;
                    }
                    else
                    {
                        globalArgs.showFrontPanel = true;
                    }
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
