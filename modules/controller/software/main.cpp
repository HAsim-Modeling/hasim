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
HASIM_GLOBAL_ARGS globalArgs;

// main
int main(int argc, char *argv[])
{
    // parse args and place in global array
    globalArgs = new HASIM_GLOBAL_ARGS_CLASS(argc, argv);

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
HASIM_GLOBAL_ARGS_CLASS::HASIM_GLOBAL_ARGS_CLASS(int argc, char *argv[]) :
    benchmark("program.vmh"),
    modelDir("."),
    showFrontPanel(false)
{
    enum 
    {
        OPT_BENCHMARK,
        OPT_MODELDIR,
        OPT_SHOWFP,
        OPT_NOSHOWFP
    };

    int c;

    while (true)
    {
        int this_option_optind = optind ? optind : 1;
        int option_index = 0;
        static struct option long_options[] =
        {
            {"benchmark", required_argument, NULL, OPT_BENCHMARK},
            {"modeldir", required_argument, NULL, OPT_MODELDIR},
            {"showfp", no_argument, NULL, OPT_SHOWFP},
            {"noshowfp", no_argument, NULL, OPT_NOSHOWFP},
            {0, 0, 0, 0}
        };

        c = getopt_long (argc, argv, "", long_options, &option_index);
        if (c == -1)
        {
            break;
        }

        switch (c)
        {
          case OPT_BENCHMARK:
            benchmark = strdup(optarg);
            break;

          case OPT_MODELDIR:
            modelDir = strdup(optarg);
            break;

          case OPT_SHOWFP:
            showFrontPanel = true;
            break;

          case OPT_NOSHOWFP:
            showFrontPanel = false;
            break;

          case '?':
            Usage();

          default:
            Usage();
        }
    }
}


void
HASIM_GLOBAL_ARGS_CLASS::Usage()
{
    fprintf(stderr, "\nArguments:\n");
    fprintf(stderr, "   [--[no]showfp]         Show/don't show front panel\n");
    fprintf(stderr, "   [--modeldir <dir>]     Model directory\n");
    fprintf(stderr, "   [--benchmark <name>]   User-model benchmark image\n");
    exit(1);
}
