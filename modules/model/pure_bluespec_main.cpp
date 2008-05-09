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

#include "asim/dict/init.h"

#include "asim/provides/command_switches.h"
#include "asim/provides/low_level_platform_interface.h"

#include "pure_bluespec_main.h"

// =======================================
//                 MAIN
// =======================================

// globally visible variables
GLOBAL_ARGS globalArgs;

// main
int main(int argc, char *argv[])
{
    // parse args and place in global array
    globalArgs = new GLOBAL_ARGS_CLASS(argc, argv);

    // instantiate:
    // 1. LLPI
    // 2. Controller
    // 3. System
    LLPI       llpi       = new LLPI_CLASS();

    // transfer control to controller
    while (true)
    {
        // FIXME: directly poll LLPI
        llpi->Poll();
    }

    // cleanup and exit
    delete llpi;

    return 0;
}

