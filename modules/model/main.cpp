#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>

#include "asim/syntax.h"

#include "asim/dict/init.h"

#include "asim/provides/command_switches.h"
#include "asim/provides/hasim_controller.h"
#include "asim/provides/virtual_platform.h"
#include "asim/provides/low_level_platform_interface.h"

#include "main.h"

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
    // 1. Virtual platform
    // 2. LLPI
    // 3. Controller
    VIRTUAL_PLATFORM vp   = new VIRTUAL_PLATFORM_CLASS();
    LLPI       llpi       = new LLPI_CLASS();
    CONTROLLER controller = new CONTROLLER_CLASS(llpi);

    // transfer control to controller
    int exitcode = controller->Main();

    // cleanup and exit
    delete controller;
    delete llpi;
    delete vp;

    return exitcode;
}
