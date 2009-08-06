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
#include "asim/provides/hasim_controller.h"
#include "asim/provides/bluespec_system.h"
#include "asim/provides/model.h"

#include "pure_bluespec_main.h"

// =======================================
//           PURE BLUESPEC MAIN
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
    SYSTEM     system     = new BLUESPEC_SYSTEM_CLASS(llpi);
    CONTROLLER controller = new CONTROLLER_CLASS(llpi, system);

    // transfer control to controller
    controller->Main();

    // cleanup and exit
    delete controller;
    delete system;
    delete llpi;

    return 0;
}
