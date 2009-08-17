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
#include "asim/provides/software_system.h"
#include "asim/provides/virtual_platform.h"
#include "asim/provides/low_level_platform_interface.h"

#include "asim/provides/model.h"
#include "hardware-done.h"


// =======================================
//                 MAIN
// =======================================

// globally visible variables
extern GLOBAL_ARGS globalArgs;

// main
int main(int argc, char *argv[])
{
    // Set line buffering to avoid fflush() everywhere.  stderr was probably
    // unbuffered already, but be sure.
    setvbuf(stdout, NULL, _IOLBF, 0);
    setvbuf(stderr, NULL, _IOLBF, 0);

    // instantiate:
    // 1. Virtual platform
    // 2. LLPI
    // 3. System
    // 4. Controller
    VIRTUAL_PLATFORM vp         = new VIRTUAL_PLATFORM_CLASS();
    SYSTEM           system     = new SYSTEM_CLASS();
    CONTROLLER       controller = new CONTROLLER_CLASS(vp->llpint, system);

    // Set up default switches
    globalArgs = new GLOBAL_ARGS_CLASS();
    
    // Process command line arguments
    COMMAND_SWITCH_PROCESSOR switchProc = new COMMAND_SWITCH_PROCESSOR_CLASS();
    switchProc->ProcessArgs(argc, argv);

    vp->Init();

    // transfer control to controller
    controller->Main();

    // Application's Main() exited => wait for hardware to be done.
    // The user can use a parameter to indicate the hardware never 
    // terminates (IE because it's a pure server).
    
    if (WAIT_FOR_HARDWARE && !hardwareFinished)
    {
        // We need to wait for it and it's not finished.
        // So we'll wait to receive the signal from the VP.

        pthread_mutex_lock(&hardwareStatusLock);
        pthread_cond_wait(&hardwareFinishedSignal, &hardwareStatusLock);
        pthread_mutex_unlock(&hardwareStatusLock);
    }

    // cleanup and exit
    delete controller;
    delete system;
    delete vp;

    return 0;
}
