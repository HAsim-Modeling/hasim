#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "asim/rrr/service_ids.h"
#include "asim/provides/starter.h"
#include "asim/provides/command_switches.h"
#include "asim/provides/central_controllers.h"
#include "asim/provides/hasim_controller.h"
#include "asim/provides/hasim_modellib.h"

#include "asim/ioformat.h"

using namespace std;

// ===== service instantiation =====
STARTER_SERVER_CLASS STARTER_SERVER_CLASS::instance;

// constructor
STARTER_SERVER_CLASS::STARTER_SERVER_CLASS() :
    fpgaStartCycle(0),
    modelStartCycle(0),
    modelStartInstrs(0),
    latestFMR(-1),
    instrCommits(0),
    modelCycles(0),
    nextProgressMsgCycle(0)
{
    // instantiate stubs
    clientStub = new STARTER_CLIENT_STUB_CLASS(this);
    serverStub = new STARTER_SERVER_STUB_CLASS(this);

    //
    // The timing model doesn't stop running while statistics are scanned out.
    // Make sure the counters are big enough so they don't wrap while the
    // scan is running.  At least 24 bits should be safe.
    //
    ASSERT(HASIM_STATS_SIZE >= 24, "Statistics counter too small");

    //
    // We assume that HW statistics counters may be incremented at most once
    // per model cycle.  The heartbeat will be used to trigger scanning out
    // intermediate counter values in the middle of a run.  Here we confirm
    // that the heartbeat is frequent enough.
    //
    ASSERT(HEARTBEAT_TRIGGER_BIT < HASIM_STATS_SIZE - 2,
           "Heartbeat too infrequent for triggering statistics scan out");

    statsScanMask = 1 << (HASIM_STATS_SIZE - 2);
    lastStatsScanCycle = 0;
}

// destructor
STARTER_SERVER_CLASS::~STARTER_SERVER_CLASS()
{
    Cleanup();
}

// init
void
STARTER_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    parent = p;
    nextProgressMsgCycle = globalArgs->ProgressMsgInterval();
}

// uninit: override
void
STARTER_SERVER_CLASS::Uninit()
{
    // cleanup
    Cleanup();
    
    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
STARTER_SERVER_CLASS::Cleanup()
{
    // deallocate stubs
    delete clientStub;
    delete serverStub;
}

// poll
void
STARTER_SERVER_CLASS::Poll()
{
}

//
// RRR service requests
//

// EndSim
void
STARTER_SERVER_CLASS::EndSim(
    UINT8 success)
{
    if (success == 1)
    {
        cout << "        starter: completed successfully. ";
    }
    else
    {
        cout << "        starter: completed with errors.  ";
    }
    
    EndSimulation(success == 0);
}

// Heartbeat
void
STARTER_SERVER_CLASS::Heartbeat(
    UINT64 fpga_cycles,
    UINT32 model_cycles,
    UINT32 instr_commits)
{
    modelCycles  += model_cycles;
    instrCommits += instr_commits;

    gettimeofday(&heartbeatLastTime, NULL);
    
    if (fpgaStartCycle == 0)
    {
        //
        // First heartbeat, which is some distance in to the run.  Record
        // first values seen so startup timing doesn't affect reported values.
        //
        fpgaStartCycle = fpga_cycles;
        modelStartCycle = modelCycles;
        modelStartInstrs = instrCommits;
        heartbeatStartTime = heartbeatLastTime;
    }
    else
    {
        latestFMR = (double)(fpga_cycles - fpgaStartCycle) /
            (double)(modelCycles - modelStartCycle);
    }
    
    if (nextProgressMsgCycle && (modelCycles >= nextProgressMsgCycle))
    {
        nextProgressMsgCycle += globalArgs->ProgressMsgInterval();
        
        cout << "[" << std::dec << std::setw(13) << fpga_cycles
             << "]: model cycles: "
             << std::setw(10) << modelCycles;
        
        ProgressStats();
    }
    
    //
    // Is model broken?
    //
    if (instr_commits == 0)
    {
        ASIMERROR("No instructions committed for entire heartbeat interval (" <<
                  model_cycles << " cycles)");
    }

    //
    // HW statistics counters are smaller than full counters to save
    // space.  Time to scan out intermediate statistics values before
    // they wrap around?
    //
    if (((modelCycles ^ lastStatsScanCycle) & statsScanMask) != 0)
    {
        lastStatsScanCycle = modelCycles;
        DumpStats();
    }
    
    //
    // Done?
    //
    if (globalArgs->StopCycle() && (modelCycles >= globalArgs->StopCycle()))
    {
        cout << "starter: simulation reached stop cycle." << endl;
        EndSimulation(0);
    }
}

//
// Local methods
//

void
STARTER_SERVER_CLASS::EndSimulation(int exitValue)
{
    struct timeval end_time;
    double sec;
    double usec;
    double elapsed;

    // stop the clock
    gettimeofday(&end_time, NULL);
    sec = double(end_time.tv_sec) - double(startTime.tv_sec);
    usec = double(end_time.tv_usec) - double(startTime.tv_usec);
    elapsed = sec + usec/1000000;

    if (modelCycles > 0)
    {
        ProgressStats();
    }

    cout << "        syncing... ";
    Sync();
    cout << "sunk." << endl;

    cout << "        starting stats dump... ";
    DumpStats();
    StatsEmitFile();
    cout << "done." << endl;

    printf("        elapsed (wall-clock) time = %.4f seconds.\n", elapsed);

    CallbackExit(exitValue);
}


void
STARTER_SERVER_CLASS::ProgressStats()
{
    double sec = double(heartbeatLastTime.tv_sec) - double(heartbeatStartTime.tv_sec);
    double usec = double(heartbeatLastTime.tv_usec) - double(heartbeatStartTime.tv_usec);
    double heartbeat_run_time = sec + usec/1000000;

    cout << " (IPC=" << IoFormat::fmt(".2f", (double)instrCommits / (double)modelCycles);

    UINT64 commits = instrCommits - modelStartInstrs;
    if ((heartbeat_run_time > 0) && (commits > 0))
    {
        cout << " / IPS="
             << IoFormat::fmt(".2f", (double)commits / heartbeat_run_time);
    }

    if (latestFMR >= 0)
    {
        cout << " / FMR=" << IoFormat::fmt(".1f", latestFMR);
    }

    cout << ")" << endl;
}


// client: run
void
STARTER_SERVER_CLASS::Run()
{
    // log start time
    gettimeofday(&startTime, NULL);

    // call client stub
    clientStub->Run(0);
}

// client: pause
void
STARTER_SERVER_CLASS::Pause()
{
    clientStub->Pause(0);
}

// client: sync
void
STARTER_SERVER_CLASS::Sync()
{
    clientStub->Sync(0);
}

// client: dump stats
void
STARTER_SERVER_CLASS::DumpStats()
{
    UINT32 ack = clientStub->DumpStats(0);
}

// client: enable context
void
STARTER_SERVER_CLASS::EnableContext(UINT32 ctx_id)
{
    clientStub->EnableContext(ctx_id);
}

// client: disable context
void
STARTER_SERVER_CLASS::DisableContext(UINT32 ctx_id)
{
    clientStub->DisableContext(ctx_id);
}
