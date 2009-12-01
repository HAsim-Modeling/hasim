#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "asim/atomic.h"

#include "asim/rrr/service_ids.h"
#include "asim/provides/hasim_common.h"
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
    stopCycleSwitch(),
    messageIntervalSwitch(),
    lastStatsScanCycle(0),
    lastFPGAClockModelCycles(0),
    lastFPGAClockCommits(0),
    noChangeBeats(0)
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
    for (CONTEXT_ID c = 0; c < NUM_CONTEXTS; c++)
    {
        ctxHeartbeat[c].Init(&messageIntervalSwitch);
    }
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
        cout << "        starter: completed successfully. " << endl;
    }
    else
    {
        cout << "        starter: completed with errors.  " << endl;
    }
    
    EndSimulation(success == 0);
}

// Heartbeat
void
STARTER_SERVER_CLASS::Heartbeat(
    UINT8 fpgaClockBeat,
    CONTEXT_ID ctxId,
    UINT64 fpga_cycles,
    UINT32 model_cycles,
    UINT32 instr_commits)
{
    if (! fpgaClockBeat)
    {
        ctxHeartbeat[ctxId].Heartbeat(ctxId, fpga_cycles, model_cycles, instr_commits);
    }

    //
    // HW statistics counters are smaller than full counters to save
    // space.  Time to scan out intermediate statistics values before
    // they wrap around?
    //
    UINT64 total_model_cycles = 0;
    UINT64 total_model_commits = 0;
    for (CONTEXT_ID c = 0; c < NUM_CONTEXTS; c++)
    {
        total_model_cycles += ctxHeartbeat[ctxId].GetModelCycles();
        total_model_commits += ctxHeartbeat[ctxId].GetInstrCommits();
    }

    //
    // Is the current heartbeat an FPGA-clock heartbeat?  If so, just check
    // for deadlocks.
    //
    if (fpgaClockBeat)
    {
        // Has the model progressed since the last FPGA clock heartbeat?
        if ((total_model_cycles == lastFPGAClockModelCycles) &&
            (total_model_commits == lastFPGAClockCommits))
        {
            // No!  Possible deadlock.
            noChangeBeats += 1;
        }
        else
        {
            lastFPGAClockModelCycles = total_model_cycles;
            lastFPGAClockCommits = total_model_commits;
            noChangeBeats = 0;
        }

        if (noChangeBeats == 1000)
        {
            cerr << "starter: model deadlock!" << endl;
            DebugScan();
            EndSimulation(1);
        }

        return;
    }


    if (((total_model_cycles ^ lastStatsScanCycle) & statsScanMask) != 0)
    {
        lastStatsScanCycle = total_model_cycles;
        DumpStats();
    }
    
    //
    // Done?
    //
    if (stopCycleSwitch.StopCycle() &&
        (ctxHeartbeat[ctxId].GetModelCycles() >= stopCycleSwitch.StopCycle()))
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

    UINT64 allModelCycles = 0;
    UINT64 allInstrCommits = 0;
    UINT64 allFPGACycles = 0;
    double allIPS = 0;

    cout << endl;
    for (int c = 0; c < NUM_CONTEXTS; c++)
    {
        if (ctxHeartbeat[c].GetModelCycles() != 0)
        {
            cout << "    ";
            ctxHeartbeat[c].ProgressStats(c);
        }

        //
        // Compute a merged summary of all contexts
        //

        allModelCycles += (ctxHeartbeat[c].GetModelCycles() -
                           ctxHeartbeat[c].GetModelStartCycle());

        UINT64 fpga_cycles = ctxHeartbeat[c].GetFPGACycles();
        if (fpga_cycles > allFPGACycles)
        {
            allFPGACycles = fpga_cycles;
        }

        allInstrCommits += (ctxHeartbeat[c].GetInstrCommits() -
                            ctxHeartbeat[c].GetInstrStartCommits());
        allIPS += ctxHeartbeat[c].GetModelIPS();
    }

    if (allModelCycles > 0)
    {
        cout << "    ALL                      ";
        cout << " (IPC=" << IoFormat::fmt(".2f", (double)allInstrCommits / (double)allModelCycles)
             << " / IPS="  << IoFormat::fmt(".2f", allIPS);

        if (allModelCycles > 0)
        {
            double fmr = double(allFPGACycles) / double(allModelCycles);
            cout << " / FMR=" << IoFormat::fmt(".1f", fmr);
        }

        cout << ")" << endl;
    }
    cout << endl;

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

// client: debug scan
void
STARTER_SERVER_CLASS::DebugScan()
{
    // Only allow one scan to be active at a time.  Just drop requests that
    // happen simultaneously.
    static volatile UINT32 scanBusy = 0;
    if (CompareAndExchangeU32(&scanBusy, 0, 1))
    {
        UINT32 ack = clientStub->DebugScan(0);
        scanBusy = 0;
    }
}

// client: enable context
void
STARTER_SERVER_CLASS::EnableContext(CONTEXT_ID ctx_id)
{
    clientStub->EnableContext(ctx_id);
}

// client: disable context
void
STARTER_SERVER_CLASS::DisableContext(CONTEXT_ID ctx_id)
{
    clientStub->DisableContext(ctx_id);
}


// ========================================================================
//
// Heartbeat monitor class for a single context.
//
// ========================================================================

CONTEXT_HEARTBEAT_CLASS::CONTEXT_HEARTBEAT_CLASS() :
    fpgaStartCycle(0),
    fpgaLastCycle(0),
    modelStartCycle(0),
    modelStartInstrs(0),
    latestFMR(-1),
    instrCommits(0),
    modelCycles(0),
    nextProgressMsgCycle(0)
{}


void
CONTEXT_HEARTBEAT_CLASS::Init(MESSAGE_INTERVAL_SWITCH_CLASS* mis)
{
    messageIntervalSwitch = mis;
    nextProgressMsgCycle = mis->ProgressMsgInterval();
}

void
CONTEXT_HEARTBEAT_CLASS::Heartbeat(
    CONTEXT_ID ctxId,
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
    
    fpgaLastCycle = fpga_cycles;

    if (nextProgressMsgCycle && (modelCycles >= nextProgressMsgCycle))
    {
        nextProgressMsgCycle += messageIntervalSwitch->ProgressMsgInterval();
        
        cout << "[" << std::dec << std::setw(13) << fpga_cycles << "] ";
        ProgressStats(ctxId);
    }
    
    //
    // Is model broken?
    //
    if (instr_commits == 0)
    {
        ASIMERROR("No instructions committed for entire heartbeat interval (" <<
                  model_cycles << " cycles)");
    }
}

void
CONTEXT_HEARTBEAT_CLASS::ProgressStats(CONTEXT_ID ctxId)
{
    if (modelCycles == 0) return;

    cout << "CTX " << std::setw(2) << UINT64(ctxId)
         << ": " << std::setw(10) << modelCycles << " cycles";

    cout << " (IPC=" << IoFormat::fmt(".2f", (double)instrCommits / (double)modelCycles);

    double ips = GetModelIPS();
    if (ips > 0)
    {
        cout << " / IPS="  << IoFormat::fmt(".2f", ips);
    }

    if (latestFMR >= 0)
    {
        cout << " / FMR=" << IoFormat::fmt(".1f", latestFMR);
    }

    cout << ")" << endl;
}

double
CONTEXT_HEARTBEAT_CLASS::GetModelIPS() const
{
    double sec = double(heartbeatLastTime.tv_sec) - double(heartbeatStartTime.tv_sec);
    double usec = double(heartbeatLastTime.tv_usec) - double(heartbeatStartTime.tv_usec);
    double heartbeat_run_time = sec + usec/1000000;

    UINT64 commits = instrCommits - modelStartInstrs;
    if ((heartbeat_run_time > 0) && (commits > 0))
    {
        return (double)commits / heartbeat_run_time;
    }
    else
    {
        return 0;
    }
}

// Switch for reading the stop cycle.
STOP_CYCLE_SWITCH_CLASS::STOP_CYCLE_SWITCH_CLASS() :
    COMMAND_SWITCH_INT_CLASS("cycles")
{
}

STOP_CYCLE_SWITCH_CLASS::~STOP_CYCLE_SWITCH_CLASS()
{
}

void
STOP_CYCLE_SWITCH_CLASS::ProcessSwitchInt(int arg)
{
    stopCycle = arg;
}

bool
STOP_CYCLE_SWITCH_CLASS::ShowSwitch(char* buff)
{
    strcpy(buff, "[--cycles=<n>]          Stop simulation after n cycles");
}

// Switch for reading the progress message interval
MESSAGE_INTERVAL_SWITCH_CLASS::MESSAGE_INTERVAL_SWITCH_CLASS() :
    COMMAND_SWITCH_INT_CLASS("pc"),
    messageInterval(1)
{
}

MESSAGE_INTERVAL_SWITCH_CLASS::~MESSAGE_INTERVAL_SWITCH_CLASS()
{
}

void
MESSAGE_INTERVAL_SWITCH_CLASS::ProcessSwitchInt(int arg)
{
    messageInterval = arg;
}

bool
MESSAGE_INTERVAL_SWITCH_CLASS::ShowSwitch(char* buff)
{
    strcpy(buff, "[--pc=<interval>]       Progress message (hearbeat) interval.");
}
