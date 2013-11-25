//
// Copyright (C) 2008 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <iostream>

#include "asim/syntax.h"
#include "asim/mesg.h"

#include "asim/provides/soft_services_deps.h"
#include "asim/provides/command_switches.h"
#include "asim/provides/stats_service.h"
#include "asim/provides/commands_service.h"
#include "asim/provides/starter_device.h"

using namespace std;

// ===== service instantiation =====
COMMANDS_SERVER_CLASS COMMANDS_SERVER_CLASS::instance;

volatile bool COMMANDS_SERVER_CLASS::scanning = false;
std::mutex COMMANDS_SERVER_CLASS::scanDoneMutex;
std::condition_variable COMMANDS_SERVER_CLASS::scanDoneCond;
bool COMMANDS_SERVER_CLASS::scanDoneReceived;


struct END_SIMULATION_STATE_CLASS
{
    COMMANDS_SERVER cmdServer;
    int exitValue;
};

typedef END_SIMULATION_STATE_CLASS* END_SIMULATION_STATE;


static void *EndSimulationThread(void *arg);
static void *DebugScanExitThread(void *arg);
static void *TestThroughputThread(void *arg);



// constructor
COMMANDS_SERVER_CLASS::COMMANDS_SERVER_CLASS() :
    stopCycleSwitch(),
    messageIntervalSwitch(),
    tpTestSwitch(),
    lastStatsScanCycle(0),
    noChangeBeats(0),
    running(false),
    healthy(true),
    scanParser("([0-9]+),([0-9]+),([0-9]+),([0-9]+),(.*)"),
    scanRunningIdx(0),
    scanStream(&cout)
{
    SetTraceableName("commands_server");

    // instantiate stubs
    clientStub = new COMMANDS_CLIENT_STUB_CLASS(this);
    serverStub = new COMMANDS_SERVER_STUB_CLASS(this);

    hwThreadHeartbeat = NULL;
}

// destructor
COMMANDS_SERVER_CLASS::~COMMANDS_SERVER_CLASS()
{
    Cleanup();
}

// init
void
COMMANDS_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    PLATFORMS_MODULE_CLASS::Init(p);
}

// uninit: override
void
COMMANDS_SERVER_CLASS::Uninit()
{
    // cleanup
    Cleanup();
    
    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
COMMANDS_SERVER_CLASS::Cleanup()
{
    delete [] hwThreadHeartbeat;

    // deallocate stubs
    delete clientStub;
    delete serverStub;
}

// init
void
COMMANDS_SERVER_CLASS::SetNumHardwareThreads(UINT32 num)
{
    numThreads = num;
    hwThreadHeartbeat = new HW_THREAD_HEARTBEAT_CLASS[numThreads];
    for (int c = 0; c < numThreads; c++)
    {
        hwThreadHeartbeat[c].Init(&messageIntervalSwitch);
    }
}

// client: run
void
COMMANDS_SERVER_CLASS::Run()
{
    if (localControllerNames.empty())
    {
        // Collect the names of all the local controllers in scan/throughput test
        // order.  This will be useful if names are needed later in the run.
        onlyCollectNames = true;
        Scan();
        onlyCollectNames = false;
    }

    // Set simulation end cycle
    if (stopCycleSwitch.StopCycle() != 0)
    {
        clientStub->SetEndModelCycle(stopCycleSwitch.StopCycle());
    }

    // Tell model which hardware threads are enabled.  Clearly this will need to change.
    for (int c = 0; c < numThreads; c++)
    {
        clientStub->EnableContext(c);
    }

    // log start time
    gettimeofday(&startTime, NULL);

    // call client stub
    clientStub->Run(0);

    running = true;
    noChangeBeats = 0;
}

// client: pause
void
COMMANDS_SERVER_CLASS::Pause()
{
    running = false;
    clientStub->Pause(0);
}

// client: sync
void
COMMANDS_SERVER_CLASS::Sync()
{
    clientStub->Sync(0);
}

// client: scan (for debugging)
void
COMMANDS_SERVER_CLASS::Scan(std::ostream& ofile)
{
    if (! scanning)
    {
        scanning = true;
        scanDoneReceived = false;

        scanStream = &ofile;

        if (! onlyCollectNames)
        {
            *scanStream << "Commands service scan:" << endl;
        }

        clientStub->Scan(0);

        // Block until scan is done.
        std::unique_lock<std::mutex> scanDoneLock(scanDoneMutex);
        scanDoneCond.wait(scanDoneLock, []{ return scanDoneReceived; });

        if (! onlyCollectNames)
        {
            *scanStream << "Done" << endl;
        }
        scanning = false;
    }
}

//
// Throughput test (for debugging).  The throughput test isolates each local
// controller and tests throughput through the controlled simulator
// pipeline.  The goal is to find the modules with the slowest throughput.
// Without isolation, all stages in a pipeline with cycles appear to have
// the throughput of the slowest stage.
//
void
COMMANDS_SERVER_CLASS::TestThroughput()
{
    if (! scanning)
    {
        scanning = true;
        scanDoneReceived = false;

        tpControllerName = localControllerNames.cbegin();
        cout << "Commands service throughput test:" << endl;

        clientStub->TestThroughput(0);

        // Block until scan is done.
        std::unique_lock<std::mutex> scanDoneLock(scanDoneMutex);
        scanDoneCond.wait(scanDoneLock, []{ return scanDoneReceived; });

        cout << "Done" << endl;
        scanning = false;
    }
}



//
// RRR request methods
//

// EndSim
void
COMMANDS_SERVER_CLASS::EndSim(
    UINT8 success)
{
    if (success == 1)
    {
        cout << "        commands relay: completed successfully. " << endl;
    }
    else
    {
        cout << "        commands relay: completed with errors.  " << endl;
    }
    
    running = false;
    EndSimulation(success == 0);
}


//
// FPGA hardware heartbeat used for deadlock detection.
//
void
COMMANDS_SERVER_CLASS::FPGAHeartbeat(UINT8 dummy)
{
    if (running)
    {
        noChangeBeats += 1;

        //
        // Standard heartbeat is set to trigger about once a second.  Call
        // deadlock after a minute of no model activity.
        //
        if (healthy && (noChangeBeats == 60))
        {
            healthy = false;
            cerr << "commands relay: model deadlock!" << endl;

            pthread_t tid;
            END_SIMULATION_STATE state = new END_SIMULATION_STATE_CLASS;
            state->cmdServer = this;
            state->exitValue = 1;
            VERIFYX(! pthread_create(&tid, NULL, &DebugScanExitThread, state));
        }
    }
}


//
// Model heartbeat
//
void
COMMANDS_SERVER_CLASS::ModelHeartbeat(
    UINT32 hwThreadId,
    UINT64 fpga_cycles,
    UINT32 model_cycles,
    UINT32 instr_commits)
{
    if (! healthy) return;

    hwThreadHeartbeat[hwThreadId].Heartbeat(hwThreadId, fpga_cycles,
                                            model_cycles, instr_commits);

    // Reset the deadlock counter.
    noChangeBeats = 0;

    //
    // Done?  Perhaps context 0 missed the command to stop.
    //
    if (stopCycleSwitch.StopCycle() &&
        (hwThreadHeartbeat[hwThreadId].GetModelCycles() >= stopCycleSwitch.StopCycle()))
    {
        cout << "commands relay: simulation reached stop cycle." << endl;
        EndSimulation(0);
    }

    // Time to test throughput through each controller?
    if (hwThreadId == 0)
    {
        static UINT32 cnt = 0;
        if (++cnt == tpTestSwitch.Trigger())
        {
            pthread_t tid;
            VERIFYX(! pthread_create(&tid, NULL, &TestThroughputThread, this));
        }
    }

}

//
// Local methods
//

void
COMMANDS_SERVER_CLASS::EndSimulation(int exitValue)
{
    // Has an end already been requested?  Ignore multiple requests.
    static bool simEnding = false;
    if (simEnding) return;
    simEnding = true;

    //
    // Synchronize (balance) the system before retrieving the final statistcs.
    //
    cout << "        syncing... ";
    clientStub->Sync(0);    

    int max_tries = 100000;
    bool not_balanced = true;
    while (not_balanced && (--max_tries != 0))
    {
        not_balanced = (clientStub->IsSynced(0) == 0);
    }

    if (not_balanced)
    {
        cerr << "Failed to synchronize model!" << endl;
    }

    cout << "sunk." << endl;

    // Stop running
    Pause();

    gettimeofday(&endTime, NULL);

    UINT64 allModelCycles = 0;
    UINT64 allInstrCommits = 0;
    UINT64 allFPGACycles = 0;
    double allIPS = 0;

    cout << endl;
    for (int c = 0; c < numThreads; c++)
    {
        if (hwThreadHeartbeat[c].GetModelCycles() != 0)
        {
            cout << "    ";
            hwThreadHeartbeat[c].ProgressStats(c);
        }

        //
        // Compute a merged summary of all HW threads
        //

        allModelCycles += (hwThreadHeartbeat[c].GetModelCycles() -
                           hwThreadHeartbeat[c].GetModelStartCycle());

        UINT64 fpga_cycles = hwThreadHeartbeat[c].GetFPGACycles();
        if (fpga_cycles > allFPGACycles)
        {
            allFPGACycles = fpga_cycles;
        }

        allInstrCommits += (hwThreadHeartbeat[c].GetInstrCommits() -
                            hwThreadHeartbeat[c].GetInstrStartCommits());
        allIPS += hwThreadHeartbeat[c].GetModelIPS();
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

    //
    // Ideally we would just call STATS_SERVER_CLASS DumpStats() and EmitFile()
    // here.  RRR doesn't correctly call statistics services when invoked
    // from this routine, which is itself an RRR software service.  We solve
    // the problem by spawning a new thread to trigger the exit.
    //
    END_SIMULATION_STATE state = new END_SIMULATION_STATE_CLASS;
    state->cmdServer = this;
    state->exitValue = exitValue;

    pthread_t tid;
    VERIFYX(! pthread_create(&tid, NULL, &EndSimulationThread, state));
}


//
// Called as a side-effect of calling the stats device EmitFile...
//
void
COMMANDS_SERVER_CLASS::EmitStats(ofstream &statsFile)
{
    double sec;
    double usec;
    double elapsed;

    sec = double(endTime.tv_sec) - double(startTime.tv_sec);
    usec = double(endTime.tv_usec) - double(startTime.tv_usec);
    elapsed = sec + usec/1000000;

    statsFile << "SIM_WALL_TIME_SEC,\"Simulator: Wall time (sec.)\","
              << elapsed
              << endl;
}


//
// ScanData --
//     All scanned out state from local controllers arrives via RRR through
//     this method.
//
void
COMMANDS_SERVER_CLASS::ScanData(UINT8 data, UINT8 flags)
{
    bool eom = (flags & 1) != 0;

    if ((flags & 2) == 0)
    {
        // Bit 1 of flags is 0, indicating normal scan data...
        ScanDataNormal(data, eom);
    }
    else
    {
        // Payload indicates whether a single instace is running...
        ScanDataRunning(data, eom);
    }
}


//
// ScanDataNormal --
//     Handle arrival of the standard local controller scan.
//
void
COMMANDS_SERVER_CLASS::ScanDataNormal(UINT8 data, bool eom)
{
    scanData.Put(data);

    if (eom)
    {
        GLOBAL_STRING_UID uid = scanData.Get(GLOBAL_STRING_UID_SZ);
        const string *tag = GLOBAL_STRINGS::Lookup(uid);

        //
        // The size and name of the command node are encoded in the string.
        // The first 3 comma separated values are the number of input
        // ports of each category.  The next field is the number of bits in
        // an instance ID.  The final field is the node name.
        //

        VERIFY(scanParser.matchSub(*tag),
               "Illegally formatted command service sizes/name string (" << *tag << ")");

        UINT32 num_inports = atoi(scanParser.getSubstring(1).c_str());
        UINT32 num_unports = atoi(scanParser.getSubstring(2).c_str());
        UINT32 num_outports = atoi(scanParser.getSubstring(3).c_str());
        UINT32 num_iid_bits = atoi(scanParser.getSubstring(4).c_str());

        if (onlyCollectNames)
        {
            localControllerNames.push_back(scanParser.getSubstring(5));
        }
        else
        {
            *scanStream << "  " << scanParser.getSubstring(5)
                        << " (" << num_inports
                        << ", " << num_unports
                        << ", " << num_outports << "):" << endl;

            bool next_is_ready = (scanData.Get(1) != 0);
            *scanStream << "    Next instance (" << scanData.Get(num_iid_bits) << ") is "
                        << (next_is_ready ? "READY" : "NOT ready") << endl;

            *scanStream << "    Cycle: " << scanData.Get(4) << endl;

            *scanStream << "    Ready input ports:";
            for (UINT32 i = 0; i < num_inports; i++)
            {
                if (scanData.Get(1)) *scanStream << " " << i;
            }
            *scanStream << endl;

            if (num_unports)
            {
                *scanStream << "    Ready uncontrolled ports:";
                for (UINT32 i = 0; i < num_unports; i++)
                {
                    if (scanData.Get(1)) *scanStream << " " << i;
                }
                *scanStream << endl;
            }

            *scanStream << "    Ready output ports:";
            for (UINT32 i = 0; i < num_outports; i++)
            {
                if (scanData.Get(1)) *scanStream << " " << i;
            }
            *scanStream << endl;
        }

        scanData.Reset();
    }
}


//
// ScanDataRunning --
//     Handle arrival of the "running" state of each instance managed by a
//     local controller.  Instances are scanning out serially, with the
//     running scan of a controller immediately following the normal scan
//     of the controller.
//
void
COMMANDS_SERVER_CLASS::ScanDataRunning(UINT8 data, bool eom)
{
    if (onlyCollectNames) return;

    // First IID for this controller?
    if (scanRunningIdx == 0)
    {
        *scanStream << "    Running instances:";
    }

    // Is the instance running?
    if ((data & 1) != 0)
    {
        *scanStream << " " << scanRunningIdx;
    }

    if (eom)
    {
        *scanStream << endl;
        scanRunningIdx = 0;
    }
    else
    {
        scanRunningIdx += 1;
    }
}


//
// Done --
//     All packets received.
//
void
COMMANDS_SERVER_CLASS::ScanDone(UINT8 test)
{
    std::unique_lock<std::mutex> scanDoneLock(scanDoneMutex);
    scanDoneReceived = true;
    scanDoneCond.notify_one();
}


//
// ThroughputData --
//     All throughput samples arrive here.
//
void
COMMANDS_SERVER_CLASS::ThroughputData(UINT16 data, UINT8 flags)
{
    static bool first = true;

    if (first)
    {
        cout << "  " << *tpControllerName << ":" << endl << "  ";
        tpControllerName++;
        first = false;
    }

    bool eom = (flags & 1) != 0;

    if (eom)
    {
        cout << endl;
        first = true;
    }
    else
    {
        cout << "  " << (data + 1);
    }
}


// ========================================================================
//
// Heartbeat monitor class for a single hardware thread.
//
// ========================================================================

HW_THREAD_HEARTBEAT_CLASS::HW_THREAD_HEARTBEAT_CLASS() :
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
HW_THREAD_HEARTBEAT_CLASS::Init(MESSAGE_INTERVAL_SWITCH_CLASS* mis)
{
    messageIntervalSwitch = mis;
    nextProgressMsgCycle = mis->ProgressMsgInterval();
}

void
HW_THREAD_HEARTBEAT_CLASS::Heartbeat(
    UINT32 hwThreadId,
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
        ProgressStats(hwThreadId);
    }
    
    //
    // Is model broken?  Consider a model broken if no CPU commits any
    // instructions in a heartbeat interval.
    //
    static UINT64 totalIntervalCommits = 1;
    if (hwThreadId == 0)
    {
        if (totalIntervalCommits == 0)
        {
            ASIMERROR("No instructions committed for entire heartbeat interval (" <<
                      model_cycles << " cycles)");
        }

        totalIntervalCommits = 0;
    }
    totalIntervalCommits += instr_commits;
}

void
HW_THREAD_HEARTBEAT_CLASS::ProgressStats(UINT32 hwThreadId)
{
    if (modelCycles == 0) return;

    cout << "CTX " << std::setw(2) << UINT64(hwThreadId)
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
HW_THREAD_HEARTBEAT_CLASS::GetModelIPS() const
{
    double sec = double(heartbeatLastTime.tv_sec) - double(heartbeatStartTime.tv_sec);
    double usec = double(heartbeatLastTime.tv_usec) - double(heartbeatStartTime.tv_usec);
    double heartbeat_run_time = sec + usec/1000000;

    UINT64 commits = instrCommits - modelStartInstrs;
    if ((heartbeat_run_time > 0) && (commits > 0))
    {
        return double(commits) / heartbeat_run_time;
    }
    else
    {
        return 0;
    }
}


// Switch for reading the stop cycle.
STOP_CYCLE_SWITCH_CLASS::STOP_CYCLE_SWITCH_CLASS() :
    COMMAND_SWITCH_UINT64_CLASS("cycles")
{
    stopCycle = 0;
}

void
STOP_CYCLE_SWITCH_CLASS::ShowSwitch(std::ostream& ostr, const string& prefix)
{
    ostr << prefix << "[--cycles=<n>]          Stop simulation after n cycles" << endl;
}

// Switch for reading the progress message interval
MESSAGE_INTERVAL_SWITCH_CLASS::MESSAGE_INTERVAL_SWITCH_CLASS() :
    COMMAND_SWITCH_UINT64_CLASS("pc"),
    messageInterval(1)
{
}

void
MESSAGE_INTERVAL_SWITCH_CLASS::ShowSwitch(std::ostream& ostr, const string& prefix)
{
    ostr << prefix << "[--pc=<interval>]       Progress message (heartbeat) interval." << endl;
}

// Switch for triggering throughput test
COMMANDS_TP_TEST_SWITCH_CLASS::COMMANDS_TP_TEST_SWITCH_CLASS() :
    COMMAND_SWITCH_UINT64_CLASS("test-throughput"),
    trigger(0)
{
}

void
COMMANDS_TP_TEST_SWITCH_CLASS::ShowSwitch(std::ostream& ostr, const string& prefix)
{
    ostr << prefix << "[--test-throughput=<n>] Trigger local controller throughput testing after n heartbeats." << endl;
}



// ========================================================================
//
//   Fault handlers
//
// ========================================================================

//
// EndSimulationThread is a hack to allow calling DumpStats() as a side-effect
// of ending the simulation.  RRR doesn't deal well with a software service
// (EndSimulation) that calls a hardware service (DumpStats) when the caller
// (DumpStats) blocks until some other software service is called (internal
// to DumpStats).  When we replace RRR with threads this pthreads hack may
// become unnecessary.
//

static void *EndSimulationThread(void *arg)
{
    END_SIMULATION_STATE state = END_SIMULATION_STATE(arg);

    cout << "        starting stats dump... ";
    STATS_SERVER_CLASS::GetInstance()->DumpStats();
    STATS_SERVER_CLASS::GetInstance()->EmitFile();
    cout << "done." << endl;

    STARTER_DEVICE_SERVER_CLASS::GetInstance()->End(state->exitValue);
}


static void *DebugScanExitThread(void *arg)
{
    END_SIMULATION_STATE state = END_SIMULATION_STATE(arg);

    cerr << "commands relay: scanning due to model deadlock..." << endl;

    DEBUG_SCAN_SERVER_CLASS::GetInstance()->Scan(stderr);
    cerr << endl;
    state->cmdServer->Scan(cerr);

    STARTER_DEVICE_SERVER_CLASS::GetInstance()->End(state->exitValue);
}


static void *TestThroughputThread(void *arg)
{
    COMMANDS_SERVER cmdServer = COMMANDS_SERVER(arg);
    cmdServer->TestThroughput();
}
