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

#include "asim/provides/command_switches.h"
#include "asim/provides/stats_device.h"
#include "asim/provides/commands_service.h"
#include "asim/provides/debug_scan_device.h"

using namespace std;

// ===== service instantiation =====
COMMANDS_SERVER_CLASS COMMANDS_SERVER_CLASS::instance;

// constructor
COMMANDS_SERVER_CLASS::COMMANDS_SERVER_CLASS() :
    stopCycleSwitch(),
    messageIntervalSwitch(),
    lastStatsScanCycle(0),
    noChangeBeats(0),
    running(false)
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
    parent = p;
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
    // Set simulation end cycle
    clientStub->SetEndModelCycle(stopCycleSwitch.StopCycle());

    // Tell the stats device to setup itself. We wait until this
    // point to do it to ensure that the RRR stack is up.
    STATS_DEVICE_CLASS::GetInstance()->SetupStats();

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

        if (noChangeBeats == 100)
        {
            cerr << "commands relay: model deadlock!" << endl;

            DEBUG_SCAN_DEVICE_SERVER_CLASS::GetInstance()->Scan();
            EndSimulation(1);
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
    hwThreadHeartbeat[hwThreadId].Heartbeat(hwThreadId, fpga_cycles, model_cycles, instr_commits);
    
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

    // stop the simulation
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

    cout << "        syncing... ";
    //Sync();
    cout << "sunk." << endl;

    cout << "        starting stats dump... ";
    STATS_DEVICE_SERVER_CLASS::GetInstance()->DumpStats();
    STATS_DEVICE_SERVER_CLASS::GetInstance()->EmitFile();
    cout << "done." << endl;

    CallbackExit(exitValue);
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

    statsFile << "\"Simulator: Wall time (sec.)\",SIM_WALL_TIME_SEC,"
              << elapsed
              << endl;
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
    // Is model broken?
    //
    if (instr_commits == 0 && instrCommits > 0)
    {
        ASIMERROR("No instructions committed for entire heartbeat interval (" <<
                  model_cycles << " cycles)");
    }
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
    stopCycle = 0;
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
