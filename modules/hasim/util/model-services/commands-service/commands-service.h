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

#ifndef _COMMANDS_CONTROLLER_
#define _COMMANDS_CONTROLLER_

#include <stdio.h>

#include "asim/syntax.h"
#include "asim/trace.h"
#include "asim/regexobj.h"

#include "platforms-module.h"
#include "asim/provides/rrr.h"
#include "asim/provides/command_switches.h"
#include "asim/provides/hasim_common.h"
#include "asim/provides/stats_service.h"
#include "asim/provides/debug_scan_service.h"

#include "asim/rrr/client_stub_COMMANDS.h"

// Relay commands to HW.

typedef class COMMANDS_SERVER_CLASS* COMMANDS_SERVER;


class STOP_CYCLE_SWITCH_CLASS : public COMMAND_SWITCH_INT_CLASS
{
  private:
    int stopCycle;

  public:
    STOP_CYCLE_SWITCH_CLASS();
    ~STOP_CYCLE_SWITCH_CLASS();
    
    void ProcessSwitchInt(int arg);
    void ShowSwitch(std::ostream& ostr, const string& prefix);
    
    int StopCycle() { return stopCycle; }
};

class MESSAGE_INTERVAL_SWITCH_CLASS : public COMMAND_SWITCH_INT_CLASS
{
  private:
    int messageInterval;

  public:
    MESSAGE_INTERVAL_SWITCH_CLASS();
    ~MESSAGE_INTERVAL_SWITCH_CLASS();
    
    void ProcessSwitchInt(int arg);
    void ShowSwitch(std::ostream& ostr, const string& prefix);
    
    int ProgressMsgInterval() { return messageInterval; }
};

typedef class HW_THREAD_HEARTBEAT_CLASS* HW_THREAD_HEARTBEAT;

//
// HW_THREAD_HEARTBEAT_CLASS --
//    Maintain heartbeat information for a single hardware thread.
//
class HW_THREAD_HEARTBEAT_CLASS
{
  public:
    HW_THREAD_HEARTBEAT_CLASS();
    ~HW_THREAD_HEARTBEAT_CLASS() {};

    void Init(MESSAGE_INTERVAL_SWITCH_CLASS* mis);

    void Heartbeat(UINT32 hwThreadId,
                   UINT64 fpga_cycles,
                   UINT32 model_cycles,
                   UINT32 instr_commits);

    void ProgressStats(UINT32 hwThreadId);
    UINT64 GetInstrCommits() const { return instrCommits; };
    UINT64 GetInstrStartCommits() const { return modelStartInstrs; };
    UINT64 GetModelCycles() const { return modelCycles; };
    UINT64 GetModelStartCycle() const { return modelStartCycle; };
    UINT64 GetFPGACycles() const { return fpgaLastCycle - fpgaStartCycle; };
    double GetModelIPS() const;

  private:
    // These let us compute FMR starting after the first heartbeat is received.
    // We can thus eliminate model start-up cycles from FMR.
    UINT64 fpgaStartCycle;
    UINT64 fpgaLastCycle;
    UINT64 modelStartCycle;
    UINT64 modelStartInstrs;
    MESSAGE_INTERVAL_SWITCH_CLASS* messageIntervalSwitch;

    double latestFMR;
    struct timeval heartbeatStartTime;
    struct timeval heartbeatLastTime;

    // Keep running totals of model cycles and committed instructions since
    // heartbeat provides total since last beat.
    UINT64 instrCommits;
    UINT64 modelCycles;

    UINT64 nextProgressMsgCycle;
};


class COMMANDS_SERVER_CLASS: public RRR_SERVER_CLASS,
                             public PLATFORMS_MODULE_CLASS,
                             public TRACEABLE_CLASS,
                             public STATS_EMITTER_CLASS
{
  private:
    // self-instantiation
    static COMMANDS_SERVER_CLASS instance;
        
    // stubs
    COMMANDS_CLIENT_STUB clientStub;
    RRR_SERVER_STUB serverStub;

    // Command switch for stop cycle
    STOP_CYCLE_SWITCH_CLASS stopCycleSwitch;
    
    // Command switch for message interval
    MESSAGE_INTERVAL_SWITCH_CLASS messageIntervalSwitch;

    HW_THREAD_HEARTBEAT_CLASS* hwThreadHeartbeat; // Dynamic arrays of heartbeats
    
    // Number of active HW threads.
    UINT32 numThreads;

    // Cycle when statistics were last scanned
    UINT64 lastStatsScanCycle;
    // Mask of bits to monitor for triggering statistics scan out from HW
    UINT64 statsScanMask;

    // Deadlock detection
    UINT32 noChangeBeats;
    bool running;
    
    // wall-clock time tracking
    struct timeval startTime;
    struct timeval endTime;

    // Scan data buffering
    DEBUG_SCAN_DATA_CLASS scanData;
    Regex scanParser;       // Parser for scan data size/name records
    UINT32 scanRunningIdx;  // Instance ID of the current "running" scan message

    void ScanDataNormal(UINT8 data, bool eom);
    void ScanDataRunning(UINT8 data, bool eom);

    void EndSimulation(int exitVal);

    // Virtual function for STATS_EMITTER_CLASS
    void EmitStats(ofstream &statsFile);

  public:
    COMMANDS_SERVER_CLASS();
    ~COMMANDS_SERVER_CLASS();

    // static methods
    static COMMANDS_SERVER GetInstance() { return &instance; }

    // Client methods
    void Run();
    void Pause();
    void Sync();
    void Scan();

    void SetNumHardwareThreads(UINT32 num);

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();

    // RRR service methods
    void EndSim(UINT8 success);

    void ModelHeartbeat(UINT32 hwThreadId,
                        UINT64 fpga_cycles,
                        UINT32 model_cycles,
                        UINT32 instr_commits);


    void FPGAHeartbeat(UINT8 dummy);

    void ScanData(UINT8 data, UINT8 flags);

    UINT8 Done(UINT8 dummy) { return dummy; }
};

// server stub
#include "asim/rrr/server_stub_COMMANDS.h"

// all functionalities of the event controller are completely implemented
// by the COMMANDS_SERVER
typedef COMMANDS_SERVER_CLASS* COMMANDS_SERVER;

#endif
