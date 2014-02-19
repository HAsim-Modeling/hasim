//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

#ifndef _COMMANDS_CONTROLLER_
#define _COMMANDS_CONTROLLER_

#include <stdio.h>
#include <string>
#include <list>
#include <iostream>
#include <mutex>
#include <condition_variable>
#include "tbb/atomic.h"

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

using namespace std;

// Relay commands to HW.

typedef class COMMANDS_SERVER_CLASS* COMMANDS_SERVER;


class STOP_CYCLE_SWITCH_CLASS : public COMMAND_SWITCH_UINT64_CLASS
{
  private:
    UINT64 stopCycle;

  public:
    STOP_CYCLE_SWITCH_CLASS();
    ~STOP_CYCLE_SWITCH_CLASS() {};
    
    void ProcessSwitchInt(UINT64 arg) { stopCycle = arg; };
    void ShowSwitch(std::ostream& ostr, const string& prefix);
    
    UINT64 StopCycle() { return stopCycle; }
};

class MESSAGE_INTERVAL_SWITCH_CLASS : public COMMAND_SWITCH_UINT64_CLASS
{
  private:
    UINT64 messageInterval;

  public:
    MESSAGE_INTERVAL_SWITCH_CLASS();
    ~MESSAGE_INTERVAL_SWITCH_CLASS() {};
    
    void ProcessSwitchInt(UINT64 arg) { messageInterval = arg; };
    void ShowSwitch(std::ostream& ostr, const string& prefix);
    
    UINT64 ProgressMsgInterval() { return messageInterval; }
};

class COMMANDS_TP_TEST_SWITCH_CLASS : public COMMAND_SWITCH_UINT64_CLASS
{
  private:
    UINT64 trigger;

  public:
    COMMANDS_TP_TEST_SWITCH_CLASS();
    ~COMMANDS_TP_TEST_SWITCH_CLASS() {};
    
    void ProcessSwitchInt(UINT64 arg) { trigger = arg; };
    void ShowSwitch(std::ostream& ostr, const string& prefix);
    
    UINT64 Trigger() { return trigger; }
};

typedef class HW_THREAD_HEARTBEAT_CLASS* HW_THREAD_HEARTBEAT;

//
// HW_THREAD_HEARTBEAT_CLASS --
//    Maintain heartbeat information for a single hardware thread.
//
class HW_THREAD_HEARTBEAT_CLASS : public STATS_EMITTER_CLASS
{
  public:
    HW_THREAD_HEARTBEAT_CLASS();
    ~HW_THREAD_HEARTBEAT_CLASS() {};

    void Init(UINT32 id, MESSAGE_INTERVAL_SWITCH_CLASS* mis);

    void Heartbeat(UINT64 fpga_cycles,
                   UINT32 model_cycles,
                   UINT32 instr_commits);

    void ProgressStats(UINT32 hwThreadId);
    UINT64 GetInstrCommits() const { return instrCommits; };
    UINT64 GetInstrStartCommits() const { return modelStartInstrs; };
    UINT64 GetModelCycles() const { return modelCycles; };
    UINT64 GetModelStartCycle() const { return modelStartCycle; };
    UINT64 GetFPGACycles() const { return fpgaLastCycle - fpgaStartCycle; };
    double GetModelIPS() const;

    // Virtual function for STATS_EMITTER_CLASS
    void EmitStats(ofstream &statsFile);
    void ResetStats();

  private:
    UINT32 hwThreadId;

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

    std::mutex statsMutex;
};


class COMMANDS_SERVER_CLASS: public RRR_SERVER_CLASS,
                             public PLATFORMS_MODULE_CLASS,
                             public TRACEABLE_CLASS,
                             public STATS_EMITTER_CLASS,
                             public DEBUG_SCANNER_CLASS
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

    // Throughput test trigger
    COMMANDS_TP_TEST_SWITCH_CLASS tpTestSwitch;

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
    volatile bool healthy;
    
    // wall-clock time tracking
    struct timeval startTime;

    // Names of local command service nodes in the connected ring's order
    list<string> localControllerNames;

    // Scan data buffering
    DEBUG_SCAN_DATA_CLASS scanData;
    Regex scanParser;       // Parser for scan data size/name records
    UINT32 scanRunningIdx;  // Instance ID of the current "running" scan message
    bool onlyCollectNames;
    std::ostream* scanStream;

    // Throughput testing state
    list<string>::const_iterator tpControllerName;

    void ScanDataNormal(UINT8 data, bool eom);
    void ScanDataRunning(UINT8 data, bool eom);

    void EndSimulation(int exitVal);

    // Virtual function for STATS_EMITTER_CLASS
    void EmitStats(ofstream &statsFile);
    void ResetStats();

    static volatile bool scanning;
    static std::mutex scanDoneMutex;
    static std::condition_variable scanDoneCond;
    static bool scanDoneReceived;

    // We use this variable to guard against multiple calls to Uninit();
    // It is possible that we should make this variable static, since
    // there is only on class instance.

    class tbb::atomic<bool> uninitialized;

public:
    COMMANDS_SERVER_CLASS();
    ~COMMANDS_SERVER_CLASS();

    // static methods
    static COMMANDS_SERVER GetInstance() { return &instance; }

    // Client methods
    void Run();
    void Pause();
    void Resume();
    void Sync();
    void DebugScan(std::ostream& ofile = cout);
    void TestThroughput();

    void SetNumHardwareThreads(UINT32 num);

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();

    // RRR service methods
    void EndSim(UINT8 success);

    void ModelHeartbeat(UINT32 hwThreadId,
                        UINT64 fpga_cycles,
                        UINT32 model_cycles,
                        UINT32 instr_commits);


    void FPGAHeartbeat(UINT8 dummy);

    void ScanData(UINT8 data, UINT8 flags);
    void ThroughputData(UINT16 data, UINT8 flags);
    void ScanDone(UINT8 test);
};

// server stub
#include "asim/rrr/server_stub_COMMANDS.h"

// all functionalities of the event controller are completely implemented
// by the COMMANDS_SERVER
typedef COMMANDS_SERVER_CLASS* COMMANDS_SERVER;

#endif
