#ifndef _STARTER_
#define _STARTER_

#include <stdio.h>
#include <sys/time.h>

#include "command-switches.h"

#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/rrr.h"
#include "asim/provides/funcp_base_types.h"
#include "asim/provides/starter.h"
#include "asim/rrr/client_stub_STARTER.h"

// this module provides both client and service functionalities


//
// CONTEXT_HEARTBEAT_CLASS --
//    Maintain heartbeat information for a single context.
//

class STOP_CYCLE_SWITCH_CLASS : public COMMAND_SWITCH_INT_CLASS
{
  private:
    int stopCycle;

  public:
    STOP_CYCLE_SWITCH_CLASS();
    ~STOP_CYCLE_SWITCH_CLASS();
    
    void ProcessSwitchInt(int arg);
    bool ShowSwitch(char *buff);
    
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
    bool ShowSwitch(char *buff);
    
    int ProgressMsgInterval() { return messageInterval; }
};

typedef class CONTEXT_HEARTBEAT_CLASS* CONTEXT_HEARTBEAT;

class CONTEXT_HEARTBEAT_CLASS
{
  public:
    CONTEXT_HEARTBEAT_CLASS();
    ~CONTEXT_HEARTBEAT_CLASS() {};

    void Init(MESSAGE_INTERVAL_SWITCH_CLASS* mis);

    void Heartbeat(CONTEXT_ID ctxId,
                   UINT64 fpga_cycles,
                   UINT32 model_cycles,
                   UINT32 instr_commits);

    void ProgressStats(CONTEXT_ID ctxId);

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



//
// STARTER_SERVER_CLASS --
//
//

typedef class STARTER_SERVER_CLASS* STARTER_SERVER;

class STARTER_SERVER_CLASS: public RRR_SERVER_CLASS,
                            public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static STARTER_SERVER_CLASS instance;

    // stubs
    STARTER_CLIENT_STUB clientStub;
    RRR_SERVER_STUB     serverStub;

    // wall-clock time tracking
    struct timeval startTime;

    void EndSimulation(int exitValue);

  public:
    STARTER_SERVER_CLASS();
    ~STARTER_SERVER_CLASS();

    // static methods
    static STARTER_SERVER GetInstance() { return &instance; }

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();

    //
    // RRR service methods
    //
    void EndSim(UINT8 success);
    void Heartbeat(UINT8 fpgaClockBeat,
                   CONTEXT_ID ctxId,
                   UINT64 fpga_cycles,
                   UINT32 model_cycles,
                   UINT32 instr_commits);

    // client methods
    void Run();
    void Pause();
    void Sync();
    void DumpStats();
    void DebugScan();
    void EnableContext(CONTEXT_ID ctx_id);
    void DisableContext(CONTEXT_ID ctx_id);

  private:
    // Command switch for stop cycle
    STOP_CYCLE_SWITCH_CLASS stopCycleSwitch;
    
    // Command switch for message interval
    MESSAGE_INTERVAL_SWITCH_CLASS messageIntervalSwitch;

    CONTEXT_HEARTBEAT_CLASS ctxHeartbeat[NUM_CONTEXTS];

    // Cycle when statistics were last scanned
    UINT64 lastStatsScanCycle;
    // Mask of bits to monitor for triggering statistics scan out from HW
    UINT64 statsScanMask;

    // Deadlock detection
    UINT64 lastFPGAClockModelCycles;
    UINT64 lastFPGAClockCommits;
    UINT32 noChangeBeats;
};


// server stub
#include "asim/rrr/server_stub_STARTER.h"

// our STARTER_SERVER class is itself the main STARTER class
typedef STARTER_SERVER_CLASS STARTER_CLASS;

#endif
