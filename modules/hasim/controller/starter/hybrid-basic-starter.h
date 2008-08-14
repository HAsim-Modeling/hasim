#ifndef _STARTER_
#define _STARTER_

#include <stdio.h>
#include <sys/time.h>

#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/rrr.h"
#include "asim/provides/starter.h"
#include "asim/rrr/client_stub_STARTER.h"

// this module provides both client and service functionalities

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
    void ProgressStats();

  public:
    STARTER_SERVER_CLASS();
    ~STARTER_SERVER_CLASS();

    // static methods
    static STARTER_SERVER GetInstance() { return &instance; }

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();
    void Poll();

    //
    // RRR service methods
    //
    void EndSim(UINT8 success);
    void Heartbeat(UINT64 fpga_cycles, UINT32 model_cycles, UINT32 instr_commits);

    // client methods
    void Run();
    void Pause();
    void Sync();
    void DumpStats();

  private:
    // These let us compute FMR starting after the first heartbeat is received.
    // We can thus eliminate model start-up cycles from FMR.
    UINT64 fpgaStartCycle;
    UINT64 modelStartCycle;
    UINT64 modelStartInstrs;

    double latestFMR;
    struct timeval heartbeatStartTime;
    struct timeval heartbeatLastTime;

    // Keep running totals of model cycles and committed instructions since
    // heartbeat provides total since last beat.
    UINT64 instrCommits;
    UINT64 modelCycles;

    UINT64 nextProgressMsgCycle;

    // Cycle when statistics were last scanned
    UINT64 lastStatsScanCycle;
    // Mask of bits to monitor for triggering statistics scan out from HW
    UINT64 statsScanMask;
};

// server stub
#include "asim/rrr/server_stub_STARTER.h"

// our STARTER_SERVER class is itself the main STARTER class
typedef STARTER_SERVER_CLASS STARTER_CLASS;

#endif
