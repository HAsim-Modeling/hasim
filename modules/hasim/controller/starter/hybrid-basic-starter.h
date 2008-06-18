#ifndef _STARTER_
#define _STARTER_

#include <stdio.h>
#include <sys/time.h>

#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/rrr.h"
#include "asim/provides/starter.h"

// this module provides both client and service functionalities

typedef class STARTER_CLASS* STARTER;
class STARTER_CLASS: public RRR_SERVICE_CLASS,
                     public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static STARTER_CLASS instance;

    // wall-clock time tracking
    struct timeval startTime;

    void EndSimulation(int exitValue);

  public:
    STARTER_CLASS();
    ~STARTER_CLASS();

    // static methods
    static STARTER GetInstance() { return &instance; }

    // required RRR service methods
    void Init(PLATFORMS_MODULE);
    UMF_MESSAGE Request(UMF_MESSAGE);
    void Poll();

    // client methods
    void Run();
    void Pause();
    void Sync();
    void DumpStats();

  private:
    // These let us compute FMR starting after the first heartbeat is received.
    // We can thus eliminate model start-up cycles from FMR.
    UINT64 fpga_start_cycle;
    UINT64 model_start_cycle;

    UINT64 next_progress_msg_cycle;

    // Cycle when statistics were last scanned
    UINT64 last_stats_scan_cycle;
    // Mask of bits to monitor for triggering statistics scan out from HW
    UINT64 stats_scan_mask;
};

#endif
