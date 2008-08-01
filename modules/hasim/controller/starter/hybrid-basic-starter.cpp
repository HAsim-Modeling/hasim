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

#define SERVICE_ID  STARTER_SERVICE_ID

// TEMPORARY: cheat and assign server method IDs
#define METHOD_ID_ENDSIM    0
#define METHOD_ID_HEARTBEAT 1

using namespace std;

// ===== service instantiation =====
STARTER_CLASS STARTER_CLASS::instance;

// constructor
STARTER_CLASS::STARTER_CLASS() :
    fpga_start_cycle(0),
    model_start_cycle(0),
    model_start_instrs(0),
    latest_fmr(-1),
    instr_commits(0),
    model_cycles(0),
    next_progress_msg_cycle(0),
    clientStub(this)
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);

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

    stats_scan_mask = 1 << (HASIM_STATS_SIZE - 2);
    last_stats_scan_cycle = 0;
}

// destructor
STARTER_CLASS::~STARTER_CLASS()
{
}

// init
void
STARTER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    parent = p;
    next_progress_msg_cycle = globalArgs->ProgressMsgInterval();
}

// poll
void
STARTER_CLASS::Poll()
{
}

// handle service request
UMF_MESSAGE
STARTER_CLASS::Request(
    UMF_MESSAGE req)
{
    // extract info
    UINT32 methodID = req->GetMethodID();
    UINT32 success;
    UINT64 fpga_cycles;

    switch (req->GetMethodID())
    {
      case METHOD_ID_ENDSIM:
        success  = req->ExtractUINT32();
        req->Delete();

        if (success == 1)
        {
            cout << "        starter: completed successfully. ";
        }
        else
        {
            cout << "        starter: completed with errors.  ";
        }

        EndSimulation(success == 0);
        break;

      case METHOD_ID_HEARTBEAT:
        instr_commits += req->ExtractUINT32();
        model_cycles += req->ExtractUINT32();
        fpga_cycles = req->ExtractUINT64();
        req->Delete();

        gettimeofday(&heartbeat_last_time, NULL);

        if (fpga_start_cycle == 0)
        {
            //
            // First heartbeat, which is some distance in to the run.  Record
            // first values seen so startup timing doesn't affect reported values.
            //
            fpga_start_cycle = fpga_cycles;
            model_start_cycle = model_cycles;
            model_start_instrs = instr_commits;
            heartbeat_start_time = heartbeat_last_time;
        }
        else
        {
            latest_fmr = (double)(fpga_cycles - fpga_start_cycle) /
                         (double)(model_cycles - model_start_cycle);
        }

        if (next_progress_msg_cycle && (model_cycles >= next_progress_msg_cycle))
        {
            next_progress_msg_cycle += globalArgs->ProgressMsgInterval();

            cout << "[" << std::dec << std::setw(13) << fpga_cycles
                 << "]: model cycles: "
                 << std::setw(10) << model_cycles;

            ProgressStats();
        }

        //
        // HW statistics counters are smaller than full counters to save
        // space.  Time to scan out intermediate statistics values before
        // they wrap around?
        //
        if (((model_cycles ^ last_stats_scan_cycle) & stats_scan_mask) != 0)
        {
            last_stats_scan_cycle = model_cycles;
            DumpStats();
        }

        //
        // Done?
        //
        if (globalArgs->StopCycle() && (model_cycles >= globalArgs->StopCycle()))
        {
            cout << "starter: simulation reached stop cycle." << endl;
            EndSimulation(0);
        }
        break;

      default:
        req->Delete();
        cerr << "starter: invalid methodID." << endl;
        CallbackExit(1);
        break;
    }

    // no response
    return NULL;
}


void
STARTER_CLASS::EndSimulation(int exitValue)
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

    if (model_cycles > 0)
    {
        ProgressStats();
    }

    cout << "         syncing... ";
    Sync();
    cout << "sunk." << endl;

    cout << "         starting stats dump... ";
    DumpStats();
    StatsEmitFile();
    cout << "done." << endl;

    printf("         elapsed (wall-clock) time = %.4f seconds.\n", elapsed);

    CallbackExit(exitValue);
}


void
STARTER_CLASS::ProgressStats()
{
    double sec = double(heartbeat_last_time.tv_sec) - double(heartbeat_start_time.tv_sec);
    double usec = double(heartbeat_last_time.tv_usec) - double(heartbeat_start_time.tv_usec);
    double heartbeat_run_time = sec + usec/1000000;

    cout << " (IPC=" << IoFormat::fmt(".2f", (double)instr_commits / (double)model_cycles);

    UINT64 commits = instr_commits - model_start_instrs;
    if ((heartbeat_run_time > 0) && (commits > 0))
    {
        cout << " / IPS="
             << IoFormat::fmt(".2f", (double)commits / heartbeat_run_time);
    }

    if (latest_fmr >= 0)
    {
        cout << " / FMR=" << IoFormat::fmt(".1f", latest_fmr);
    }

    cout << ")" << endl;
}


// client: run
void
STARTER_CLASS::Run()
{
    // log start time
    gettimeofday(&startTime, NULL);

    // call client stub
    clientStub.Run(0);
}

// client: pause
void
STARTER_CLASS::Pause()
{
    clientStub.Pause(0);
}

// client: sync
void
STARTER_CLASS::Sync()
{
    clientStub.Sync(0);
}

// client: dump stats
void
STARTER_CLASS::DumpStats()
{
    UINT32 ack = clientStub.DumpStats(0);
}
