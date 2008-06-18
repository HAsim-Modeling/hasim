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

// TEMPORARY: cheat and assign client method IDs
#define METHOD_ID_RUN       0
#define METHOD_ID_PAUSE     1
#define METHOD_ID_SYNC      2
#define METHOD_ID_DUMPSTATS 3

#define METHOD_ID_ENDSIM    0
#define METHOD_ID_HEARTBEAT 1

using namespace std;

// ===== service instantiation =====
STARTER_CLASS STARTER_CLASS::instance;

// constructor
STARTER_CLASS::STARTER_CLASS() :
    fpga_start_cycle(0),
    model_start_cycle(0),
    next_progress_msg_cycle(0)
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
    UINT64 model_cycles;
    UINT64 fpga_cycles;

    switch (req->GetMethodID())
    {
      case METHOD_ID_ENDSIM:
        success  = req->ExtractUINT32();
        req->Delete();

        if (success == 1)
        {
            cout << "starter: simulation completed successfully." << endl;
        }
        else
        {
            cout << "starter: simulation completed with errors." << endl;
        }

        EndSimulation(success == 0);
        break;

      case METHOD_ID_HEARTBEAT:
        model_cycles = req->ExtractUINT64();
        fpga_cycles = req->ExtractUINT64();
        req->Delete();

        if (fpga_start_cycle == 0)
        {
            fpga_start_cycle = fpga_cycles;
            model_start_cycle = model_cycles;
        }

        if (next_progress_msg_cycle && (model_cycles >= next_progress_msg_cycle))
        {
            next_progress_msg_cycle += globalArgs->ProgressMsgInterval();

            cout << "[" << std::setw(13) << fpga_cycles
                 << "]: controller: model cycles completed: "
                 << std::setw(10) << model_cycles;

            if ((fpga_cycles - fpga_start_cycle) != 0)
            {
                cout << " (FMR="
                     << IoFormat::fmt(".1f", 
                                      (double)(fpga_cycles - fpga_start_cycle) /
                                      (double)(model_cycles - model_start_cycle))
                     << ")";
            }

            cout << endl;
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


// client: run
void
STARTER_CLASS::Run()
{
    // create message for RRR client
    UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
    msg->SetLength(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_RUN);
    msg->AppendUINT32(0);   // value doesn't matter

    // log start time
    gettimeofday(&startTime, NULL);

    RRRClient->MakeRequestNoResponse(msg);
}

// client: pause
void
STARTER_CLASS::Pause()
{
    // create message for RRR client
    UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
    msg->SetLength(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_PAUSE);
    msg->AppendUINT32(0);   // value doesn't matter

    RRRClient->MakeRequestNoResponse(msg);
}

// client: sync
void
STARTER_CLASS::Sync()
{
    // create message for RRR client
    UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
    msg->SetLength(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_SYNC);
    msg->AppendUINT32(0);   // value doesn't matter

    RRRClient->MakeRequestNoResponse(msg);
}

// client: dump stats
void
STARTER_CLASS::DumpStats()
{
    // create message for RRR client
    UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
    msg->SetLength(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_DUMPSTATS);
    msg->AppendUINT32(0);   // value doesn't matter

    // make blocking RRR call
    UMF_MESSAGE resp = RRRClient->MakeRequest(msg);

    // response simply means stats dump is over
    resp->Delete();
}
