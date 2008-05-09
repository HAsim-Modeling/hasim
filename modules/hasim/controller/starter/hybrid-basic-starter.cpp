#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "starter-common.h"
#include "asim/rrr/service_ids.h"

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
    model_start_cycle(0)
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
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

    switch (req->GetMethodID())
    {
        case METHOD_ID_ENDSIM:
            // for now, call statsdump directly from here
            UINT32 success  = req->ExtractUINT32();
            delete req;

            if (success == 1)
            {
                cout << "starter: simulation completed successfully." << endl;
                cout << "         syncing... ";
            }
            else
            {
                cout << "starter: simulation completed with errors." << endl;
                cout << "         syncing... ";
            }
            Sync();
            cout << "sunk." << endl;
            cout << "         starting stats dump... ";
            DumpStats();
            cout << "done." << endl;
            CallbackExit(0);
            break;

        case METHOD_ID_HEARTBEAT:
            UINT64 model_cycles = req->ExtractUINT64();
            UINT64 fpga_cycles = req->ExtractUINT64();
            delete req;

            if (fpga_start_cycle == 0)
            {
                fpga_start_cycle = fpga_cycles;
                model_start_cycle = model_cycles;

                fprintf(stdout, "[%12u]: controller: model cycles completed: %9u\n",
                        fpga_cycles,
                        model_cycles);
            }
            else
            {
                fprintf(stdout, "[%12u]: controller: model cycles completed: %9u (FMR=%.1f)\n",
                        fpga_cycles,
                        model_cycles,
                        (double)(fpga_cycles - fpga_start_cycle) /
                            (double)(model_cycles - model_start_cycle));
            }
            fflush(stdout);
            break;

        default:
            delete req;
            cerr << "starter: invalid methodID." << endl;
            CallbackExit(1);
            break;
    }

    // no response
    return NULL;
}

// client: run
void
STARTER_CLASS::Run()
{
    // create message for RRR client
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_RUN);
    msg->AppendUINT32(0);   // value doesn't matter

    RRRClient->MakeRequestNoResponse(msg);
}

// client: pause
void
STARTER_CLASS::Pause()
{
    // create message for RRR client
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(4);
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
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(4);
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
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_DUMPSTATS);
    msg->AppendUINT32(0);   // value doesn't matter

    // make blocking RRR call
    UMF_MESSAGE resp = RRRClient->MakeRequest(msg);

    // response simply means stats dump is over
    delete resp;
}
