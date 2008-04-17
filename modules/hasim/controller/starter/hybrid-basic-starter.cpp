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

using namespace std;

// ===== service instantiation =====
STARTER_CLASS STARTER_CLASS::instance;

// constructor
STARTER_CLASS::STARTER_CLASS()
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
bool
STARTER_CLASS::Request(
    UINT32 arg0,
    UINT32 arg1,
    UINT32 arg2,
    UINT32 arg3,
    UINT32 *result)
{
    switch (arg0)
    {
        case METHOD_ID_ENDSIM:
            // for now, call statsdump directly from here
            cout << "starter: simulation complete, syncing... ";
            Sync();
            cout << "sunk." << endl;
            cout << "         starting stats dump... ";
            DumpStats();
            cout << "dump complete, exiting." << endl;
            CallbackExit(0);
            break;

        default:
            cerr << "starter: invalid methodID." << endl;
            CallbackExit(1);
            break;
    }

    return false;
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
