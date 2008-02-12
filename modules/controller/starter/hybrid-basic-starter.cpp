#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "asim/provides/starter.h"
#include "asim/rrr/rrr_service_ids.h"
#include "asim/provides/rrr.h"

#define SERVICE_ID  STARTER_SERVICE_ID

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
    HASIM_MODULE p)
{
    parent = p;
}

// uninit
void
STARTER_CLASS::Uninit()
{
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
    if (arg0 == 0)
    {
        cout << "starter: end of simulation." << endl;
        CallbackExit(arg1);
    }
    else
    {
        cerr << "starter: invalid methodID." << endl;
        CallbackExit(1);
    }

    return false;
}

// client: start hardware partition
void
STARTER_CLASS::StartHardware()
{
    // create message for RRR client
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(0);
    msg->AppendUINT32(0);   // value doesn't matter

    RRRClient->MakeRequestNoResponse(msg);
}

// client: stop hardware partition
void
STARTER_CLASS::StopHardware()
{
    // create message for RRR client
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(1);
    msg->AppendUINT32(0);   // value doesn't matter

    RRRClient->MakeRequestNoResponse(msg);
}

