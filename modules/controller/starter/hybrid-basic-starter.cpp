#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "asim/provides/starter.h"
#include "asim/rrr/rrr_service_ids.h"
#include "asim/provides/rrr.h"

#define SERVICE_ID  STARTER_SERVICE_ID

using namespace std;

// constructor
STARTER_CLASS::STARTER_CLASS()
{
}

// destructor
STARTER_CLASS::~STARTER_CLASS()
{
}

// start hardware partition
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

// stop hardware partition
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

