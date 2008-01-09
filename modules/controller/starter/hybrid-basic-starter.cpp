#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "starter.h"
#include "rrr_service_ids.h"
#include "rrr.h"

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
    UINT32 payload = 0;

    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(0);
    msg->Append(4, (unsigned char *)&payload);

    RRRClient->MakeRequestNoResponse(msg);
}

// stop hardware partition
void
STARTER_CLASS::StopHardware()
{
    // create message for RRR client
    UINT32 payload = 0;

    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(1);
    msg->Append(4, (unsigned char *)&payload);

    RRRClient->MakeRequestNoResponse(msg);
}

