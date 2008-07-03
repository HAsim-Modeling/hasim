#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "asim/rrr/service_ids.h"
#include "asim/provides/bluespec_system.h"

#define SERVICE_ID  RRRTEST_SERVICE_ID

// TEMPORARY: cheat and assign client method IDs
#define METHOD_ID_F2HOneWayTest     0
#define METHOD_ID_F2HTwoWayTest     1
#define METHOD_ID_F2HTwoWayPipeTest 2

using namespace std;

// constructor
RRRTEST_CLIENT_CLASS::RRRTEST_CLIENT_CLASS(
    PLATFORMS_MODULE p) :
        PLATFORMS_MODULE_CLASS(p)
{
}

// destructor
RRRTEST_CLIENT_CLASS::~RRRTEST_CLIENT_CLASS()
{
}

// client: F2HOneWayTest
UINT64
RRRTEST_CLIENT_CLASS::F2HOneWayTest(
    UINT64 length)
{
    // create message for RRR client
    UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
    msg->SetLength(sizeof(UINT64));
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_F2HOneWayTest);
    msg->AppendUINT64(length);

    // make blocking RRR call
    UMF_MESSAGE resp = RRRClient->MakeRequest(msg);

    // test is over, return time taken
    UINT64 retval = resp->ExtractUINT64();
    resp->Delete();
    return retval;
}

// client: F2HTwoWayTest
UINT64
RRRTEST_CLIENT_CLASS::F2HTwoWayTest(
    UINT64 length)
{
    // create message for RRR client
    UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
    msg->SetLength(sizeof(UINT64));
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_F2HTwoWayTest);
    msg->AppendUINT64(length);

    // make blocking RRR call
    UMF_MESSAGE resp = RRRClient->MakeRequest(msg);

    // test is over, return time taken
    UINT64 retval = resp->ExtractUINT64();
    resp->Delete();
    return retval;
}

// client: F2HTwoWayPipeTest
UINT64
RRRTEST_CLIENT_CLASS::F2HTwoWayPipeTest(
    UINT64 length)
{
    // create message for RRR client
    UMF_MESSAGE msg = UMF_MESSAGE_CLASS::New();
    msg->SetLength(sizeof(UINT64));
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_F2HTwoWayPipeTest);
    msg->AppendUINT64(length);

    // make blocking RRR call
    UMF_MESSAGE resp = RRRClient->MakeRequest(msg);

    // test is over, return time taken
    UINT64 retval = resp->ExtractUINT64();
    resp->Delete();
    return retval;
}
