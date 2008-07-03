#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "asim/rrr/service_ids.h"
#include "asim/provides/bluespec_system.h"

#define SERVICE_ID  RRRTEST_SERVICE_ID

// TEMPORARY: cheat and assign method IDs
#define METHOD_ID_F2HOneWayMsg 0
#define METHOD_ID_F2HTwoWayMsg 1

using namespace std;

// ===== service instantiation =====
RRRTEST_SERVER_CLASS RRRTEST_SERVER_CLASS::instance;

// constructor
RRRTEST_SERVER_CLASS::RRRTEST_SERVER_CLASS()
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
}

// destructor
RRRTEST_SERVER_CLASS::~RRRTEST_SERVER_CLASS()
{
}

// init
void
RRRTEST_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    parent = p;
}

// poll
void
RRRTEST_SERVER_CLASS::Poll()
{
}

// handle service request
UMF_MESSAGE
RRRTEST_SERVER_CLASS::Request(
    UMF_MESSAGE req)
{
    UMF_MESSAGE response = NULL;
    UINT64 payload;

    switch (req->GetMethodID())
    {

      case METHOD_ID_F2HOneWayMsg:

        payload = req->ExtractUINT64();
        req->Delete();
        break;

      case METHOD_ID_F2HTwoWayMsg:

        payload = req->ExtractUINT64();
        req->Delete();

        response = UMF_MESSAGE_CLASS::New();
        response->SetLength(sizeof(UINT64));
        response->SetServiceID(SERVICE_ID);
        response->SetMethodID(METHOD_ID_F2HTwoWayMsg);
        response->AppendUINT64(payload);

        break;

      default:

        req->Delete();
        cerr << "rrrtest-server: invalid methodID." << endl;
        CallbackExit(1);
        break;

    }

    return response;
}
