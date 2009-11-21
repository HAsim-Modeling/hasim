#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "asim/syntax.h"
#include "asim/rrr/service_ids.h"
#include "asim/provides/hybrid_application.h"

using namespace std;

// ===== service instantiation =====
RRRTEST_SERVER_CLASS RRRTEST_SERVER_CLASS::instance;

// constructor
RRRTEST_SERVER_CLASS::RRRTEST_SERVER_CLASS()
{
    // instantiate stub
    serverStub = new RRRTEST_SERVER_STUB_CLASS(this);
}

// destructor
RRRTEST_SERVER_CLASS::~RRRTEST_SERVER_CLASS()
{
    Cleanup();
}

// init
void
RRRTEST_SERVER_CLASS::Init(PLATFORMS_MODULE p)
{
    PLATFORMS_MODULE_CLASS::Init(p);
}

// uninit
void
RRRTEST_SERVER_CLASS::Uninit()
{
    Cleanup();
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
RRRTEST_SERVER_CLASS::Cleanup()
{
    delete serverStub;
}

// poll
void
RRRTEST_SERVER_CLASS::Poll()
{
}

//
// RRR service methods
//

// F2HOneWayMsg
void
RRRTEST_SERVER_CLASS::F2HOneWayMsg1(
    UINT64 payload)
{
    // do nothing
//    static int msg_count = 0;
//    cout << "server: received one-way msg [" << msg_count++ << "]\t = " << hex << payload << dec << endl << flush;
}

void
RRRTEST_SERVER_CLASS::F2HOneWayMsg8(
    UINT64 payload0,
    UINT64 payload1,
    UINT64 payload2,
    UINT64 payload3,
    UINT64 payload4,
    UINT64 payload5,
    UINT64 payload6,
    UINT64 payload7)
{
}

// F2HTwoWayMsg
UINT64
RRRTEST_SERVER_CLASS::F2HTwoWayMsg(
    UINT64 payload)
{
    // return the bitwise-inverted payload
    return ~payload;
}
