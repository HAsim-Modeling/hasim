#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>

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
RRRTEST_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    parent = p;
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
RRRTEST_SERVER_CLASS::F2HOneWayMsg(
    UINT64 payload)
{
    // do nothing
//    static int msg_count = 0;
//    cout << "server: received one-way msg [" << msg_count++ << "]\t = " << hex << payload << dec << endl << flush;
}

// F2HTwoWayMsg
UINT64
RRRTEST_SERVER_CLASS::F2HTwoWayMsg(
    UINT64 payload)
{
    // return the bitwise-inverted payload
    return ~payload;
}
