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

//
// RRR service methods
//

// F2HOneWayMsg
void
RRRTEST_SERVER_CLASS::F2HOneWayMsg1(
    UINT64 payload)
{
//    static int msg_count = 0;
//    cout << "server: received one-way msg [" << msg_count++ << "]\t = " << hex << payload << dec << endl << flush;

    VERIFY(payload == 0x12345678abcdef2b,
           "F2HOneWayMsg1 unexpected payload: " << payload);
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
    VERIFY((payload0 == 1) &&
           (payload1 == 2) &&
           (payload2 == 3) &&
           (payload3 == 4) &&
           (payload4 == 5) &&
           (payload5 == 6) &&
           (payload6 == 7) &&
           (payload7 == 8),
           "F2HOneWayMsg8: Unexpected payload");
}

void
RRRTEST_SERVER_CLASS::F2HOneWayMsg16(
    UINT64 payload0,
    UINT64 payload1,
    UINT64 payload2,
    UINT64 payload3,
    UINT64 payload4,
    UINT64 payload5,
    UINT64 payload6,
    UINT64 payload7,
    UINT64 payload8,
    UINT64 payload9,
    UINT64 payload10,
    UINT64 payload11,
    UINT64 payload12,
    UINT64 payload13,
    UINT64 payload14,
    UINT64 payload15)
{
}


void
RRRTEST_SERVER_CLASS::F2HOneWayMsg32(
    UINT64 payload0,
    UINT64 payload1,
    UINT64 payload2,
    UINT64 payload3,
    UINT64 payload4,
    UINT64 payload5,
    UINT64 payload6,
    UINT64 payload7,
    UINT64 payload8,
    UINT64 payload9,
    UINT64 payload10,
    UINT64 payload11,
    UINT64 payload12,
    UINT64 payload13,
    UINT64 payload14,
    UINT64 payload15,
    UINT64 payload16,
    UINT64 payload17,
    UINT64 payload18,
    UINT64 payload19,
    UINT64 payload20,
    UINT64 payload21,
    UINT64 payload22,
    UINT64 payload23,
    UINT64 payload24,
    UINT64 payload25,
    UINT64 payload26,
    UINT64 payload27,
    UINT64 payload28,
    UINT64 payload29,
    UINT64 payload30,
    UINT64 payload31)
{
    VERIFY((payload7 == 8) &&
           (payload11 == 12) &&
           (payload23 == 24) &&
           (payload31 == 32),
           "F2HOneWayMsg32: Unexpected payload");
}


// F2HTwoWayMsg
UINT64
RRRTEST_SERVER_CLASS::F2HTwoWayMsg1(
    UINT64 payload)
{
    // return the bitwise-inverted payload
    return ~payload;
}


OUT_TYPE_F2HTwoWayMsg16
RRRTEST_SERVER_CLASS::F2HTwoWayMsg16(
    UINT64 payload0,
    UINT64 payload1,
    UINT64 payload2,
    UINT64 payload3,
    UINT64 payload4,
    UINT64 payload5,
    UINT64 payload6,
    UINT64 payload7,
    UINT64 payload8,
    UINT64 payload9,
    UINT64 payload10,
    UINT64 payload11,
    UINT64 payload12,
    UINT64 payload13,
    UINT64 payload14,
    UINT64 payload15)
{
    VERIFY((payload0 == 1) &&
           (payload11 == 12) &&
           (payload15 == 16),
           "F2HTwoWayMsg16: Unexpected payload");

    OUT_TYPE_F2HTwoWayMsg16 r;
    r.return0 = 1;
    r.return1 = 2;
    r.return2 = 3;
    r.return3 = 4;
    r.return4 = 5;
    r.return5 = 6;
    r.return6 = 7;
    r.return7 = 8;
    r.return8 = 9;
    r.return9 = 10;
    r.return10 = 11;
    r.return11 = 12;
    r.return12 = 13;
    r.return13 = 14;
    r.return14 = 15;
    r.return15 = 16;
}
