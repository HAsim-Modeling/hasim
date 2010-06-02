//
// Copyright (C) 2010 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

#include "asim/syntax.h"
#include "asim/rrr/service_ids.h"
#include "asim/provides/hybrid_application.h"

using namespace std;

// ===== service instantiation =====
CHANTEST_SERVER_CLASS CHANTEST_SERVER_CLASS::instance;

// constructor
CHANTEST_SERVER_CLASS::CHANTEST_SERVER_CLASS()
    : f2hRecvMsgs(0),
      f2hRecvErrors(0),
      h2fRecvErrors(0),
      h2fRecvBitErrors(0)
{
    // instantiate stub
    serverStub = new CHANTEST_SERVER_STUB_CLASS(this);
}

// destructor
CHANTEST_SERVER_CLASS::~CHANTEST_SERVER_CLASS()
{
    Cleanup();
}

// init
void
CHANTEST_SERVER_CLASS::Init(PLATFORMS_MODULE p)
{
    PLATFORMS_MODULE_CLASS::Init(p);
}

// uninit
void
CHANTEST_SERVER_CLASS::Uninit()
{
    Cleanup();
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
CHANTEST_SERVER_CLASS::Cleanup()
{
    delete serverStub;
}

//
// RRR service methods
//

void
CHANTEST_SERVER_CLASS::F2HOneWayMsg8(
    UINT64 payload0,
    UINT64 payload1,
    UINT64 payload2,
    UINT64 payload3,
    UINT64 payload4,
    UINT64 payload5,
    UINT64 payload6,
    UINT64 payload7)
{
    f2hRecvMsgs += 1;

    bool err = false;
    if (payload0 != ~payload4) err = true;
    if (payload1 != ~payload5) err = true;
    if (payload2 != ~payload6) err = true;
    if (payload3 != ~payload7) err = true;
    if (err)
    {
        f2hRecvErrors += 1;
        cout << "F2H Error" << endl;
    }
}

void
CHANTEST_SERVER_CLASS::H2FNoteError(
    UINT32 numBitsFlipped,
    UINT32 chunkIdx,
    UINT64 payload0,
    UINT64 payload1,
    UINT64 payload2,
    UINT64 payload3,
    UINT64 payload4,
    UINT64 payload5,
    UINT64 payload6,
    UINT64 payload7)
{
    h2fRecvErrors += 1;
    h2fRecvBitErrors += numBitsFlipped;
    cout << "H2F Error (" << numBitsFlipped << " bits, idx " << chunkIdx << ")" << endl;
    cout.fill('0');
    cout << hex;

    cout << "  0x" << std::setw(16) << payload0
         << "  0x" << std::setw(16) << payload4
         << "  0x" << std::setw(16) << ~(payload0 ^ payload4)
         << endl;

    cout << "  0x" << std::setw(16) << payload1
         << "  0x" << std::setw(16) << payload5
         << "  0x" << std::setw(16) << ~(payload1 ^ payload5)
         << endl;

    cout << "  0x" << std::setw(16) << payload2
         << "  0x" << std::setw(16) << payload6
         << "  0x" << std::setw(16) << ~(payload2 ^ payload6)
         << endl;

    cout << "  0x" << std::setw(16) << payload3
         << "  0x" << std::setw(16) << payload7
         << "  0x" << std::setw(16) << ~(payload3 ^ payload7)
         << endl;

    cout << dec;
    cout.fill(' ');
}

