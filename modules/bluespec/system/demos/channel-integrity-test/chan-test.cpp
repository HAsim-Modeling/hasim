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

//
// @file chan-test.cpp
// @brief Channel integrity test
//
// @author Michael Adler
//

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "asim/syntax.h"
#include "asim/ioformat.h"
#include "asim/rrr/service_ids.h"
#include "asim/provides/hybrid_application.h"
#include "asim/provides/clocks_device.h"

using namespace std;

// constructor
HYBRID_APPLICATION_CLASS::HYBRID_APPLICATION_CLASS(
    VIRTUAL_PLATFORM vp)
{
    // instantiate client stub
    clientStub = new CHANTEST_CLIENT_STUB_CLASS(NULL);
    server = CHANTEST_SERVER_CLASS::GetInstance();
}

// destructor
HYBRID_APPLICATION_CLASS::~HYBRID_APPLICATION_CLASS()
{
    delete clientStub;
}

void
HYBRID_APPLICATION_CLASS::Init()
{
}

void
HYBRID_APPLICATION_CLASS::SendH2FMsg()
{
    //
    // Generate 4 random 64 bit values
    //
    UINT64 v[4];
    for (int i = 0; i < 4; i++)
    {
        UINT64 r0 = random();
        UINT64 r1 = random();
        UINT64 r2 = random();
        v[i] = (r2 << 38) ^ (r1 << 20) ^ r0;
    }

    // Send the 4 values and their complements
    clientStub->H2FOneWayMsg8(v[0], v[1], v[2], v[3],
                              ~v[0], ~v[1], ~v[2], ~v[3]);
}


// main
void
HYBRID_APPLICATION_CLASS::Main()
{
    UINT64 test_length  = testIterSwitch.Value();

    // print banner and test parameters
    cout << endl
         << "Test Parameters" << endl
         << "---------------" << endl
         << "Number of Transactions  = " << dec << test_length << endl;

    cout << endl
         << "Running..." << endl
         << "---------------" << endl;

    //
    // Have the FPGA start sending test data to the host.
    //
    clientStub->F2HStartOneWayTest(test_length);

    //
    // Send data from the host to the FPGA.  This may run in parallel with
    // the FPGA -> Host test started above.
    //
    for (UINT64 i = 0; i < test_length; i++)
    {
        SendH2FMsg();
    }

    // Get count of host -> FPGA errors
    OUT_TYPE_H2FGetStats stats = clientStub->H2FGetStats(0);
    cout << endl
         << "Test Results" << endl
         << "---------------" << endl;

    cout << "Host -> FPGA total packets:      " << stats.recvPackets << endl
         << "Host -> FPGA error packets:      " << stats.packetErrors << endl
         << "Host -> FPGA total bits flipped: " << stats.totalBitsFlipped << endl;

    cout << "FPGA -> Host total packets:      " << server->GetF2HRecvMsgCnt() << endl
         << "FPGA -> Host error packets:      " << server->GetF2HRecvErrCnt() << endl;

    // done!
    cout << endl
         << "Tests Complete.\n";
}
