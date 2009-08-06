//
// Copyright (C) 2009 Intel Corporation
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

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "asim/rrr/service_ids.h"
#include "asim/provides/bluespec_system.h"
#include "asim/provides/command_switches.h"
#include "asim/ioformat.h"

using namespace std;

// constructor
BLUESPEC_SYSTEM_CLASS::BLUESPEC_SYSTEM_CLASS(
    LLPI llpi):
        PLATFORMS_MODULE_CLASS(NULL),
        sharedMemoryDevice(this, llpi)
{
    // instantiate client stub
    clientStub = new SHMEM_TEST_CLIENT_STUB_CLASS(this);
}

// destructor
BLUESPEC_SYSTEM_CLASS::~BLUESPEC_SYSTEM_CLASS()
{
    delete clientStub;
}

// main
void
BLUESPEC_SYSTEM_CLASS::Main()
{
    UINT64 cycles;
    UINT64 test_length  = 100000; // FIXME: take this from a dynamic parameter
    UINT64 fpga_freq    = 75;    // FIXME: take this from a dynamic parameter
    UINT64 payload_bits = 64;    // FIXME: no idea how
    UINT32 burst_length = 512;

    double datasize = payload_bits / 8;
    double latency_c;
    double latency;
    double bandwidth;

    //
    // setup shared memory
    //
    UINT64* mem = (UINT64 *)sharedMemoryDevice.Allocate();

    // print banner and test parameters
    cout << "\n";
    cout << "Test Parameters\n";
    cout << "---------------\n";
    cout << "Number of Transactions  = " << dec << test_length << endl;
    cout << "Payload Width (in bits) = " << payload_bits << endl;
    cout << "FPGA Clock Frequency    = " << fpga_freq << endl;

    //
    // perform one-way test
    //
    UINT64 last_seen_data = mem[burst_length - 1];

    for (int test = 0; test < test_length; test++)
    {
        cycles = clientStub->OneWayTest(burst_length);

        // sleep(2);

        // wait for memory flag to change
        while (last_seen_data == mem[burst_length - 1]);
        last_seen_data = mem[burst_length - 1];

        /*
        for (int i = 0; i < burst_length; i++)
        {
            cout << hex << mem[i] << dec << endl;
        }
        cout << endl;
        */
        // cout << last_seen_data << endl;
    }

    cycles = last_seen_data;

    // compute results
    latency_c = double(cycles) / test_length;
    latency   = latency_c / fpga_freq;
    bandwidth = (datasize * burst_length) / latency;
        
    // report results
    cout << "\n";
    cout << "One-Way Test Results\n";
    cout << "--------------------\n";
    cout << "FPGA cycles       = " << cycles << endl;
    cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
         << "                  = " << latency << " usec\n";
    cout << "Average Bandwidth = " << bandwidth << " MB/s\n";

    // done!
    cout << "\n";
}
