/*****************************************************************************
 * rrrtest.cpp
 *
 * Copyright (C) 2008 Intel Corporation
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

//
// @file rrrtest.cpp
// @brief RRR Test System
//
// @author Angshuman Parashar
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
    clientStub = new RRRTEST_CLIENT_STUB_CLASS(NULL);
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

// main
void
HYBRID_APPLICATION_CLASS::Main()
{
    UINT64 cycles;
    UINT64 test_length  = testIterSwitch.Value();
#ifdef MODEL_CLOCK_FREQ
    UINT64 fpga_freq    = MODEL_CLOCK_FREQ;
#elif (defined(MODEL_CLOCK_MULTIPLIER) && defined(MODEL_CLOCK_DIVIDER) && defined(CRYSTAL_CLOCK_FREQ))
    // Assume 50MHz base clock
    UINT64 fpga_freq    = (CRYSTAL_CLOCK_FREQ * MODEL_CLOCK_MULTIPLIER) / MODEL_CLOCK_DIVIDER;
#else
    UINT64 fpga_freq    = 0;
#endif
    UINT64 payload_bytes = 8;    // FIXME: no idea how
    UINT64 header_bytes = UMF_CHUNK_BYTES;

    double datasize = payload_bytes + header_bytes;
    double latency_c = 0;
    double latency = 0;
    double bandwidth = 0;

    // print banner and test parameters
    cout << "\n";
    cout << "Test Parameters\n";
    cout << "---------------\n";
    cout << "Number of Transactions  = " << dec << test_length << endl;
    cout << "FPGA Clock Frequency    = " << fpga_freq << endl;

    cout << endl << "*** Bandwidth includes internal packet headers ***" << endl << endl;

    //
    // perform one-way test with short messages
    //
    cycles = clientStub->F2HOneWayTest(0, test_length);

    // compute results
    latency_c = double(cycles) / test_length;
    if (fpga_freq != 0)
    {
        latency   = latency_c / fpga_freq;
        bandwidth = datasize / latency;
    }
        
    // report results
    cout << "\n";
    cout << "One-Way Test Results (small messages)\n";
    cout << "--------------------\n";
    cout << "FPGA cycles       = " << cycles << endl;
    cout << "Payload Bytes     = " << payload_bytes << endl;
    cout << "Header Bytes      = " << header_bytes << endl;
    cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
         << "                  = " << latency << " usec\n";
    cout << "Average Bandwidth = " << bandwidth << " MB/s\n";

    //
    // perform one-way test with 64 byte messages
    //
    cycles = clientStub->F2HOneWayTest(1, test_length);

    // compute results
    UINT64 big_payload_bytes = payload_bytes * 8;
    latency_c = double(cycles) / test_length;
    if (fpga_freq != 0)
    {
        latency   = latency_c / fpga_freq;
        bandwidth = (big_payload_bytes + header_bytes) / latency;
    }
        
    // report results
    cout << "\n";
    cout << "One-Way Test Results (64 byte messages)\n";
    cout << "--------------------\n";
    cout << "FPGA cycles       = " << cycles << endl;
    cout << "Payload Bytes     = " << big_payload_bytes << endl;
    cout << "Header Bytes      = " << header_bytes << endl;
    cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
         << "                  = " << latency << " usec\n";
    cout << "Average Bandwidth = " << bandwidth << " MB/s\n";

    if (longTestsSwitch.Value() != 0)
    {
        //
        // perform one-way test with 128 byte messages
        //
        cycles = clientStub->F2HOneWayTest(2, test_length);

        // compute results
        big_payload_bytes = payload_bytes * 16;
        latency_c = double(cycles) / test_length;
        if (fpga_freq != 0)
        {
            latency   = latency_c / fpga_freq;
            bandwidth = (big_payload_bytes + header_bytes) / latency;
        }
        
        // report results
        cout << "\n";
        cout << "One-Way Test Results (128 byte messages)\n";
        cout << "--------------------\n";
        cout << "FPGA cycles       = " << cycles << endl;
        cout << "Payload Bytes     = " << big_payload_bytes << endl;
        cout << "Header Bytes      = " << header_bytes << endl;
        cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
             << "                  = " << latency << " usec\n";
        cout << "Average Bandwidth = " << bandwidth << " MB/s\n";


        //
        // perform one-way test with 256 byte messages
        //
        cycles = clientStub->F2HOneWayTest(3, test_length);

        // compute results
        big_payload_bytes = payload_bytes * 32;
        latency_c = double(cycles) / test_length;
        if (fpga_freq != 0)
        {
            latency   = latency_c / fpga_freq;
            bandwidth = (big_payload_bytes + header_bytes) / latency;
        }
        
        // report results
        cout << "\n";
        cout << "One-Way Test Results (256 byte messages)\n";
        cout << "--------------------\n";
        cout << "FPGA cycles       = " << cycles << endl;
        cout << "Payload Bytes     = " << big_payload_bytes << endl;
        cout << "Header Bytes      = " << header_bytes << endl;
        cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
             << "                  = " << latency << " usec\n";
        cout << "Average Bandwidth = " << bandwidth << " MB/s\n";
    }

    //
    // perform two-way test
    //
    cycles = clientStub->F2HTwoWayTest(0, test_length);

    // compute results
    latency_c = double(cycles) / test_length;
    if (fpga_freq != 0)
    {
        latency   = latency_c / fpga_freq;
        bandwidth = 2 * datasize / latency;
    }
        
    // report results
    cout << "\n";
    cout << "Two-Way Test Results\n";
    cout << "--------------------\n";
    cout << "FPGA cycles       = " << cycles << endl;
    cout << "Payload Bytes     = " << payload_bytes << " (each way) " << endl;
    cout << "Header Bytes      = " << header_bytes << endl;
    cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
         << "                  = " << latency << " usec\n";
    cout << "Average Bandwidth = " << bandwidth << " MB/s\n";

    //
    // perform long two-way test
    //
    if (longTestsSwitch.Value() != 0)
    {
        cycles = clientStub->F2HTwoWayTest(1, test_length);

        // compute results
        big_payload_bytes = payload_bytes * 16;
        latency_c = double(cycles) / test_length;
        if (fpga_freq != 0)
        {
            latency   = latency_c / fpga_freq;
            bandwidth = 2 * (big_payload_bytes + header_bytes) / latency;
        }
        
        // report results
        cout << "\n";
        cout << "Two-Way Test Results (128 byte messages)\n";
        cout << "--------------------\n";
        cout << "FPGA cycles       = " << cycles << endl;
        cout << "Payload Bytes     = " << big_payload_bytes << " (each way) " << endl;
        cout << "Header Bytes      = " << header_bytes << endl;
        cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
             << "                  = " << latency << " usec\n";
        cout << "Average Bandwidth = " << bandwidth << " MB/s\n";
    }

    //
    // perform two-way pipelined test
    //
    cycles = clientStub->F2HTwoWayPipeTest(0, test_length);

    // compute results
    latency_c = double(cycles) / test_length;
    if (fpga_freq != 0)
    {
        latency   = latency_c / fpga_freq;
        bandwidth = 2 * datasize / latency;
    }

    // report results
    cout << "\n";
    cout << "Two-Way Pipelined Test Results\n";
    cout << "------------------------------\n";
    cout << "FPGA cycles       = " << cycles << endl;
    cout << "Payload Bytes     = " << payload_bytes << " (each way) " << endl;
    cout << "Header Bytes      = " << header_bytes << endl;
    cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
         << "                  = " << latency << " usec\n";
    cout << "Average Bandwidth = " << bandwidth << " MB/s\n";

    if (longTestsSwitch.Value() != 0)
    {
        //
        // perform big two-way pipelined test
        //
        cycles = clientStub->F2HTwoWayPipeTest(1, test_length);

        // compute results
        big_payload_bytes = payload_bytes * 16;
        latency_c = double(cycles) / test_length;
        if (fpga_freq != 0)
        {
            latency   = latency_c / fpga_freq;
            bandwidth = 2 * (big_payload_bytes + header_bytes) / latency;
        }

        // report results
        cout << "\n";
        cout << "Two-Way Pipelined Test Results (128 byte messages)\n";
        cout << "------------------------------\n";
        cout << "FPGA cycles       = " << cycles << endl;
        cout << "Payload Bytes     = " << big_payload_bytes << " (each way) " << endl;
        cout << "Header Bytes      = " << header_bytes << endl;
        cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
             << "                  = " << latency << " usec\n";
        cout << "Average Bandwidth = " << bandwidth << " MB/s\n";
    }

    // done!
    cout << "\n";
    cout << "Tests Complete!\n";
}
