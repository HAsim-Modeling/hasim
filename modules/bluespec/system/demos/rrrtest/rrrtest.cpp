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

#include "asim/rrr/service_ids.h"
#include "asim/provides/bluespec_system.h"
#include "asim/provides/command_switches.h"
#include "asim/ioformat.h"

using namespace std;

// constructor
BLUESPEC_SYSTEM_CLASS::BLUESPEC_SYSTEM_CLASS(
    LLPI llpi) :
        PLATFORMS_MODULE_CLASS(NULL)
{
    // instantiate client stub
    clientStub = new RRRTEST_CLIENT_STUB_CLASS(this);
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
    UINT64 test_length  = 100; // FIXME: take this from a dynamic parameter
    UINT64 fpga_freq    = 50;    // FIXME: take this from a dynamic parameter
    UINT64 payload_bits = 64;    // FIXME: no idea how

    double datasize = payload_bits / 8;
    double latency_c;
    double latency;
    double bandwidth;

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
    cycles = clientStub->F2HOneWayTest(test_length);

    // compute results
    latency_c = double(cycles) / test_length;
    latency   = latency_c / fpga_freq;
    bandwidth = datasize / latency;
        
    // report results
    cout << "\n";
    cout << "One-Way Test Results\n";
    cout << "--------------------\n";
    cout << "FPGA cycles       = " << cycles << endl;
    cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
         << "                  = " << latency << " usec\n";
    cout << "Average Bandwidth = " << bandwidth << " MB/s\n";

    //
    // perform two-way test
    //
    cycles = clientStub->F2HTwoWayTest(test_length);

    // compute results
    latency_c = double(cycles) / test_length;
    latency   = latency_c / fpga_freq;
    bandwidth = datasize / latency;
        
    // report results
    cout << "\n";
    cout << "Two-Way Test Results\n";
    cout << "--------------------\n";
    cout << "FPGA cycles       = " << cycles << endl;
    cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
         << "                  = " << latency << " usec\n";
    cout << "Average Bandwidth = " << bandwidth << " MB/s\n";

    //
    // perform two-way pipelined test
    //
    cycles = clientStub->F2HTwoWayPipeTest(test_length);

    // compute results
    latency_c = double(cycles) / test_length;
    latency   = latency_c / fpga_freq;
    bandwidth = datasize / latency;
        
    // report results
    cout << "\n";
    cout << "Two-Way Pipelined Test Results\n";
    cout << "------------------------------\n";
    cout << "FPGA cycles       = " << cycles << endl;
    cout << "Average Latency   = " << latency_c << " FPGA cycles\n" 
         << "                  = " << latency << " usec\n";
    cout << "Average Bandwidth = " << bandwidth << " MB/s\n";

    // done!
    cout << "\n";
}
