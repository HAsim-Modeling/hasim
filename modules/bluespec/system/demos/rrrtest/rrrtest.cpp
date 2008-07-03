//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
//

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
BLUESPEC_SYSTEM_CLASS::BLUESPEC_SYSTEM_CLASS() :
        PLATFORMS_MODULE_CLASS(NULL),
        client(this)
{
}

// destructor
BLUESPEC_SYSTEM_CLASS::~BLUESPEC_SYSTEM_CLASS()
{
}

// main
void
BLUESPEC_SYSTEM_CLASS::Main()
{
    UINT64 cycles;
    UINT64 test_length  = 1000000; // FIXME: take this from a dynamic parameter
    UINT64 fpga_freq    = 50;     // FIXME: take this from a dynamic parameter
    UINT64 payload_bits = 64;     // FIXME: no idea how

    double datasize = payload_bits / 8;
    double latency_c;
    double latency;
    double bandwidth;

    // print banner and test parameters
    cout << "\n";
    cout << "Test Parameters\n";
    cout << "---------------\n";
    cout << "Number of Transactions  = " << test_length << endl;
    cout << "Payload Width (in bits) = " << payload_bits << endl;
    cout << "FPGA Clock Frequency    = " << fpga_freq << endl;

    //
    // perform one-way test
    //
    cycles = client.F2HOneWayTest(test_length);

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
    cycles = client.F2HTwoWayTest(test_length);

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
    cycles = client.F2HTwoWayPipeTest(test_length);

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
