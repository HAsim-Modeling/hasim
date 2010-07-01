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
// @file platform-debugger.cpp
// @brief Platform Debugger Application
//
// @author Angshuman Parashar
//

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <cmath>

#include "asim/syntax.h"
#include "asim/ioformat.h"
#include "asim/provides/hybrid_application.h"
#include "asim/provides/clocks_device.h"
#include "asim/provides/ddr2_device.h"

using namespace std;

const char*  
getIdxName(const int idx)
{
    switch(idx)
    {
        case 0:  return "prim_device.ram1.enqueue_address_RDY()";
        case 1:  return "prim_device.ram1.enqueue_data_RDY()";
        case 2:  return "prim_device.ram1.dequeue_data_RDY()";
        case 3:  return "mergeReqQ.notEmpty()";
        case 4:  return "mergeReqQ.ports[0].notFull()";
        case 5:  return "mergeReqQ.ports[1].notFull()";
        case 6:  return "syncReadDataQ.notEmpty()";
        case 7:  return "syncReadDataQ.notFull()";
        case 8:  return "syncResetQ.notEmpty()";
        case 9:  return "syncResetQ.notFull()";
        case 10: return "syncRequestQ.notEmpty()";
        case 11: return "syncRequestQ.notFull()";
        case 12: return "syncWriteDataQ.notEmpty()";
        case 13: return "syncWriteDataQ.notFull()";
        case 14: return "writePending";
        case 15: return "readPending";
        case 16: return "nInflightReads.value() == 0";
        case 17: return "readBurstCnt == 0";
        case 18: return "writeBurstIdx == 0";
        case 19: return "state";
        default: return "unused";
    }
}

UINT64 
getBit(UINT64 bvec, int idx, UINT64 mask)
{
    return (bvec >> idx) & mask;
}

void
printRAMStatus(UINT64 status)
{
    cout << "RAM status:" << hex << status << dec << endl;
    for (int x = 0; x < 20; x++)
    {
        cout << "    [" << getIdxName(x) << "]: " << getBit(status, x, 1) << endl;
    }
}

void
printRAMStatusDiff(UINT64 new_status, UINT64 old_status)
{
    int any_change = 0;
    for (int x = 0; x < 20; x++)
    {
        UINT64 b_old = getBit(old_status, x, 1);
        UINT64 b_new = getBit(new_status, x, 1);
        if (b_old != b_new)
        {
            cout << "    [" << getIdxName(x) << "] Now: " <<  b_new << endl;
            any_change = 1;
        }
    }
    if (!any_change)
    {
        cout << "No RAM change." << endl;  
    }
}
// constructor
HYBRID_APPLICATION_CLASS::HYBRID_APPLICATION_CLASS(
    VIRTUAL_PLATFORM vp)
{
    clientStub = new PLATFORM_DEBUGGER_CLIENT_STUB_CLASS(NULL);
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
    UINT64 sts, oldsts;
    UINT64 data;

    // print banner
    cout << "\n";
    cout << "Welcome to the Platform Debugger\n";
    cout << "--------------------------------\n";

    cout << endl << "Initializing hardware\n";

    sts = clientStub->StatusCheck(0);
    oldsts = sts;
    printRAMStatus(sts);

    // transfer control to hardware
    sts = clientStub->StartDebug(0);
    cout << "debugging started, sts = " << sts << endl << flush;

    // Write a pattern to memory.  On the Bluespec side, data is written to bank 0
    // and the inverse of data is written to bank 1.
    for (int i = 0; i <= 1000; i += 1)
    {
        int addr = i;
        data = ((UINT64(i) + 123456) << 32) | (UINT64(i) + 1001);
        sts = clientStub->WriteReq(addr * 4);
        for (int b = 0; b < MEM_BURST_COUNT; b++)
        {
            sts = clientStub->WriteData(data, 0);
            data = ~data;
        }
    }

    cout << "writes done" << endl;

    // Read the pattern back.  Alternate banks on each request.
    int errors = 0;
    int incr = (MEM_BANKS > 1) ? 1 : 2;
    for (int i = 0; i <= 1000; i += incr)
    {
        int addr = i;
        sts = (addr & 1) ? clientStub->ReadReq1(addr * 4) : clientStub->ReadReq0(addr * 4);
    
        UINT64 expect = ((UINT64(i) + 123456) << 32) | (UINT64(i) + 1001);
        if (addr & 1) expect = ~expect;

        for (int b = 0; b < MEM_BURST_COUNT; b++)
        {
            data = (addr & 1) ? clientStub->ReadRsp1(0) : clientStub->ReadRsp0(0);
            if (data != expect)
            {
                cout << hex << "error read data 0x" << addr << " = 0x" << data << " expect 0x" << expect << dec << endl;
                errors += 1;
            }

            expect = ~expect;
        }
    }

    cout << errors << " read errors" << endl << endl << flush;

    //
    // Optimal read buffer size calibration
    //
#if (MEM_CHECK_LATENCY != 0)
    cout << "Latencies:" << endl;
    int min_idx = 0;
    int min_latency = 0;
    for (int i = 1; i <= SRAM_MAX_OUTSTANDING_READS; i++)
    {
        OUT_TYPE_ReadLatency r = clientStub->ReadLatency(256, i);
        cout << i << ": first " << r.firstReadLatency << " cycles, average "
             << r.totalLatency / 256.0 << " per load" << endl << flush;

        if ((min_idx == 0) || (r.totalLatency < min_latency))
        {
            min_idx = i;
            min_latency = r.totalLatency;
        }
    }

    cout << "Optimal reads in flight: " << min_idx << endl << endl << flush;
#endif

    errors = 0;
    for (int m = 0; m < 8; m++)
    {
        clientStub->WriteReq(0);
        for (int b = 0; b < MEM_BURST_COUNT; b++)
        {
            clientStub->WriteData(0xffffffffffffffff, 0);
        }

        clientStub->WriteReq(0);
        for (int b = 0; b < MEM_BURST_COUNT; b++)
        {
            clientStub->WriteData(0, 1 << m);
        }

        clientStub->ReadReq0(0);
        for (int b = 0; b < MEM_BURST_COUNT; b++)
        {
            UINT64 data = clientStub->ReadRsp0(0);
            UINT64 expect = 0xffL << (m * 8);

            if (data != expect)
            {
                printf("Mask error %d:  0x%016llx, expect 0x%016llx\n", m, data, expect);
                errors += 1;
            }
        }
    }

    cout << errors << " mask errors" << endl << endl << flush;

    // report results and exit
    cout << "Done" << endl;
}
