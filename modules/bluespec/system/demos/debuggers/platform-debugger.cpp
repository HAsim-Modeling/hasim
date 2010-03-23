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
        default: return "unused";
    }
}

UINT32 
getBit(UINT32 bvec, int idx)
{
    int v =  (int) pow(2, (double) idx);
    return (bvec & v) >> idx;
}

void
printRAMStatus(UINT32 status)
{
    cout << "RAM status:" << hex << status << dec << endl;
    for (int x = 0; x < 19; x++)
    {
        cout << "    [" << getIdxName(x) << "]: " << getBit(status,x) << endl;
    }
}

void
printRAMStatusDiff(UINT32 new_status, UINT32 old_status)
{
    int any_change = 0;
    for (int x = 0; x < 19; x++)
    {
        UINT32 b_old = getBit(old_status, x);
        UINT32 b_new = getBit(new_status, x);
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
    UINT32 sts, oldsts, data;
    const int BURSTSIZE = 2;

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

    // load
    sts = clientStub->ReadReq(0);
    cout << "read req sent, sts = " << sts << endl << flush;
    printRAMStatusDiff(sts, oldsts);
    oldsts = sts;
    
    for (int i = 0; i < BURSTSIZE; i++)
    {
        data = clientStub->ReadRsp(0);
        cout << "read data = " << data << endl;
    }

    // report results and exit
    cout << "\n";

}
