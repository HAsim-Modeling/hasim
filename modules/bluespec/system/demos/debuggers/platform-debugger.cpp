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

#include "asim/syntax.h"
#include "asim/ioformat.h"
#include "asim/provides/hybrid_application.h"
#include "asim/provides/clocks_device.h"

using namespace std;

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
    UINT32 sts, data;
    const int BURSTSIZE = 2;

    // print banner
    cout << "\n";
    cout << "Welcome to the Platform Debugger\n";
    cout << "--------------------------------\n";

    cout << endl << "Initializing hardware\n";

    // transfer control to hardware
    sts = clientStub->StartDebug(0);
    cout << "debugging started, sts = " << sts << endl << flush;

    // load
    sts = clientStub->ReadReq(0);
    cout << "read req sent, sts = " << sts << endl << flush;
    
    for (int i = 0; i < BURSTSIZE; i++)
    {
        data = clientStub->ReadRsp(0);
        cout << "read data = " << data << endl;
    }

    // store
    sts = clientStub->WriteReq(0);
    cout << "write req sent, sts = " << sts << endl;

    for (int i = 0; i < BURSTSIZE; i++)
    {
        sts = clientStub->WriteData(0xDEADBEEF, 0x3);
        cout << "write data, sts = " << sts << endl;
    }
    
    // load
    sts = clientStub->ReadReq(0);
    cout << "read req sent, sts = " << sts << endl << flush;
    
    for (int i = 0; i < BURSTSIZE; i++)
    {
        data = clientStub->ReadRsp(0);
        cout << "read data = " << data << endl;
    }

    // report results and exit
    cout << "\n";

}
