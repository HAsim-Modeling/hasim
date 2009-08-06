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
// @file hello.cpp
// @brief Bluespec System
//
// @author Angshuman Parashar
//

#include "asim/provides/bluespec_system.h"

using namespace std;

// constructor
BLUESPEC_SYSTEM_CLASS::BLUESPEC_SYSTEM_CLASS(
    LLPI llpi) :
        PLATFORMS_MODULE_CLASS(NULL)
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
    SYSTEM_CLASS::Main();
}
