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
// @file hello.h
// @brief Bluespec System
//
// @author Angshuman Parashar
//

#ifndef __BLUESPEC_SYSTEM__
#define __BLUESPEC_SYSTEM__

#include "platforms-module.h"
#include "asim/provides/hasim_controller.h"

typedef class BLUESPEC_SYSTEM_CLASS* BLUESPEC_SYSTEM;
class BLUESPEC_SYSTEM_CLASS: public SYSTEM_CLASS,
                             public PLATFORMS_MODULE_CLASS
{
  public:
    BLUESPEC_SYSTEM_CLASS();
    ~BLUESPEC_SYSTEM_CLASS();

    // main
    void Main();
};

#endif
