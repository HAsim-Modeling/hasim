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
// @file params-controller-hybrid.h
// @brief Pass dynamic parameters to the hardware side
//
// @author Michael Adler
//

#ifndef _PARAMS_CONTROLLER_
#define _PARAMS_CONTROLLER_

#include <stdio.h>

#include "platforms-module.h"
#include "asim/provides/rrr.h"

typedef class PARAMS_CONTROLLER_CLASS* PARAMS_CONTROLLER;

class PARAMS_CONTROLLER_CLASS: public RRR_SERVICE_CLASS,
                               public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static PARAMS_CONTROLLER_CLASS instance;

  public:
    PARAMS_CONTROLLER_CLASS();
    ~PARAMS_CONTROLLER_CLASS();

    // static methods
    static PARAMS_CONTROLLER GetInstance() { return &instance; }

    // Send dynamic parameters to hardware
    void SendAllParams();

    // required RRR service methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    UMF_MESSAGE Request(UMF_MESSAGE);
};

#endif
