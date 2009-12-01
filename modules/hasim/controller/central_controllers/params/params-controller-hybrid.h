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
#include "asim/rrr/client_stub_PARAMS.h"

// PARAMS_CONTROLLER has no RRR server functionalities. Why then is the code
// structured like an RRR server (with self-instantiation etc.)? FIXME

typedef class PARAMS_SERVER_CLASS* PARAMS_SERVER;

class PARAMS_SERVER_CLASS: public RRR_SERVER_CLASS,
                           public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static PARAMS_SERVER_CLASS instance;

    // stubs
    PARAMS_CLIENT_STUB clientStub;

  public:
    PARAMS_SERVER_CLASS();
    ~PARAMS_SERVER_CLASS();

    // static methods
    static PARAMS_SERVER GetInstance() { return &instance; }

    // Send dynamic parameters to hardware
    void SendAllParams();

    // required RRR service methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();
};

// PARAMS_CONTROLLER functionalities are completely implemented by the PARAMS_SERVER class
typedef PARAMS_SERVER_CLASS PARAMS_CONTROLLER_CLASS;

#endif
