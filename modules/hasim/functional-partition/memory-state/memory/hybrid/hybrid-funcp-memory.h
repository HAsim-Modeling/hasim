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
 
#ifndef __FUNCP_MEMORY__
#define __FUNCP_MEMORY__

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"

#include "asim/provides/rrr.h"
#include "asim/provides/funcp_base_types.h"
#include "asim/provides/funcp_simulated_memory.h"

// types

typedef FUNCP_ADDR    MEM_ADDRESS;;
typedef FUNCP_INT_REG MEM_VALUE;


#define CMD_LOAD    0
#define CMD_STORE   1

class FUNCP_MEMORY_CLASS: public RRR_SERVICE_CLASS,
                          public PLATFORMS_MODULE_CLASS,
                          public TRACEABLE_CLASS
{
  private:
    // self-instantiation
    static FUNCP_MEMORY_CLASS instance;

    FUNCP_SIMULATED_MEMORY memory;

  public:
    FUNCP_MEMORY_CLASS();
    ~FUNCP_MEMORY_CLASS();

    void    Init(PLATFORMS_MODULE);
    void    Uninit();
    void    Cleanup();
    UMF_MESSAGE Request(UMF_MESSAGE);
    void    Poll();
};

#endif
