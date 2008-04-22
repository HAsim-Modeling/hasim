/* INTEL CONFIDENTIAL
 * Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
 * copyright license under Intel copyrights to copy and distribute this code 
 * internally only. This code is provided "AS IS" with no support and with no 
 * warranties of any kind, including warranties of MERCHANTABILITY,
 * FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
 * By making any use of this code, Recipient agrees that no other licenses 
 * to any Intel patents, trade secrets, copyrights or other intellectual 
 * property rights are granted herein, and no other licenses shall arise by 
 * estoppel, implication or by operation of law. Recipient accepts all risks 
 * of use.
*/
 
#ifndef __HYBRID_MEMORY__
#define __HYBRID_MEMORY__

#include "asim/provides/rrr.h"
#include "asim/provides/funcp_simulated_memory.h"

#define CMD_LOAD    0
#define CMD_STORE   1

class HYBRID_FUNCP_MEMORY_CLASS: public RRR_SERVICE_CLASS,
                                 public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static HYBRID_FUNCP_MEMORY_CLASS instance;

    FUNCP_SIMULATED_MEMORY memory;

  public:
    HYBRID_FUNCP_MEMORY_CLASS();
    ~HYBRID_FUNCP_MEMORY_CLASS();

    void    Init(PLATFORMS_MODULE);
    void    Uninit();
    void    Cleanup();
    bool    Request(UINT32, UINT32, UINT32, UINT32, UINT32 *);
    void    Poll();
};

#endif
