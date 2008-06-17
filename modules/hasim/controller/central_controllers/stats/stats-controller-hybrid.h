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

#ifndef _STATS_CONTROLLER_
#define _STATS_CONTROLLER_

#include "platforms-module.h"
#include "asim/provides/rrr.h"

#include "asim/dict/STATS.h"

// this module handles gathering statistics. 
// Eventually this will interact with standard tools.

typedef class STATS_CONTROLLER_CLASS* STATS_CONTROLLER;
class STATS_CONTROLLER_CLASS: public RRR_SERVICE_CLASS,
                              public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static STATS_CONTROLLER_CLASS instance;

    // Running total of statistics as they are dumped incrementally
    UINT64 statValues[STATS_DICT_ENTRIES];

  public:
    STATS_CONTROLLER_CLASS();
    ~STATS_CONTROLLER_CLASS();

    void EmitFile();

    // static methods
    static STATS_CONTROLLER GetInstance() { return &instance; }

    // required RRR service methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    UMF_MESSAGE Request(UMF_MESSAGE);
    void Poll();
};


void StatsEmitFile();

#endif
