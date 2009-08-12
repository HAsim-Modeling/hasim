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

#ifndef __SYSTEM__
#define __SYSTEM__

#include <stdio.h>
#include <pthread.h>

#include "platforms-module.h"
#include "asim/provides/low_level_platform_interface.h"

typedef class SYSTEM_CLASS* SYSTEM;
class SYSTEM_CLASS
{
  private:
    pthread_mutex_t lock;
    pthread_cond_t  cond;

  public:
    SYSTEM_CLASS()
    {
        pthread_mutex_init(&lock, NULL);
        pthread_cond_init(&cond, NULL);
    }

    virtual void Main()
    {
        return;
    }
};

#endif
