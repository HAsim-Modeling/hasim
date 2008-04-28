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
 
#ifndef _ISA_EMULATOR_
#define _ISA_EMULATOR_

#include <stdio.h>

#include "../platforms-module.h"
#include "asim/provides/rrr.h"

// this module provides both client and service functionalities

typedef class ISA_EMULATOR_CLASS* ISA_EMULATOR;
class ISA_EMULATOR_CLASS: public RRR_SERVICE_CLASS,
                     public PLATFORMS_MODULE_CLASS
{
    private:
        // self-instantiation
        static ISA_EMULATOR_CLASS instance;

    public:
        ISA_EMULATOR_CLASS();
        ~ISA_EMULATOR_CLASS();

        // static methods
        static ISA_EMULATOR GetInstance() { return &instance; }

        // required RRR service methods
        void Init(PLATFORMS_MODULE);
        UMF_MESSAGE Request(UMF_MESSAGE req);
        void Poll();

        // client methods
        void updateRegister(UINT32 rname, UINT32 rval);
};

#endif
