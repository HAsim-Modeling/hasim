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
 
#ifndef _ISA_EMULATOR_IMPL_
#define _ISA_EMULATOR_IMPL_

#include <stdio.h>

#include "asim/syntax.h"

#include "asim/provides/funcp_base_types.h"
#include "asim/provides/hasim_isa.h"
#include "asim/provides/isa_emulator.h"

// this module provides both client and service functionalities
typedef class ISA_EMULATOR_IMPL_CLASS* ISA_EMULATOR_IMPL;

class ISA_EMULATOR_IMPL_CLASS
{
  private:
    ISA_EMULATOR parent;

  public:
    ISA_EMULATOR_IMPL_CLASS(ISA_EMULATOR parent) : parent(parent) {};
    ~ISA_EMULATOR_IMPL_CLASS() {};

    void SyncReg(
        CONTEXT_ID ctxId,
        ISA_REG_INDEX_CLASS rName,
        FUNCP_REG rVal)
    {};

    ISA_EMULATOR_RESULT Emulate(
        CONTEXT_ID ctxId,
        FUNCP_VADDR pc,
        ISA_INSTRUCTION inst,
        FUNCP_VADDR *newPC)
    {
        //
        // At this point you could update register values in the hardware
        // by doing:
        //
        //     ISA_REG_INDEX_CLASS rName;
        //     rName.SetArchReg(3);
        //     parent->UpdateRegister(rName, 0xdeadbeef);
        //

        ASIMWARNING("Hybrid ISA emulation is not supported for this configuration\n");

        *newPC = 0;
        return ISA_EMULATOR_EXIT_FAIL;
    }
};

#endif // _ISA_EMULATOR_IMPL_
