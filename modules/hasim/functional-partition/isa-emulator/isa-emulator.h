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

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"

#include "asim/provides/model.h"
#include "asim/provides/rrr.h"
#include "asim/provides/funcp_base_types.h"
#include "asim/provides/hasim_isa.h"


// this module provides both client and service functionalities

//
// Possible results from ISA emulator implementations to communicate back
// to the hardware client.
//
typedef enum
{
    ISA_EMULATOR_NORMAL,        // Standard instruction, next PC returned as a hint
    ISA_EMULATOR_BRANCH,        // Branch to new PC
    ISA_EMULATOR_EXIT_OK,       // Program done, success
    ISA_EMULATOR_EXIT_FAIL      // Program done, failure
}
ISA_EMULATOR_RESULT;

// Early declaration since of the circular dependence
typedef class ISA_EMULATOR_IMPL_CLASS* ISA_EMULATOR_IMPL;

typedef class ISA_EMULATOR_CLASS* ISA_EMULATOR;

class ISA_EMULATOR_CLASS: public RRR_SERVICE_CLASS,
                          public PLATFORMS_MODULE_CLASS,
                          public TRACEABLE_CLASS
{
  private:
    // self-instantiation
    static ISA_EMULATOR_CLASS instance;
    ISA_EMULATOR_IMPL emulator;

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
    void UpdateRegister(ISA_REG_INDEX_CLASS rName, FUNCP_INT_REG rVal);
};


//
// Include of the implementation must be delayed to here so that
// ISA_EMULATOR_RESULT and a pointer to ISA_EMULATOR_CLASS are defined.
//
#include "asim/provides/isa_emulator_impl.h"

#endif