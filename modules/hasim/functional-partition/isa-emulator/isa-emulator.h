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

#include "asim/provides/rrr.h"
#include "asim/provides/funcp_base_types.h"
#include "asim/provides/hasim_isa.h"


//
// There are multiple levels of emulation defined here, ranging from full
// blown to opcode specific.
//


// ========================================================================
//
// Normal (slowest) emulator.  All register state is copied from the FPGA,
// the FPGA simulation stops and control is passed to software.
//
// ========================================================================

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

typedef class ISA_EMULATOR_SERVER_CLASS* ISA_EMULATOR_SERVER;
typedef class ISA_EMULATOR_SERVER_CLASS* ISA_EMULATOR;

class ISA_EMULATOR_SERVER_CLASS: public RRR_SERVER_CLASS,
                                 public PLATFORMS_MODULE_CLASS,
                                 public TRACEABLE_CLASS
{
  private:
    // self-instantiation
    static ISA_EMULATOR_SERVER_CLASS instance;

    // stubs
    RRR_SERVER_STUB serverStub;

    ISA_EMULATOR_IMPL emulator;

    Format fmt_regnum;
    Format fmt_regval;
    Format fmt_inst;

  public:
    ISA_EMULATOR_SERVER_CLASS();
    ~ISA_EMULATOR_SERVER_CLASS();

    // static methods
    static ISA_EMULATOR GetInstance() { return &instance; }

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();
    void Poll();

    //
    // RRR Service Methods
    //
    UMF_MESSAGE Request(UMF_MESSAGE req);    

    // client methods
    void UpdateRegister(CONTEXT_ID ctxId, ISA_REG_INDEX_CLASS rName, FUNCP_REG rVal);
};


// ========================================================================
//
// Floating point opcode emulation.  Only the register inputs are copied
// from the FPGA.  The result register value is passed back.  The FPGA
// continues simulation and the timing model is unaware of this emulation.
//
// ========================================================================

// Early declaration since of the circular dependence
typedef class ISA_REGOP_EMULATOR_IMPL_CLASS* ISA_REGOP_EMULATOR_IMPL;

typedef class ISA_REGOP_EMULATOR_SERVER_CLASS* ISA_REGOP_EMULATOR_SERVER;
typedef class ISA_REGOP_EMULATOR_SERVER_CLASS* ISA_REGOP_EMULATOR;

class ISA_REGOP_EMULATOR_SERVER_CLASS: public RRR_SERVER_CLASS,
                                       public PLATFORMS_MODULE_CLASS,
                                       public TRACEABLE_CLASS
{
  private:
    // self-instantiation
    static ISA_REGOP_EMULATOR_SERVER_CLASS instance;

    // stubs
    RRR_SERVER_STUB serverStub;

    ISA_REGOP_EMULATOR_IMPL emulator;

    Format fmt_regnum;
    Format fmt_regval;
    Format fmt_inst;

  public:
    ISA_REGOP_EMULATOR_SERVER_CLASS();
    ~ISA_REGOP_EMULATOR_SERVER_CLASS();

    // static methods
    static ISA_REGOP_EMULATOR GetInstance() { return &instance; }

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();
    void Poll();

    //
    // RRR Service Methods
    //
    UMF_MESSAGE Request(UMF_MESSAGE req);    

    // client methods
    void UpdateRegister(CONTEXT_ID ctxId, ISA_REG_INDEX_CLASS rName, FUNCP_REG rVal);
};


// server stub
#define BYPASS_SERVER_STUB
#include "asim/rrr/server_stub_ISA_EMULATOR.h"
#include "asim/rrr/server_stub_ISA_REGOP_EMULATOR.h"
#undef  BYPASS_SERVER_STUB

//
// Include of the implementation must be delayed to here so that
// ISA_EMULATOR_RESULT and a pointer to ISA_EMULATOR_SERVER_CLASS are defined.
//
#include "asim/provides/isa_emulator_impl.h"

#endif
