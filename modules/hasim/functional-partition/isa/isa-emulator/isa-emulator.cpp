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
 
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>

#include "asim/syntax.h"
#include "asim/mesg.h"

#include "awb/provides/model.h"
#include "asim/rrr/service_ids.h"
#include "asim/provides/isa_emulator.h"
#include "asim/provides/isa_emulator_impl.h"

using namespace std;

// ========================================================================
//
// Normal (slowest) emulator.  All register state is copied from the FPGA,
// the FPGA simulation stops and control is passed to software.
//
// ========================================================================

// ===== service instantiation =====
ISA_EMULATOR_SERVER_CLASS ISA_EMULATOR_SERVER_CLASS::instance;

// constructor
ISA_EMULATOR_SERVER_CLASS::ISA_EMULATOR_SERVER_CLASS() : 
    emulator(NULL)
{
    SetTraceableName("isa_emulator");

    VERIFYX(sizeof(ISA_ADDRESS) == sizeof(FUNCP_VADDR));
    VERIFYX(sizeof(ISA_VALUE) == sizeof(FUNCP_INT_REG));

    // instantiate stubs
    serverStub = new ISA_EMULATOR_SERVER_STUB_CLASS(this);
    clientStub = new ISA_EMULATOR_CLIENT_STUB_CLASS(this);

    char fmt[16];
    sprintf(fmt, "0%dx", sizeof(ISA_VALUE) * 2);
    fmt_regval = Format("0x", fmt);
    fmt_regnum = Format("3d");
    fmt_inst = Format("0x", "08x");
}

// destructor
ISA_EMULATOR_SERVER_CLASS::~ISA_EMULATOR_SERVER_CLASS()
{
    // deallocate stubs
    delete serverStub;
}

// init
void
ISA_EMULATOR_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    parent = p;

    emulator = new ISA_EMULATOR_IMPL_CLASS(this);
}

// uninit: override
void
ISA_EMULATOR_SERVER_CLASS::Uninit()
{
    delete emulator;    
}


typedef UINT32 ISA_INSTRUCTION;


// Copy a register from FPGA to host in preparation for emulation
//method sync(in CONTEXT_ID_RRR[CONTEXT_ID_BITS_RRR] ctxId,
//            in REG_NAME_RRR[16] rName,
//            in ISA_VALUE[FUNCP_ISA_INT_REG_SIZE] rValue);

// Full instruction emulation with possible memory or I/O side
// effects
//        method emulate(in CONTEXT_ID_RRR[CONTEXT_ID_BITS_RRR] ctxId,
//                       in INSTRUCTION_RRR[32] instr,
//                       in ISA_ADDRESS[FUNCP_ISA_V_ADDR_SIZE] pc,
//                       out ISA_ADDRESS[FUNCP_ISA_V_ADDR_SIZE] newPc);

void
ISA_EMULATOR_SERVER_CLASS::sync(CONTEXT_ID_RRR ctxIdRaw, REG_NAME_RRR rNameRaw, ISA_VALUE rValueRaw)
{
    FUNCP_REG rVal;
    ISA_REG_INDEX_CLASS rName;
    CONTEXT_ID ctxId;

    rVal.intReg = rValueRaw;
    rName = rNameRaw;
    ctxId = CONTEXT_ID(ctxIdRaw);


    if (TRACING(2))
    {
        if (rName.IsArchReg())
        {
            T2("\tisa_emulator: Sync CTX " << UINT64(ctxId) << " ArchReg " << fmt_regnum(rName.ArchRegNum()) << ": " << fmt_regval(rVal.intReg));
        }
        else if (rName.IsFPReg())
        {
            T2("\tisa_emulator: Sync CTX " << UINT64(ctxId) << " FPReg " << fmt_regnum(rName.FPRegNum()) << ": " << fmt_regval(rVal.intReg));
        }
        else if (rName.IsControlReg())
        {
            T2("\tisa_emulator: Sync CTX " << UINT64(ctxId) << " ControlReg:  " << fmt_regval(rVal.intReg));
        }
        else if (rName.IsFPControlReg())
        {
            T2("\tisa_emulator: Sync CTX " << UINT64(ctxId) << " FPControlReg:  " << fmt_regval(rVal.intReg));
        }
        else
        {
            ASIMERROR("Unknown register type");
        }
    }

    emulator->SyncReg(ctxId, rName, rVal);
        
}

ISA_ADDRESS
ISA_EMULATOR_SERVER_CLASS::emulate(CONTEXT_ID_RRR ctxIdRaw,
                                   INSTRUCTION_RRR instRaw,
                                   ISA_ADDRESS pcRaw)
{
    FUNCP_VADDR pc;
    ISA_INSTRUCTION inst;
    CONTEXT_ID ctxId;

    pc = pcRaw;
    inst = instRaw;
    ctxId = CONTEXT_ID(ctxIdRaw);

    T1("\tisa_emulator: Emulate CTX " << UINT64(ctxId) << " PC " << fmt_regval(pc) << ", Inst " << fmt_inst(inst));
        
    FUNCP_VADDR newPC;
    ISA_EMULATOR_RESULT r = emulator->Emulate(ctxId, pc, inst, &newPC);

    //
    // Hack.  Result is sent in low 2 bits of the returned PC.  This works
    // for Alpha and MIPS until we get support for multiple returned objects
    // in RRR.
    //
    newPC ^= (newPC & 3);       // Clear low 2 bits
    switch (r)
    {
        case ISA_EMULATOR_NORMAL:
            T1("\tisa_emulator: Done with emulation");
        break;

        case ISA_EMULATOR_BRANCH:
            T1("\tisa_emulator: Done with emulation, BRANCH to next PC " << fmt_regval(newPC));
            newPC |= 1;
        break;

        case ISA_EMULATOR_SLEEP:
            T1("\tisa_emulator: Done with emulation, SLEEP");
            newPC |= 2;
        break;

        case ISA_EMULATOR_EXIT_OK:
            T1("\tisa_emulator: Emulation forcing normal exit");
            newPC = 7;
        break;

        case ISA_EMULATOR_EXIT_FAIL:
            T1("\tisa_emulator: Emulation forcing normal ERROR exit");
            newPC = 3;
        break;

        default:
            ASIMERROR("Unexpected result from ISA emulator implementation");
    }

    return newPC;
}


// client: update register
void
ISA_EMULATOR_SERVER_CLASS::UpdateRegister(
    CONTEXT_ID ctxId,
    ISA_REG_INDEX_CLASS rName,
    FUNCP_REG rVal)
{

    if (TRACING(1))
    {
        if (rName.IsArchReg())
        {
            T1("\tisa_emulator: Updating CTX " << UINT64(ctxId) << " ArchReg " << fmt_regnum(rName.ArchRegNum()) << ": " << fmt_regval(rVal.intReg));
        }
        else if (rName.IsFPReg())
        {
            T1("\tisa_emulator: Updating CTX " << UINT64(ctxId) << " FPReg " << fmt_regnum(rName.FPRegNum()) << ": " << fmt_regval(rVal.intReg));
        }
        else if (rName.IsControlReg())
        {
            T1("\tisa_emulator: Updating CTX " << UINT64(ctxId) << " ControlReg: " << fmt_regval(rVal.intReg));
        }
        else if (rName.IsFPControlReg())
        {
            T1("\tisa_emulator: Updating CTX " << UINT64(ctxId) << " FPControlReg: " << fmt_regval(rVal.intReg));
        }
        else
        {
            ASIMERROR("Unknown register type");
        }
    }

    clientStub->updateRegister(CONTEXT_ID_RRR(ctxId), rName, rVal.intReg);
}


// ========================================================================
//
// Floating point opcode emulation.  Only the register inputs are copied
// from the FPGA.  The result register value is passed back.  The FPGA
// continues simulation and the timing model is unaware of this emulation.
//
// ========================================================================


// ===== service instantiation =====
ISA_REGOP_EMULATOR_SERVER_CLASS ISA_REGOP_EMULATOR_SERVER_CLASS::instance;

// constructor
ISA_REGOP_EMULATOR_SERVER_CLASS::ISA_REGOP_EMULATOR_SERVER_CLASS() : emulator(NULL)
{
    SetTraceableName("isa_regop_emulator");

    VERIFYX(sizeof(ISA_ADDRESS) == sizeof(FUNCP_VADDR));
    VERIFYX(sizeof(ISA_VALUE) == sizeof(FUNCP_INT_REG));

    // instantiate stubs
    serverStub = new ISA_REGOP_EMULATOR_SERVER_STUB_CLASS(this);

    char fmt[16];
    sprintf(fmt, "0%dx", sizeof(ISA_VALUE) * 2);
    fmt_regval = Format("0x", fmt);
    fmt_regnum = Format("3d");
    fmt_inst = Format("0x", "08x");
}

// destructor
ISA_REGOP_EMULATOR_SERVER_CLASS::~ISA_REGOP_EMULATOR_SERVER_CLASS()
{
    // deallocate stubs
    delete serverStub;
}

// init
void
ISA_REGOP_EMULATOR_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    parent = p;

    emulator = new ISA_REGOP_EMULATOR_IMPL_CLASS(this);
}

// uninit: override
void
ISA_REGOP_EMULATOR_SERVER_CLASS::Uninit()
{
    delete emulator;
}

typedef UINT32 ISA_INSTRUCTION;


ISA_VALUE 
ISA_REGOP_EMULATOR_SERVER_CLASS::emulateRegOp(
    CONTEXT_ID_RRR ctxIdRaw,
    ISA_INSTRUCTION instrRaw,
    ISA_ADDRESS pcRaw,
    ISA_VALUE srcVal0Raw,
    ISA_VALUE srcVal1Raw,
    REG_NAME_RRR rNameSrc0Raw,  // Source arch. register
    REG_NAME_RRR rNameSrc1Raw,
    REG_NAME_RRR rNameDstRaw)
{

    CONTEXT_ID ctxId;
    ISA_INSTRUCTION inst;
    FUNCP_VADDR pc;
    FUNCP_REG srcVal0;
    FUNCP_REG srcVal1;
    ISA_REG_INDEX_CLASS rNameSrc0;
    ISA_REG_INDEX_CLASS rNameSrc1;
    ISA_REG_INDEX_CLASS rNameDst;

    FUNCP_REG dstVal;

    rNameDst = rNameDstRaw;
    rNameSrc1 = rNameSrc1Raw;
    rNameSrc0 = rNameSrc0Raw;
    srcVal1.intReg = srcVal1Raw;
    srcVal0.intReg = srcVal0Raw;
    pc = pcRaw;
    inst = instrRaw;
    ctxId = CONTEXT_ID(ctxIdRaw);

    if (TRACING(1))
    {
        T1("\tisa_regop_emulator: CTX " << UINT64(ctxId) << ": " << fmt_regval(pc) << " " << fmt_inst(inst));
    }

    dstVal = emulator->EmulateRegOp(ctxId, pc, inst,
                                    srcVal0, srcVal1,
                                    rNameSrc0, rNameSrc1, rNameDst);

    if (TRACING(1))
    {
        T1("\tisa_regop_emulator: CTX " << UINT64(ctxId) << ": " << fmt_regval(pc)
           << "  Resp: " << fmt_regval(dstVal.intReg));
    }

    return dstVal.intReg;
}    

// ========================================================================
//
// Debug logger.
//
// ========================================================================


// ===== service instantiation =====
ISA_DP_DEBUG_SERVER_CLASS ISA_DP_DEBUG_SERVER_CLASS::instance;

// constructor
ISA_DP_DEBUG_SERVER_CLASS::ISA_DP_DEBUG_SERVER_CLASS()
{
    SetTraceableName("isa_dp_debug");

    // instantiate stubs
    serverStub = new ISA_DP_DEBUG_SERVER_STUB_CLASS(this);

    char fmt[16];
    sprintf(fmt, "0%dx", sizeof(ISA_VALUE) * 2);
    fmt_regval = Format("0x", fmt);
    fmt_inst = Format("0x", "08x");
}

// destructor
ISA_DP_DEBUG_SERVER_CLASS::~ISA_DP_DEBUG_SERVER_CLASS()
{
    // deallocate stubs
    delete serverStub;
}

// init
void
ISA_DP_DEBUG_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    parent = p;

    logFile.open(LEAP_DEBUG_PATH "/hasim_isa_dp_rrr.out", ios::out | ios::trunc);
}

// uninit: override
void
ISA_DP_DEBUG_SERVER_CLASS::Uninit()
{
    logFile.close();
}

void
ISA_DP_DEBUG_SERVER_CLASS::noteInstr(
    CONTEXT_ID_RRR  ctxIdRaw,
    INSTRUCTION_RRR instrRaw,
    ISA_ADDRESS pcRaw,
    ISA_VALUE srcVal0Raw,
    ISA_VALUE srcVal1Raw)
{
    CONTEXT_ID ctxId;
    ISA_INSTRUCTION inst;
    FUNCP_VADDR pc;
    FUNCP_REG srcVal0;
    FUNCP_REG srcVal1;

    srcVal1.intReg = srcVal1Raw;
    srcVal0.intReg = srcVal0Raw;
    pc = pcRaw;
    inst = instrRaw;
    ctxId = CONTEXT_ID(ctxIdRaw);

    logFile << UINT64(ctxId) << ": "
            << fmt_regval(pc) << " " << fmt_inst(inst) << " <- "
            << fmt_regval(srcVal0.intReg) << " "
            << fmt_regval(srcVal1.intReg) << endl;
    logFile.flush();

}
