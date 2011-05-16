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

#include "asim/rrr/service_ids.h"
#include "asim/provides/isa_emulator.h"


using namespace std;

// ========================================================================
//
// Normal (slowest) emulator.  All register state is copied from the FPGA,
// the FPGA simulation stops and control is passed to software.
//
// ========================================================================

// TEMPORARY: cheat and assign client method IDs
#define CMD_SYNC     0
#define CMD_EMULATE  1
#define METHOD_ID_UPDATE_REG    0

// ===== service instantiation =====
ISA_EMULATOR_SERVER_CLASS ISA_EMULATOR_SERVER_CLASS::instance;

// constructor
ISA_EMULATOR_SERVER_CLASS::ISA_EMULATOR_SERVER_CLASS() : emulator(NULL)
{
    SetTraceableName("isa_emulator");

    VERIFYX(sizeof(ISA_ADDRESS) == sizeof(FUNCP_VADDR));
    VERIFYX(sizeof(ISA_VALUE) == sizeof(FUNCP_INT_REG));

    // instantiate stubs
    serverStub = new ISA_EMULATOR_SERVER_STUB_CLASS(this);

    char fmt[16];
    sprintf(fmt, "0%dx", sizeof(ISA_VALUE) * 2);
    fmt_regval = Format("0x", fmt);
    fmt_regnum = Format("3d");
    fmt_inst = Format("0x", "08x");
}

// destructor
ISA_EMULATOR_SERVER_CLASS::~ISA_EMULATOR_SERVER_CLASS()
{
    Cleanup();
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
    // cleanup
    Cleanup();
    
    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
ISA_EMULATOR_SERVER_CLASS::Cleanup()
{
    delete emulator;

    // deallocate stubs
    delete serverStub;
}

typedef UINT32 ISA_INSTRUCTION;

// handle service request
UMF_MESSAGE
ISA_EMULATOR_SERVER_CLASS::Request(UMF_MESSAGE req)
{
    FUNCP_REG rVal;
    FUNCP_VADDR pc;
    ISA_REG_INDEX_CLASS rName;
    ISA_INSTRUCTION inst;
    CONTEXT_ID ctx_id;

    UMF_MESSAGE resp;

    switch(req->GetMethodID())
    {
      case CMD_SYNC:
        rVal.intReg = req->ExtractUINT(sizeof(rVal));
        rName = req->ExtractUINT(2);
        ctx_id = CONTEXT_ID(req->ExtractUINT(sizeof(ctx_id)));
        delete req;

        if (TRACING(2))
        {
            if (rName.IsArchReg())
            {
                T2("\tisa_emulator: Sync CTX " << UINT64(ctx_id) << " ArchReg " << fmt_regnum(rName.ArchRegNum()) << ": " << fmt_regval(rVal.intReg));
            }
            else if (rName.IsFPReg())
            {
                T2("\tisa_emulator: Sync CTX " << UINT64(ctx_id) << " FPReg " << fmt_regnum(rName.FPRegNum()) << ": " << fmt_regval(rVal.intReg));
            }
            else if (rName.IsControlReg())
            {
                T2("\tisa_emulator: Sync CTX " << UINT64(ctx_id) << " ControlReg:  " << fmt_regval(rVal.intReg));
            }
            else if (rName.IsFPControlReg())
            {
                T2("\tisa_emulator: Sync CTX " << UINT64(ctx_id) << " FPControlReg:  " << fmt_regval(rVal.intReg));
            }
            else if (rName.IsLockReg())
            {
                T2("\tisa_emulator: Sync CTX " << UINT64(ctx_id) << " LockReg:     " << fmt_regval(rVal.intReg));
            }
            else if (rName.IsLockAddrReg())
            {
                T2("\tisa_emulator: Sync CTX " << UINT64(ctx_id) << " LockAddrReg: " << fmt_regval(rVal.intReg));
            }
            else
            {
                ASIMERROR("Unknown register type");
            }
        }

        emulator->SyncReg(ctx_id, rName, rVal);

        return NULL;
        break;

      case CMD_EMULATE:
        pc = req->ExtractUINT(sizeof(pc));
        inst = req->ExtractUINT(4);
        ctx_id = CONTEXT_ID(req->ExtractUINT(sizeof(ctx_id)));
        delete req;

        T1("\tisa_emulator: Emulate CTX " << UINT64(ctx_id) << " PC " << fmt_regval(pc) << ", Inst " << fmt_inst(inst));
        
        FUNCP_VADDR newPC;
        ISA_EMULATOR_RESULT r = emulator->Emulate(ctx_id, pc, inst, &newPC);

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

        resp = new UMF_MESSAGE_CLASS;
        resp->SetLength(8);
        resp->SetMethodID(CMD_EMULATE);
        resp->AppendUINT(newPC, sizeof(newPC));     // terminate
        return resp;
        break;
    }
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
        else if (rName.IsLockReg())
        {
            T1("\tisa_emulator: Updating CTX " << UINT64(ctxId) << " LockReg: " << fmt_regval(rVal.intReg));
        }
        else if (rName.IsLockAddrReg())
        {
            T1("\tisa_emulator: Updating CTX " << UINT64(ctxId) << " LockAddrReg: " << fmt_regval(rVal.intReg));
        }
        else
        {
            ASIMERROR("Unknown register type");
        }
    }

    //
    // Create message for RRR client.
    //
    // Assumes integer and FP registers are the same size!
    //
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS;
    msg->SetLength(sizeof(rVal) + 2 + sizeof(CONTEXT_ID_RRR));
    msg->SetServiceID(ISA_EMULATOR_SERVICE_ID);
    msg->SetMethodID(METHOD_ID_UPDATE_REG);
    msg->AppendUINT(rVal.intReg, sizeof(rVal));
    msg->AppendUINT(rName, 2);
    msg->AppendUINT(CONTEXT_ID_RRR(ctxId), sizeof(CONTEXT_ID_RRR));

    RRRClient->MakeRequestNoResponse(msg);
}


// ========================================================================
//
// Floating point opcode emulation.  Only the register inputs are copied
// from the FPGA.  The result register value is passed back.  The FPGA
// continues simulation and the timing model is unaware of this emulation.
//
// ========================================================================


// TEMPORARY: cheat and assign client method IDs
#define CMD_EMULATE_REGOP  0

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
    Cleanup();
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
    // cleanup
    Cleanup();
    
    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
ISA_REGOP_EMULATOR_SERVER_CLASS::Cleanup()
{
    delete emulator;

    // deallocate stubs
    delete serverStub;
}

typedef UINT32 ISA_INSTRUCTION;

// handle service request
UMF_MESSAGE
ISA_REGOP_EMULATOR_SERVER_CLASS::Request(UMF_MESSAGE req)
{
    CONTEXT_ID ctx_id;
    ISA_INSTRUCTION inst;
    FUNCP_VADDR pc;
    FUNCP_REG srcVal0;
    FUNCP_REG srcVal1;
    ISA_REG_INDEX_CLASS rNameSrc0;
    ISA_REG_INDEX_CLASS rNameSrc1;
    ISA_REG_INDEX_CLASS rNameDst;

    FUNCP_REG dstVal;
    UMF_MESSAGE resp;

    switch(req->GetMethodID())
    {
      case CMD_EMULATE_REGOP:
        rNameDst = req->ExtractUINT(2);
        rNameSrc1 = req->ExtractUINT(2);
        rNameSrc0 = req->ExtractUINT(2);
        srcVal1.intReg = req->ExtractUINT(sizeof(srcVal1));
        srcVal0.intReg = req->ExtractUINT(sizeof(srcVal0));
        pc = req->ExtractUINT(sizeof(pc));
        inst = req->ExtractUINT(sizeof(inst));
        ctx_id = CONTEXT_ID(req->ExtractUINT(sizeof(ctx_id)));
        delete req;

        if (TRACING(1))
        {
            T1("\tisa_regop_emulator: CTX " << UINT64(ctx_id) << ": " << fmt_regval(pc) << " " << fmt_inst(inst));
        }

        dstVal = emulator->EmulateRegOp(ctx_id, pc, inst,
                                        srcVal0, srcVal1,
                                        rNameSrc0, rNameSrc1, rNameDst);

        if (TRACING(1))
        {
            T1("\tisa_regop_emulator: CTX " << UINT64(ctx_id) << ": " << fmt_regval(pc)
               << "  Resp: " << fmt_regval(dstVal.intReg));
        }

        resp = new UMF_MESSAGE_CLASS;
        resp->SetLength(sizeof(dstVal));
        resp->SetMethodID(CMD_EMULATE_REGOP);
        resp->AppendUINT(dstVal.intReg, sizeof(dstVal));
        return resp;

      default:
        ASIMERROR("Unexpected command to ISA REGOP emulator");
    }
}


// ========================================================================
//
// Debug logger.
//
// ========================================================================


// TEMPORARY: cheat and assign client method IDs
#define CMD_NOTE_INSTR 0

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
    Cleanup();
}

// init
void
ISA_DP_DEBUG_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    parent = p;

    logFile.open("leap_debug/hasim_isa_dp_rrr.out", ios::out | ios::trunc);
}

// uninit: override
void
ISA_DP_DEBUG_SERVER_CLASS::Uninit()
{
    logFile.close();

    // cleanup
    Cleanup();
    
    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
ISA_DP_DEBUG_SERVER_CLASS::Cleanup()
{
    // deallocate stubs
    delete serverStub;
}

typedef UINT32 ISA_INSTRUCTION;

// handle service request
UMF_MESSAGE
ISA_DP_DEBUG_SERVER_CLASS::Request(UMF_MESSAGE req)
{
    CONTEXT_ID ctx_id;
    ISA_INSTRUCTION inst;
    FUNCP_VADDR pc;
    FUNCP_REG srcVal0;
    FUNCP_REG srcVal1;

    switch(req->GetMethodID())
    {
      case CMD_NOTE_INSTR:
        srcVal1.intReg = req->ExtractUINT(sizeof(FUNCP_REG));
        srcVal0.intReg = req->ExtractUINT(sizeof(FUNCP_REG));
        pc = req->ExtractUINT(sizeof(pc));
        inst = req->ExtractUINT(sizeof(inst));
        ctx_id = CONTEXT_ID(req->ExtractUINT(sizeof(ctx_id)));
        delete req;

        logFile << UINT64(ctx_id) << ": "
                << fmt_regval(pc) << " " << fmt_inst(inst) << " <- "
                << fmt_regval(srcVal0.intReg) << " "
                << fmt_regval(srcVal1.intReg) << endl;
        logFile.flush();
        return NULL;

      default:
        ASIMERROR("Unexpected command to ISA REGOP emulator");
    }
}
