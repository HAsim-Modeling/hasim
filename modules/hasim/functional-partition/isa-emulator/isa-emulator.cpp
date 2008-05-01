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

#include "asim/syntax.h"
#include "asim/mesg.h"

#include "asim/rrr/service_ids.h"
#include "asim/provides/isa_emulator.h"

#define SERVICE_ID  ISA_EMULATOR_SERVICE_ID

// TEMPORARY: cheat and assign client method IDs
#define CMD_SYNC     0
#define CMD_EMULATE  1
#define METHOD_ID_UPDATE_REG    0

using namespace std;

// ===== service instantiation =====
ISA_EMULATOR_CLASS ISA_EMULATOR_CLASS::instance;

// constructor
ISA_EMULATOR_CLASS::ISA_EMULATOR_CLASS()
{
    SetTraceableName("funcp_memory");

    VERIFYX(sizeof(ISA_ADDRESS) == sizeof(FUNCP_ADDR));
    VERIFYX(sizeof(ISA_VALUE) == sizeof(FUNCP_INT_REG));

    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);

    emulator = new ISA_EMULATOR_IMPL_CLASS(this);
}

// destructor
ISA_EMULATOR_CLASS::~ISA_EMULATOR_CLASS()
{
    delete emulator;
}

// init
void
ISA_EMULATOR_CLASS::Init(
    PLATFORMS_MODULE p)
{
    parent = p;
}

// poll
void
ISA_EMULATOR_CLASS::Poll()
{
}

typedef UINT32 ISA_INSTRUCTION;

// handle service request
UMF_MESSAGE
ISA_EMULATOR_CLASS::Request(UMF_MESSAGE req)
{
    FUNCP_INT_REG rVal;
    FUNCP_ADDR pc;
    ISA_REG_INDEX_CLASS rName;
    ISA_INSTRUCTION inst;

    UMF_MESSAGE resp;

    switch(req->GetMethodID())
    {
      case CMD_SYNC:
        rVal = req->ExtractUINT(sizeof(rVal));
        rName = req->ExtractUINT(4);
        delete req;

        if (TRACING(1))
        {
            if (rName.IsArchReg())
            {
                T1("\tisa_emulator: Sync ArchReg " << rName.ArchRegNum() << ": 0x" << fmt_x(rVal));
            }
            if (rName.IsControlReg())
            {
                T1("\tisa_emulator: Sync ControlReg: 0x" << fmt_x(rVal));
            }
            if (rName.IsLockReg())
            {
                T1("\tisa_emulator: Sync LockReg: 0x" << fmt_x(rVal));
            }
            if (rName.IsLockAddrReg())
            {
                T1("\tisa_emulator: Sync LockAddrReg: 0x" << fmt_x(rVal));
            }
            if (rName.IsIllegalReg())
            {
                T1("\tisa_emulator: Illegal register number: " << UINT32(rName));
            }
        }

        emulator->SyncReg(rName, rVal);

        return NULL;
        break;

      case CMD_EMULATE:
        pc = req->ExtractUINT(sizeof(pc));
        inst = req->ExtractUINT(4);
        delete req;

        T1("\tisa_emulator: Emulate PC 0x" << fmt_x(pc) << ", Inst 0x" << fmt_x(inst));
        
        FUNCP_ADDR newPC;
        ISA_EMULATOR_RESULT r = emulator->Emulate(pc, inst, &newPC);

        //
        // Hack.  Result is sent in low 2 bits of the returned PC.  This works
        // for Alpha and MIPS until we get support for multiple returned objects
        // in RRR.
        //
        newPC ^= (newPC & 3);       // Clear low 2 bits
        switch (r)
        {
          case ISA_EMULATOR_NORMAL:
            break;

          case ISA_EMULATOR_BRANCH:
            newPC |= 1;
            break;

          case ISA_EMULATOR_EXIT_OK:
            newPC = 7;
            break;

          case ISA_EMULATOR_EXIT_FAIL:
            newPC = 3;
            break;

          default:
            ASIMERROR("Unexpected result from ISA emulator implementation");
        }

        resp = new UMF_MESSAGE_CLASS(8);
        resp->SetMethodID(CMD_EMULATE);
        resp->AppendUINT(newPC, sizeof(newPC));     // terminate
        return resp;
        break;
    }
}


// client: update register
void
ISA_EMULATOR_CLASS::UpdateRegister(ISA_REG_INDEX_CLASS rName, FUNCP_INT_REG rVal)
{
    // create message for RRR client
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(sizeof(rVal) + 4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_UPDATE_REG);
    msg->AppendUINT(rVal, sizeof(rVal));
    msg->AppendUINT32(rName);

    RRRClient->MakeRequestNoResponse(msg);
}
