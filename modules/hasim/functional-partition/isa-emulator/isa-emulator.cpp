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

#include "isa-emulator-common.h"
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
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
}

// destructor
ISA_EMULATOR_CLASS::~ISA_EMULATOR_CLASS()
{
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

typedef UINT32 ISA_REG_INDEX;

typedef UINT32 ISA_INSTRUCTION;

// handle service request
UMF_MESSAGE
ISA_EMULATOR_CLASS::Request(UMF_MESSAGE req)
{
    FUNCP_INT_REG rval;
    ISA_REG_INDEX rname;
    FUNCP_ADDR pc;
    ISA_INSTRUCTION inst;

    UMF_MESSAGE resp;

    switch(req->GetMethodID())
    {
      case CMD_SYNC:
          rval = req->ExtractUINT(sizeof(rval));
          rname = req->ExtractUINT(4);
          cout << "RRR Sync: reg-index: " << rname << " reg-val: " << hex << rval << endl;
          delete req;
          return NULL;
          break;
      case CMD_EMULATE:
        pc = req->ExtractUINT(sizeof(pc));
        inst = req->ExtractUINT(4);
        cout << "RRR Emulate: pc: " << hex << pc << " inst: " << hex << inst << endl;
        delete req;

        resp = new UMF_MESSAGE_CLASS(8);
        resp->SetMethodID(CMD_EMULATE);
        resp->AppendUINT(7, sizeof(pc));     // terminate
        return resp;
        break;
    }
}


// client: update register
void
ISA_EMULATOR_CLASS::updateRegister(UINT32 rname, FUNCP_INT_REG rval)
{
    // create message for RRR client
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(8);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_UPDATE_REG);
    msg->AppendUINT32(rname);
    msg->AppendUINT(rval, sizeof(rval));

    RRRClient->MakeRequestNoResponse(msg);
}
