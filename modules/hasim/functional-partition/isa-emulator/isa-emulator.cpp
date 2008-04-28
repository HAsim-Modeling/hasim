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
#define METHOD_ID_EMULATION_FINISHED  1

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

#if (MEMORY_VALUE_BYTES == 4)
typedef UINT32 ISA_VALUE;
#else
typedef UINT64 ISA_VALUE;
#endif

#if (MEMORY_ADDR_BYTES == 4)
typedef UINT32 ISA_ADDRESS;
#else
typedef UINT64 ISA_ADDRESS;
#endif

typedef UINT8 ISA_REG_INDEX;

typedef UINT32 ISA_INSTRUCTION;

// handle service request
UMF_MESSAGE
ISA_EMULATOR_CLASS::Request(UMF_MESSAGE req)
{
    ISA_VALUE rval;
    ISA_REG_INDEX rname;
    ISA_ADDRESS pc;
    ISA_INSTRUCTION inst;

    switch(req->GetMethodID())
    {
        case CMD_SYNC:
            rval = req->ExtractUINT(8);
            rname = req->ExtractUINT(1);
            cout << "RRR Sync: reg-index: " << rname << " reg-val: " << hex << rval << endl; 
            break;
        case CMD_EMULATE:
            pc = req->ExtractUINT(8);
            inst = req->ExtractUINT(4);
            cout << "RRR Emulate: pc: " << pc << " inst: " << hex << inst << endl;
            break;
    }
}


/*
// handle service request
bool
ISA_EMULATOR_CLASS::Request(
    UINT32 arg0,
    UINT32 arg1,
    UINT32 arg2,
    UINT32 arg3,
    UINT32 *result)
{
    switch (arg0)
    {
        // Here we should call a handler rather than just doing a printf.
        case METHOD_ID_SYNC_REG:
            cout << "isa emulator: received sync reg request: reg " 
                 << arg1 << " <= " << arg2 << "\n";
            break;

        case METHOD_ID_EMULATE_INST:
            cout << "isa emulator: received emulate inst request: inst: "
                 << arg1 << " cur pc: " << arg2 << "\n";
            ASIMERROR("ISA Emulation not supported");
            break;

        default:
            cerr << "isa emulator: invalid methodID." << endl;
            CallbackExit(1);
            break;
    }

    return false;
}
*/

// client: update register
void
ISA_EMULATOR_CLASS::updateRegister(UINT32 rname, UINT32 rval)
{
    // create message for RRR client
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(8);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_UPDATE_REG);
    msg->AppendUINT32(rname);
    msg->AppendUINT32(rval);

    RRRClient->MakeRequestNoResponse(msg);
}

// client: emulation finished
void
ISA_EMULATOR_CLASS::emulationFinished()
{
    // create message for RRR client
    UMF_MESSAGE msg = new UMF_MESSAGE_CLASS(4);
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_EMULATION_FINISHED);
    msg->AppendUINT32(0);   // value doesn't matter

    RRRClient->MakeRequestNoResponse(msg);
}
