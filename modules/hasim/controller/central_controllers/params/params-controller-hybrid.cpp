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

//
// @file params-controller-hybrid.cpp
// @brief Pass dynamic parameters to the hardware side
//
// @author Michael Adler
//

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <iostream>

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/rrr/service_ids.h"
#include "asim/provides/command_switches.h"
#include "asim/provides/params_controller.h"

#include "asim/dict/PARAMS.h"

#define SERVICE_ID       PARAMS_SERVICE_ID

// temporary
#define METHOD_ID_SEND_PARAM 0

//
// This code builds parallel arrays for mapping dynamic parameter values to
// dictionary entries.  Two arrays are built instead of using a struct to save
// space since values pointers are 64 bits and dictionary entries are 32 bits.
//
// sim_config.h is built by hasim-configure and depends on macros to give
// information about parameters.  We define the macro multiple times to build
// the table.
//
#undef Register
#undef Declare
#undef RegisterDyn

//
// Dictionary ID array
//
#define RegisterDynDict(VAR,DICT_ENTRY) DICT_ENTRY,
static UINT32 paramDictIDs[] =
{
#include "asim/provides/sim_config.h"
0
};

//
// extern declarations for all dynamic parameter variables
//
#undef RegisterDynDict
#define RegisterDynDict(VAR,DICT_ENTRY) extern UINT64 VAR;
#include "asim/provides/sim_config.h"

//
// Dynamic parameter pointer array
//
#undef RegisterDynDict
#define RegisterDynDict(VAR,DICT_ENTRY) &VAR,
typedef UINT64 *DYN_PARAM_PTR;
static DYN_PARAM_PTR paramValues[] =
{
#include "asim/provides/sim_config.h"
NULL
};

using namespace std;

// ===== service instantiation =====
PARAMS_CONTROLLER_CLASS PARAMS_CONTROLLER_CLASS::instance;

// ===== methods =====

// constructor
PARAMS_CONTROLLER_CLASS::PARAMS_CONTROLLER_CLASS()
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
}

// destructor
PARAMS_CONTROLLER_CLASS::~PARAMS_CONTROLLER_CLASS()
{
}

// init
void
PARAMS_CONTROLLER_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;
}

// uninit: we have to write this explicitly
void
PARAMS_CONTROLLER_CLASS::Uninit()
{
    // simply chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// request
UMF_MESSAGE
PARAMS_CONTROLLER_CLASS::Request(
    UMF_MESSAGE request)
{
    ASIMERROR("PARAMS software side provides no services");
}

void
PARAMS_CONTROLLER_CLASS::SendAllParams()
{
    UMF_MESSAGE msg;

    UINT32 i = 0;
    while (paramValues[i])
    {
        msg = new UMF_MESSAGE_CLASS(sizeof(UINT32) + sizeof(UINT64));
        msg->SetServiceID(SERVICE_ID);
        msg->SetMethodID(METHOD_ID_SEND_PARAM);
        msg->AppendUINT64(*paramValues[i]);
        msg->AppendUINT32(paramDictIDs[i]);

        RRRClient->MakeRequestNoResponse(msg);

        i += 1;
    }

    //
    // Send NULL parameter as the last token.  Hardware responds with an ACK
    // to this one so we know everything is done.
    //
    msg = new UMF_MESSAGE_CLASS(sizeof(UINT32) + sizeof(UINT64));
    msg->SetServiceID(SERVICE_ID);
    msg->SetMethodID(METHOD_ID_SEND_PARAM);
    msg->AppendUINT64(0);
    msg->AppendUINT32(PARAMS_NULL);

    UMF_MESSAGE resp = RRRClient->MakeRequest(msg);
    delete resp;
}
