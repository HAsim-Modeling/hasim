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
PARAMS_SERVER_CLASS PARAMS_SERVER_CLASS::instance;

// ===== methods =====

// constructor
PARAMS_SERVER_CLASS::PARAMS_SERVER_CLASS()
{
    // instantiate stubs
    clientStub = new PARAMS_CLIENT_STUB_CLASS(this);
}

// destructor
PARAMS_SERVER_CLASS::~PARAMS_SERVER_CLASS()
{
    Cleanup();
}

// init
void
PARAMS_SERVER_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;
}

// uninit: we have to write this explicitly
void
PARAMS_SERVER_CLASS::Uninit()
{
    Cleanup();

    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
PARAMS_SERVER_CLASS::Cleanup()
{
    // delete stubs
    delete clientStub;
}

// client method
void
PARAMS_SERVER_CLASS::SendAllParams()
{
    UMF_MESSAGE msg;

    UINT32 i = 0;
    while (paramValues[i])
    {
        UINT8 ack = clientStub->sendParam(paramDictIDs[i], *paramValues[i]);
        i += 1;
    }
}
