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

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <iostream>

#include "asim/rrr/service_ids.h"
#include "asim/provides/command_switches.h"

#include "asim/provides/stats_controller.h"


#define SERVICE_ID       STATS_SERVICE_ID

// temporary
#define METHOD_ID_SEND 0
#define METHOD_ID_DONE 1

using namespace std;

// ===== service instantiation =====
STATS_CONTROLLER_CLASS STATS_CONTROLLER_CLASS::instance;

// ===== methods =====

// constructor
STATS_CONTROLLER_CLASS::STATS_CONTROLLER_CLASS()
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);

    bzero(statValues, sizeof(statValues));
}


// destructor
STATS_CONTROLLER_CLASS::~STATS_CONTROLLER_CLASS()
{
}


// init
void
STATS_CONTROLLER_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;
}


// uninit: we have to write this explicitly
void
STATS_CONTROLLER_CLASS::Uninit()
{
    // simply chain
    PLATFORMS_MODULE_CLASS::Uninit();
}


// request
UMF_MESSAGE
STATS_CONTROLLER_CLASS::Request(
    UMF_MESSAGE request)
{

    // extract event ID, data, and modelCC
    UINT32 methodID  = request->GetMethodID();

    // decode
    if (methodID == METHOD_ID_SEND)
    {
        UINT32 stat_data = request->ExtractUINT32();
        UINT32 stat_id   = request->ExtractUINT32();

        //
        // Add new value to running total
        //

        ASSERT(stat_id < STATS_DICT_ENTRIES, "stats-controller:  Invalid stat id");

        statValues[stat_id] += stat_data;
        
        // free
        request->Delete();

        // no RRR response
        return NULL;
    }
    else if (methodID == METHOD_ID_DONE)
    {
        // free request
        request->Delete();

        // prepare response
        UMF_MESSAGE response = UMF_MESSAGE_CLASS::New();
        response->SetLength(4);
        response->SetMethodID(METHOD_ID_DONE);
        response->AppendUINT32(0);

        // send response
        return response;
    }
    else
    {
        // error
        cerr << "stats: invalid methodID\n" << flush;

        // free
        request->Delete();

        // exit
        CallbackExit(1);
    }
}


// poll
void
STATS_CONTROLLER_CLASS::Poll()
{
}


//
// EmitStatsFile --
//    Dump the in-memory statistics to a file.
//
void
STATS_CONTROLLER_CLASS::EmitFile()
{
    // Open the output file
    string statsFileName = string(globalArgs->Workload()) + ".stats";
    ofstream statsFile(statsFileName.c_str());

    if (! statsFile.is_open())
    {
        cerr << "Failed to open stats file: " << statsFile << endl;
        ASIMERROR("Can't dump statistics");
    }

    for (unsigned int i = 0; i < STATS_DICT_ENTRIES; i++)
    {
        // lookup event name from dictionary
        const char *statName = STATS_DICT::Name(i);
        const char *statStr  = STATS_DICT::Str(i);

        if ((i != STATS_NULL) && (statName != NULL))
        {
            statsFile << statName << "," << statStr << "," << statValues[i] << endl;
        }
    }

    statsFile.close();
}


void
StatsEmitFile()
{
    STATS_CONTROLLER_CLASS::GetInstance()->EmitFile();
}
