//
// Copyright (C) 2008 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/select.h>
#include <sys/types.h>
#include <signal.h>
#include <string>
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

        VERIFY(stat_id < STATS_DICT_ENTRIES, "stats-controller:  Invalid stat id");

        VERIFY(! sawStat.test(stat_id), "stats-controller: stat " << STATS_DICT::Name(stat_id) << " appears more than once in the HW");
        sawStat.set(stat_id);

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

        sawStat.reset();

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
            statsFile << "\"" << statStr << "\"," << statName << "," << statValues[i] << endl;
        }
    }

    statsFile.close();
}


void
StatsEmitFile()
{
    STATS_CONTROLLER_CLASS::GetInstance()->EmitFile();
}
