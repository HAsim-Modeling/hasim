//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
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

#include "asim/provides/events_service.h"

#include "asim/dict/EVENTS.h"

using namespace std;

// ===== service instantiation =====
EVENTS_SERVER_CLASS EVENTS_SERVER_CLASS::instance;

// ===== methods =====

// constructor
EVENTS_SERVER_CLASS::EVENTS_SERVER_CLASS():
    uninitialized()
#ifdef HASIM_EVENTS_ENABLED
    ,
    eventFile("hasim_events.out")
#endif
{

    // instantiate stubs
    serverStub = new EVENTS_SERVER_STUB_CLASS(this);
    clientStub = new EVENTS_CLIENT_STUB_CLASS(this);
    uninitialized = 0;

#ifdef HASIM_EVENTS_ENABLED
    eventFile.fill('0');
#endif
}

// destructor
EVENTS_SERVER_CLASS::~EVENTS_SERVER_CLASS()
{
    delete serverStub;
    delete clientStub;
}

// init
void
EVENTS_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    // set parent pointer
    parent = p;

    // Allocate buckets for all event IDs.  Each event ID may have a vector
    // of buckets corresponding to instance IDs.
    cycles = new UINT64*[EVENTS_DICT_ENTRIES];
    for (int i = 0; i < EVENTS_DICT_ENTRIES; i++)
    {
        cycles[i] = NULL;
    }

    //
    // Emit all the event name mappings as a header so that the event records
    // can be encoded smaller.
    //
    eventFile << "# Key format:         +,<name>,<description>,<event id>" << endl;
    eventFile << "# Key model specifc:  -,<name>,<description>,<event id>" << endl;
    eventFile << "# Log format:         <cycle>,<event id>,<iid>,<data>" << endl;

    for (int i = 0; i < EVENTS_DICT_ENTRIES; i++)
    {
        const char *event_name = EVENTS_DICT::Name(i);
        const char *event_msg  = EVENTS_DICT::Str(i);
        if (event_name != NULL)
        {
            eventFile << "+," << event_name
                      << ",\"" << event_msg << "\""
                      << "," << i
                      << endl;
        }
    }

    if (ENABLE_EVENTS)
    {
        EnableEvents();
    }
    else
    {
        DisableEvents();
    }
}

// uninit: we have to write this explicitly
void
EVENTS_SERVER_CLASS::Uninit()
{
    if(!uninitialized.fetch_and_store(1))
    {

        for (int i = 0; i < EVENTS_DICT_ENTRIES; i++)
        {
            if (cycles[i] != NULL)
            {
                delete[] cycles[i];
            }
        }
        delete[] cycles;

        // chain
        PLATFORMS_MODULE_CLASS::Uninit();
    }
}

// EnableEvents
void
EVENTS_SERVER_CLASS::EnableEvents()
{
    clientStub->EnableEvents(1);
}

// DisableEvents
void
EVENTS_SERVER_CLASS::DisableEvents()
{
    clientStub->EnableEvents(0);
}


// Annotate file with model-specific metadata
void
EVENTS_SERVER_CLASS::ModelSpecific(
    const char *name,
    const char *descr,
    UINT64 val)
{
    eventFile << "-," << name
              << ",\"" << descr << "\""
              << "," << val
              << endl;
}

void
EVENTS_SERVER_CLASS::ModelSpecific(
    const char *name,
    const char *descr,
    UINT32 nEntries,
    UINT32 *val)
{
    eventFile << "-," << name
              << ",\"" << descr << "\"";
    for (UINT32 i = 0; i < nEntries; i++)
    {
        eventFile << "," << val[i];
    }
    eventFile << endl;
}

void
EVENTS_SERVER_CLASS::ModelSpecific(
    const char *name,
    const char *descr,
    UINT32 nEntries,
    UINT64 *val)
{
    eventFile << "-," << name
              << ",\"" << descr << "\"";
    for (UINT32 i = 0; i < nEntries; i++)
    {
        eventFile << "," << val[i];
    }
    eventFile << endl;
}



//
// RRR request methods
//
void
EVENTS_SERVER_CLASS::LogInit(
    UINT32 event_id,
    UINT32 max_event_iid)
{
    ASSERTX((cycles != NULL) && (cycles[event_id] == NULL));

    cycles[event_id] = new UINT64[max_event_iid + 1];
    for (int i = 0; i <= max_event_iid; i++)
    {
        // Event cycle numbers always come with 1 added to them, so start cycle
        // counter at -1.
        cycles[event_id][i] = ~0;
    }
}


inline void
EVENTS_SERVER_CLASS::LogCycles(
    UINT32 event_id,
    UINT32 event_iid,
    UINT32 model_cc)
{
    ASSERTX((event_id < EVENTS_DICT_ENTRIES) && (cycles[event_id] != NULL));
    cycles[event_id][event_iid] += (model_cc + 1);
}


void
EVENTS_SERVER_CLASS::LogEvent(
    UINT32 event_id,
    UINT32 event_iid,
    UINT32 event_data,
    UINT32 model_cc)
{
    ASSERTX((event_id < EVENTS_DICT_ENTRIES) && (cycles[event_id] != NULL));
    cycles[event_id][event_iid] += (model_cc + 1);

#ifndef HASIM_EVENTS_ENABLED
    ASIMERROR("Event " << EVENTS_DICT::Name(event_id) << " (" << EVENTS_DICT::Str(event_id) << ") received but events are disabled");
#endif

    // write to file
    // eventually this will be replaced with calls to DRAL.
    eventFile.width(10);
    eventFile << cycles[event_id][event_iid];

    eventFile.width(0);
    eventFile << "," << event_id
              << "," << event_iid
              << "," << event_data
              << endl;
}
