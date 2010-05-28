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
EVENTS_SERVER_CLASS::EVENTS_SERVER_CLASS()
#ifdef HASIM_EVENTS_ENABLED
    : eventFile("hasim_events.out")
#endif
{
    // instantiate stubs
    serverStub = new EVENTS_SERVER_STUB_CLASS(this);
    clientStub = new EVENTS_CLIENT_STUB_CLASS(this);

#ifdef HASIM_EVENTS_ENABLED
    eventFile.fill('0');
#endif
}

// destructor
EVENTS_SERVER_CLASS::~EVENTS_SERVER_CLASS()
{
    Cleanup();
}

// init
void
EVENTS_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    // set parent pointer
    parent = p;

    cycles = new UINT64[EVENTS_DICT_ENTRIES];
    for (int i = 0; i < EVENTS_DICT_ENTRIES; i++)
    {
        // Event cycle numbers always come with 1 added to them, so start cycle
        // counter at -1.
        cycles[i] = ~0;
    }

    //
    // Emit all the event name mappings as a header so that the event records
    // can be encoded smaller.
    //
    eventFile << "# Key format:         +,<name>,<description>,<event id>" << endl;
    eventFile << "# Key model specifc:  -,<name>,<description>,<event id>" << endl;
    eventFile << "# Log format:         <cycle>,<event id>,<data>" << endl;

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
    Cleanup();
    delete[] cycles;

    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
EVENTS_SERVER_CLASS::Cleanup()
{
    // kill stubs
    delete serverStub;
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
inline void
EVENTS_SERVER_CLASS::LogCycles(
    UINT32 event_id,
    UINT32 model_cc)
{
    ASSERTX(event_id < EVENTS_DICT_ENTRIES);
    cycles[event_id] += (model_cc + 1);
}


void
EVENTS_SERVER_CLASS::LogEvent(
    UINT32 event_id,
    UINT32 event_data,
    UINT32 model_cc)
{
    ASSERTX(event_id < EVENTS_DICT_ENTRIES);
    cycles[event_id] += (model_cc + 1);

#ifndef HASIM_EVENTS_ENABLED
    ASIMERROR("Event " << EVENTS_DICT::Name(event_id) << " (" << EVENTS_DICT::Str(event_id) << ") received but events are disabled");
#endif

    // write to file
    // eventually this will be replaced with calls to DRAL.
    eventFile.width(10);
    eventFile << cycles[event_id];

    eventFile.width(0);
    eventFile << "," << event_id
              << "," << event_data
              << endl;
}
