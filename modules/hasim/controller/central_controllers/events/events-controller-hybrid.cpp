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

#include "asim/provides/events_controller.h"
#include "asim/rrr/service_ids.h"

#include "asim/dict/EVENTS.h"

using namespace std;

// ===== service instantiation =====
EVENTS_SERVER_CLASS EVENTS_SERVER_CLASS::instance;

// ===== methods =====

// constructor
EVENTS_SERVER_CLASS::EVENTS_SERVER_CLASS()
{
    // instantiate stubs
    serverStub = new EVENTS_SERVER_STUB_CLASS(this);
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
    
    // Open the output file
#ifdef HASIM_EVENTS_ENABLED
    eventFile = fopen("hasim_events.out", "w+");
#else
    eventFile = NULL;
#endif    
}

// uninit: we have to write this explicitly
void
EVENTS_SERVER_CLASS::Uninit()
{
    Cleanup();

    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
EVENTS_SERVER_CLASS::Cleanup()
{
    if (eventFile != NULL)
    {
        fclose(eventFile);
    }

    // kill stubs
    delete serverStub;
}

//
// RRR request methods
//
void
EVENTS_SERVER_CLASS::LogEvent(
    UINT32 event_id,
    UINT32 event_data,
    UINT32 model_cc)
{
    // lookup event name from dictionary
    const char *event_name = EVENTS_DICT::Str(event_id);
    if (event_name == NULL)
    {
        cerr << "streams: invalid event_id: " << event_id << endl;
        CallbackExit(1);
    }

#ifndef HASIM_EVENTS_ENABLED
    ASIMERROR("Event id " << event_id << " (" << event_name << ") received but events are disabled");
#endif
    ASSERTX(eventFile != NULL);

    // write to file
    // eventually this will be replaced with calls to DRAL.
    fprintf(eventFile, "[%010u]: %s: %u\n", model_cc, event_name, event_data);
}
