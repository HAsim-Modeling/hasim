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

#ifndef _EVENTS_CONTROLLER_
#define _EVENTS_CONTROLLER_

#include <stdio.h>

#include "platforms-module.h"
#include "asim/provides/rrr.h"

// this module handles events and will eventually interact with DRAL.

typedef class EVENTS_SERVER_CLASS* EVENTS_SERVER;

class EVENTS_SERVER_CLASS: public RRR_SERVER_CLASS,
                           public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static EVENTS_SERVER_CLASS instance;
        
    // stubs
    RRR_SERVER_STUB serverStub;

    // File for output until we use DRAL.
    FILE* eventFile;
    
  public:
    EVENTS_SERVER_CLASS();
    ~EVENTS_SERVER_CLASS();
    
    // static methods
    static EVENTS_SERVER GetInstance() { return &instance; }

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();

    // RRR service methods
    void LogEvent(UINT32 event_id, UINT32 event_data, UINT32 model_cc);
};

// server stub
#include "asim/rrr/server_stub_EVENTS.h"

// all functionalities of the event controller are completely implemented
// by the EVENTS_SERVER
typedef EVENTS_SERVER_CLASS EVENTS_CONTROLLER_CLASS;

#endif
