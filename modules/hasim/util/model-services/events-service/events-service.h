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

#ifndef _EVENTS_CONTROLLER_
#define _EVENTS_CONTROLLER_

#include <iostream>

#include "platforms-module.h"
#include "asim/provides/rrr.h"

#include "asim/rrr/client_stub_EVENTS.h"

// this module handles events and will eventually interact with DRAL.

typedef class EVENTS_SERVER_CLASS* EVENTS_SERVER;

class EVENTS_SERVER_CLASS: public RRR_SERVER_CLASS,
                           public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static EVENTS_SERVER_CLASS instance;
        
    // stubs
    EVENTS_CLIENT_STUB clientStub;
    RRR_SERVER_STUB serverStub;

    // File for output until we use DRAL.
    ofstream eventFile;
    
    UINT64 **cycles;

  public:
    EVENTS_SERVER_CLASS();
    ~EVENTS_SERVER_CLASS();
    
    // static methods
    static EVENTS_SERVER GetInstance() { return &instance; }

    // Client methods
    void EnableEvents();
    void DisableEvents();

    // Annotate file with model-specific metadata
    void ModelSpecific(const char *name,
                       const char *descr,
                       UINT64 val);
    void ModelSpecific(const char *name,
                       const char *descr,
                       UINT32 nEntries,
                       UINT32 *val);
    void ModelSpecific(const char *name,
                       const char *descr,
                       UINT32 nEntries,
                       UINT64 *val);

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();

    // RRR service methods
    void LogInit(UINT32 event_id, UINT32 max_event_iid);

    void LogEvent(UINT32 event_id,
                  UINT32 event_iid,
                  UINT32 event_data,
                  UINT32 model_cc);

    void LogCycles(UINT32 event_id,
                   UINT32 event_iid,
                   UINT32 model_cc);
};

// server stub
#include "asim/rrr/server_stub_EVENTS.h"

// all functionalities of the event controller are completely implemented
// by the EVENTS_SERVER
typedef EVENTS_SERVER_CLASS EVENTS_CONTROLLER_CLASS;

#endif
