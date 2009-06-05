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
#include <strings.h>
#include <string>
#include <iostream>
#include <cmath>

#include "asim/rrr/service_ids.h"
#include "asim/provides/hasim_common.h"
#include "asim/provides/command_switches.h"

#include "asim/provides/debug_scan_controller.h"

using namespace std;


// ===== service instantiation =====
DEBUG_SCAN_SERVER_CLASS DEBUG_SCAN_SERVER_CLASS::instance;

// ===== methods =====

// constructor
DEBUG_SCAN_SERVER_CLASS::DEBUG_SCAN_SERVER_CLASS()
{
    // instantiate stubs
    serverStub = new DEBUG_SCAN_SERVER_STUB_CLASS(this);
}


// destructor
DEBUG_SCAN_SERVER_CLASS::~DEBUG_SCAN_SERVER_CLASS()
{
    Cleanup();
}


// init
void
DEBUG_SCAN_SERVER_CLASS::Init(
    PLATFORMS_MODULE p)
{
    // set parent pointer
    parent = p;
}


// uninit: we have to write this explicitly
void
DEBUG_SCAN_SERVER_CLASS::Uninit()
{
    Cleanup();

    // chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// cleanup
void
DEBUG_SCAN_SERVER_CLASS::Cleanup()
{
    // kill stubs
    delete serverStub;
}

//
// RRR request methods
//

//
// Send --
//     Receive a debug scan packet.
//
void
DEBUG_SCAN_SERVER_CLASS::Send(
    UINT32 id,
    UINT8 value)
{
    VERIFY(id < DEBUG_SCAN_DICT_ENTRIES, "debug-scan-controller:  Invalid id");
    cout << "DEBUG SCAN " << DEBUG_SCAN_DICT::Name(id) << " " << UINT32(value) << endl;
}


//
// Done --
//     The simple arrival of the done request means all the data has arrived.
//
UINT8
DEBUG_SCAN_SERVER_CLASS::Done(
    UINT8 syn)
{
    // Return ACK
    return 1;
}


// poll
void
DEBUG_SCAN_SERVER_CLASS::Poll()
{
}
