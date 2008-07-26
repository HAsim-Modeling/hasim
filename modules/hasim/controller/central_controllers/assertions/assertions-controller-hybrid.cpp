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

#include "asim/provides/hasim_common.h"
#include "asim/provides/assertions_controller.h"
#include "asim/rrr/service_ids.h"

#include "asim/dict/ASSERTIONS.h"

#define SERVICE_ID       ASSERTIONS_SERVICE_ID

using namespace std;

enum ASSERTION_SEVERITY
{
    ASSERT_NONE,
    ASSERT_MESSAGE,
    ASSERT_WARNING,
    ASSERT_ERROR
};


// ===== service instantiation =====
ASSERTIONS_CONTROLLER_CLASS ASSERTIONS_CONTROLLER_CLASS::instance;

// ===== methods =====

// constructor
ASSERTIONS_CONTROLLER_CLASS::ASSERTIONS_CONTROLLER_CLASS()
{
    // register with server's map table
    RRR_SERVER_CLASS::RegisterService(SERVICE_ID, &instance);
}

// destructor
ASSERTIONS_CONTROLLER_CLASS::~ASSERTIONS_CONTROLLER_CLASS()
{
}

// init
void
ASSERTIONS_CONTROLLER_CLASS::Init(
    PLATFORMS_MODULE     p)
{
    // set parent pointer
    parent = p;
    
    // Open the output file
    assertionsFile = fopen("hasim_assertions.out", "w+");
}

// uninit: we have to write this explicitly
void
ASSERTIONS_CONTROLLER_CLASS::Uninit()
{
    fclose(assertionsFile);

    // simply chain
    PLATFORMS_MODULE_CLASS::Uninit();
}

// request
bool
ASSERTIONS_CONTROLLER_CLASS::Request(
    UINT32 arg0,
    UINT32 arg1,
    UINT32 arg2,
    UINT32 arg3,
    UINT32 *result)
{
    // extract event ID, data, and modelCC
    UINT32 unused = arg0; // Reserved to be methodID later if needed.
    UINT32 assert_base = arg1;
    UINT32 fpga_cc = arg2;
    UINT32 assertions = arg3;

    //
    // Assertions come from hardware in groups as a bit vector.  Each element
    // of the vector is a 2-bit value, equivalent to an ASSERTION_SEVERITY.
    // The dictionary ID is assert_base + the index of the vector.
    //

    // Check each vector entry and generate messages
    for (int i = 0; i < ASSERTIONS_PER_NODE; i++)
    {
        UINT32 assert_id = assert_base + i;
        ASSERTION_SEVERITY severity = ASSERTION_SEVERITY((assertions >> i*2) & 3);

        if (severity != ASSERT_NONE)
        {
            // lookup event name from dictionary
            const char *assert_msg = ASSERTIONS_DICT::Str(assert_id);
            if (assert_msg == NULL)
            {
                cerr << "streams: " << ASSERTIONS_DICT::Str(assert_id)
                     << ": invalid assert_id: " << assert_id << endl;
                CallbackExit(1);
            }

            // write to file
            fprintf(assertionsFile, "[%010u]: %s\n", fpga_cc, assert_msg);
            fflush(assertionsFile);
    
            // if severity is great, end the simulation.
            if (severity > ASSERT_WARNING)
            {
                cerr << "ERROR: Fatal HAsim assertion failure.\n";
                cerr << "MESSAGE: " << assert_msg << "\n";
                CallbackExit(1);
            }
        }
    }

    // no RRR response
    return false;
}

// poll
void
ASSERTIONS_CONTROLLER_CLASS::Poll()
{
}


