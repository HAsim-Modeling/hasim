/*****************************************************************************
 * rrrtest.cpp
 *
 * Copyright (C) 2008 Intel Corporation
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

//
// @file rrrtest.cpp
// @brief RRR Test System
//
// @author Angshuman Parashar
//

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "asim/syntax.h"
#include "asim/ioformat.h"
#include "asim/rrr/service_ids.h"
#include "asim/provides/connected_application.h"
#include "asim/provides/clocks_device.h"
#include "asim/rrr/client_stub_CLOCKTEST.h"
#include "clock_test.h"

using namespace std;

// constructor
CONNECTED_APPLICATION_CLASS::CONNECTED_APPLICATION_CLASS(
    VIRTUAL_PLATFORM vp)
{
    // instantiate client stub
    clientStub = new CLOCKTEST_CLIENT_STUB_CLASS(NULL);
}

// destructor
CONNECTED_APPLICATION_CLASS::~CONNECTED_APPLICATION_CLASS()
{
    delete clientStub;
}

void
CONNECTED_APPLICATION_CLASS::Init()
{
}

// main
void
CONNECTED_APPLICATION_CLASS::Main()
{
    printf("Beginning Clock test.\n");
    fflush(stdout);
    for(int i = 0; i < (1<<20); i++) { 
      int result  = clientStub->test(i);
      if(i%1000 == 0) {
        printf("Clock Test @ %d\n", i);
	fflush(stdout);
      }
      if(i != result) {
        printf("Got %x, expected %x\n", result, i);
	fflush(stdout);
      }
    }

    printf("Finishing Clock test.\n");
    fflush(stdout);
}
