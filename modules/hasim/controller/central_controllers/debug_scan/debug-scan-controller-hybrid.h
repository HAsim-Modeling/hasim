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

#ifndef _DEBUG_SCAN_CONTROLLER_
#define _DEBUG_SCAN_CONTROLLER_

#include <bitset>

#include "platforms-module.h"
#include "asim/provides/hasim_common.h"
#include "asim/provides/rrr.h"

#include "asim/dict/DEBUG_SCAN.h"

//
// Manage debug scan chain coming from the hardware.
//

typedef class DEBUG_SCAN_SERVER_CLASS* DEBUG_SCAN_SERVER;

class DEBUG_SCAN_SERVER_CLASS: public RRR_SERVER_CLASS,
                               public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static DEBUG_SCAN_SERVER_CLASS instance;

    // stubs
    RRR_SERVER_STUB serverStub;

  public:
    DEBUG_SCAN_SERVER_CLASS();
    ~DEBUG_SCAN_SERVER_CLASS();

    // static methods
    static DEBUG_SCAN_SERVER GetInstance() { return &instance; }

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();
    void Poll();

    // RRR service methods
    void  Send(UINT32 id, UINT8 value);
    UINT8 Done(UINT8 syn);
};

// server stub
#include "asim/rrr/server_stub_DEBUG_SCAN.h"

// all functionalities of the stats controller are completely implemented
// by the DEBUG_SCAN_SERVER class
typedef DEBUG_SCAN_SERVER_CLASS DEBUG_SCAN_CONTROLLER_CLASS;

#endif
