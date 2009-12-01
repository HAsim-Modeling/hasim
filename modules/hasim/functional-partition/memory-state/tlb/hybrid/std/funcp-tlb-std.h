//
// Copyright (C) 2009 Intel Corporation
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

#ifndef __FUNCP_TLB__
#define __FUNCP_TLB__

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"

#include "asim/provides/rrr.h"
#include "asim/dict/VDEV.h"

// Get the data types from the functional memory RRR definition
#define TYPES_ONLY
#include "asim/rrr/server_stub_FUNCP_TLB.h"
#undef TYPES_ONLY

typedef class FUNCP_TLB_SERVER_CLASS* FUNCP_TLB_SERVER;

class FUNCP_TLB_SERVER_CLASS: public RRR_SERVER_CLASS,
                              public PLATFORMS_MODULE_CLASS,
                              public TRACEABLE_CLASS
{
  private:
    // self-instantiation
    static FUNCP_TLB_SERVER_CLASS instance;

    // stubs
    RRR_SERVER_STUB serverStub;

    Format fmt_addr;
    Format fmt_data;

  public:
    FUNCP_TLB_SERVER_CLASS();
    ~FUNCP_TLB_SERVER_CLASS();

    // generic RRR methods
    void   Init(PLATFORMS_MODULE);
    void   Uninit();
    void   Cleanup();

    // RRR request methods
    OUT_TYPE_VtoP VtoP(CONTEXT_ID ctxId, MEM_VALUE va, UINT8 reqWordIdx);
};

// Now that the server class is defined the RRR wrapper can be loaded.
#include "asim/rrr/server_stub_FUNCP_TLB.h"

#endif
