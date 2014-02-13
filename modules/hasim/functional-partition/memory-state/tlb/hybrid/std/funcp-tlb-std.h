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

    FUNCP_SIMULATED_MEMORY memory;

    UINT32 maxContexts;
    MEM_VALUE *dActivateAddrs;
    MEM_VALUE *iActivateAddrs;

  public:
    FUNCP_TLB_SERVER_CLASS();
    ~FUNCP_TLB_SERVER_CLASS();

    // generic RRR methods
    void   Init(PLATFORMS_MODULE);
    void   Uninit();
    void   Cleanup();

    // RRR request methods
    OUT_TYPE_VtoP VtoP(CONTEXT_ID ctxId, MEM_VALUE va, UINT8 reqWordIdx);
    void ActivateVAddr(CONTEXT_ID ctxId, MEM_VALUE va, UINT8 isITranslate);
};

// Now that the server class is defined the RRR wrapper can be loaded.
#include "asim/rrr/server_stub_FUNCP_TLB.h"

#endif
