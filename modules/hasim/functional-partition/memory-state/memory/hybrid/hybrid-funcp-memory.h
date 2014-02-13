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
 
#ifndef __FUNCP_MEMORY__
#define __FUNCP_MEMORY__

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"

#include "asim/provides/rrr.h"
#include "asim/provides/funcp_base_types.h"
#include "asim/provides/funcp_simulated_memory.h"

#include "asim/rrr/client_stub_FUNCP_MEMORY.h"

// Get the data types from the functional memory RRR definition
#define TYPES_ONLY
#include "asim/rrr/server_stub_FUNCP_MEMORY.h"
#undef TYPES_ONLY

// types

//
// Cache line, described as words.
//
typedef struct {
    MEM_VALUE w[FUNCP_CACHELINE_BITS / FUNCP_ISA_INT_REG_SIZE];
}
MEM_CACHELINE;

// Valid bits corresponding to words in a cache line.  Bit 0 corresponds to the
// low word when viewed as a full cache line.
typedef UINT32 MEM_CACHELINE_WORD_VALID_MASK;

typedef class FUNCP_MEMORY_SERVER_CLASS* FUNCP_MEMORY_SERVER;

class FUNCP_MEMORY_SERVER_CLASS: public RRR_SERVER_CLASS,
                                 public PLATFORMS_MODULE_CLASS,
                                 public TRACEABLE_CLASS
{
  private:
    // self-instantiation
    static FUNCP_MEMORY_SERVER_CLASS instance;

    // stubs
    RRR_SERVER_STUB serverStub;
    FUNCP_MEMORY_CLIENT_STUB clientStub;

    FUNCP_SIMULATED_MEMORY memory;

    Format fmt_addr;
    Format fmt_data;

  public:
    FUNCP_MEMORY_SERVER_CLASS();
    ~FUNCP_MEMORY_SERVER_CLASS();

    void    Init(PLATFORMS_MODULE);
    void    Uninit();
    void    Cleanup();

    //
    // RRR Service Methods
    //
    OUT_TYPE_Load Load(FUNCP_PADDR addr, UINT8 isSpeculative);
    void Store(FUNCP_PADDR addr, MEM_VALUE val);

    OUT_TYPE_LoadLine LoadLine(FUNCP_PADDR addr, UINT8 isSpeculative);

    void StoreLine(UINT8 wordValid,
                   UINT8 sendAck,
                   FUNCP_PADDR addr,
                   MEM_VALUE word0, MEM_VALUE word1, MEM_VALUE word2, MEM_VALUE word3);

    //
    // Incoming messages from the software side (e.g. instruction emulation)
    // that may cause changes in the FPGA-side cache of simulated memory.
    //
    static void NoteSystemMemoryRead(CONTEXT_ID ctxId, FUNCP_PADDR addr, FUNCP_PADDR size);
    static void NoteSystemMemoryWrite(CONTEXT_ID ctxId, FUNCP_PADDR addr, FUNCP_PADDR size);

    void SystemMemoryRef(FUNCP_PADDR addr, UINT64 size, bool isWrite);

    // Other code may need to talk to simulated memory.  We should fix this hack
    // and have simulated memory allocated by a parent of all the classes that
    // need this handle.
    static FUNCP_SIMULATED_MEMORY GetMemoryHandle();
};

// server stub
#include "asim/rrr/server_stub_FUNCP_MEMORY.h"

// hack: required because m5 ISA_EMULATOR uses the name FUNCP_MEMORY_CLASS: FIXME
typedef FUNCP_MEMORY_SERVER_CLASS FUNCP_MEMORY_CLASS;

#endif
