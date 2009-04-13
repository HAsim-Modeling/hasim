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
 
#ifndef __FUNCP_MEMORY__
#define __FUNCP_MEMORY__

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"

#include "asim/provides/rrr.h"
#include "asim/provides/funcp_base_types.h"
#include "asim/provides/funcp_simulated_memory.h"

// types

typedef FUNCP_PADDR     MEM_ADDRESS;
typedef FUNCP_INT_REG   MEM_VALUE;

typedef struct {
    char _x[FUNCP_CACHELINE_BITS/8];
} MEM_CACHELINE;

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

    FUNCP_SIMULATED_MEMORY memory;

    Format fmt_addr;
    Format fmt_data;

  public:
    FUNCP_MEMORY_SERVER_CLASS();
    ~FUNCP_MEMORY_SERVER_CLASS();

    void    Init(PLATFORMS_MODULE);
    void    Uninit();
    void    Cleanup();
    void    Poll();

    //
    // RRR Service Methods
    //
    UMF_MESSAGE Request(UMF_MESSAGE);

    //
    // Incoming messages from the software side (e.g. instruction emulation)
    // that may cause changes in the FPGA-side cache of simulated memory.
    //
    static void NoteSystemMemoryRead(CONTEXT_ID ctxId, MEM_ADDRESS addr, MEM_ADDRESS size);
    static void NoteSystemMemoryWrite(CONTEXT_ID ctxId, MEM_ADDRESS addr, MEM_ADDRESS size);

    void SystemMemoryRef(CONTEXT_ID ctxId, MEM_ADDRESS addr, UINT64 size, bool isWrite);

    // Other code may need to talk to simulated memory.  We should fix this hack
    // and have simulated memory allocated by a parent of all the classes that
    // need this handle.
    static FUNCP_SIMULATED_MEMORY GetMemoryHandle();
};

// server stub
#define BYPASS_SERVER_STUB
#include "asim/rrr/server_stub_FUNCP_MEMORY.h"
#undef  BYPASS_SERVER_STUB

// hack: required because m5 ISA_EMULATOR uses the name FUNCP_MEMORY_CLASS: FIXME
typedef FUNCP_MEMORY_SERVER_CLASS FUNCP_MEMORY_CLASS;

#endif
