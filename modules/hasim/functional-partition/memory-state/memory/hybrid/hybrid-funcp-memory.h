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

    //
    // Current state of a pipelined store line
    //
    UINT8 stWordIdx;
    UINT8 stWordValid;
    UINT8 stSendAck;
    CONTEXT_ID stCtxId;
    FUNCP_PADDR stAddr;
    MEM_CACHELINE stData;

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
    MEM_VALUE Load(CONTEXT_ID ctxId, FUNCP_PADDR addr);
    void Store(CONTEXT_ID ctxId, FUNCP_PADDR addr, MEM_VALUE val);

    void LoadLine(CONTEXT_ID ctxId, FUNCP_PADDR addr);

    void StoreLine(CONTEXT_ID ctxId, UINT8 wordValid, UINT8 sendAck, FUNCP_PADDR addr);
    void StoreData(MEM_VALUE val);


    //
    // Incoming messages from the software side (e.g. instruction emulation)
    // that may cause changes in the FPGA-side cache of simulated memory.
    //
    static void NoteSystemMemoryRead(CONTEXT_ID ctxId, FUNCP_PADDR addr, FUNCP_PADDR size);
    static void NoteSystemMemoryWrite(CONTEXT_ID ctxId, FUNCP_PADDR addr, FUNCP_PADDR size);

    void SystemMemoryRef(CONTEXT_ID ctxId, FUNCP_PADDR addr, UINT64 size, bool isWrite);

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
