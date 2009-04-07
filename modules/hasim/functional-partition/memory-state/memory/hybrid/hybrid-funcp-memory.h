//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
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
};

// server stub
#define BYPASS_SERVER_STUB
#include "asim/rrr/server_stub_FUNCP_MEMORY.h"
#undef  BYPASS_SERVER_STUB

// hack: required because m5 ISA_EMULATOR uses the name FUNCP_MEMORY_CLASS: FIXME
typedef FUNCP_MEMORY_SERVER_CLASS FUNCP_MEMORY_CLASS;

#endif
