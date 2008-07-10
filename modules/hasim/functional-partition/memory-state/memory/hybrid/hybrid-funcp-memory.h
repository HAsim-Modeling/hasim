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


class FUNCP_MEMORY_CLASS: public RRR_SERVICE_CLASS,
                          public PLATFORMS_MODULE_CLASS,
                          public TRACEABLE_CLASS
{
  private:
    // self-instantiation
    static FUNCP_MEMORY_CLASS instance;

    FUNCP_SIMULATED_MEMORY memory;

    Format fmt_addr;
    Format fmt_data;

  public:
    FUNCP_MEMORY_CLASS();
    ~FUNCP_MEMORY_CLASS();

    void    Init(PLATFORMS_MODULE);
    void    Uninit();
    void    Cleanup();
    UMF_MESSAGE Request(UMF_MESSAGE);
    void    Poll();

    //
    // Incoming messages from the software side (e.g. instruction emulation)
    // that may cause changes in the FPGA-side cache of simulated memory.
    //
    static void NoteSystemMemoryRead(MEM_ADDRESS addr, MEM_ADDRESS size);
    static void NoteSystemMemoryWrite(MEM_ADDRESS addr, MEM_ADDRESS size);
    static void NoteSystemMemoryWriteUnknownAddr();

    void SystemMemoryRef(MEM_ADDRESS addr, UINT64 size, bool isWrite);
    void InvalidateAllCaches();
};

#endif
