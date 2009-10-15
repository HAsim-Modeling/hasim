/* INTEL CONFIDENTIAL
 * Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
 * copyright license under Intel copyrights to copy and distribute this code 
 * internally only. This code is provided "AS IS" with no support and with no 
 * warranties of any kind, including warranties of MERCHANTABILITY,
 * FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
 * By making any use of this code, Recipient agrees that no other licenses 
 * to any Intel patents, trade secrets, copyrights or other intellectual 
 * property rights are granted herein, and no other licenses shall arise by 
 * estoppel, implication or by operation of law. Recipient accepts all risks 
 * of use.
*/
 
/**
 * @file vmh-memory.h
 * @brief Implementation of funcp_simulated_memory using VMH images.
 * @author Michael Adler
 */

#ifndef __VMH_MEMORY__
#define __VMH_MEMORY__


#include "asim/syntax.h"
#include "asim/provides/funcp_simulated_memory.h"
#include "asim/provides/funcp_base_types.h"


typedef class FUNCP_SIMULATED_MEMORY_CLASS *FUNCP_SIMULATED_MEMORY;

// Response from VtoP
struct FUNCP_MEM_VTOP_RESP
{
    UINT64 pa;
    bool pageFault;    // Translation failed
    bool ioSpace;      // Reference is to uncacheable I/O space
};

class FUNCP_SIMULATED_MEMORY_CLASS
{
  public:
    //
    // Required public interface
    //

    FUNCP_SIMULATED_MEMORY_CLASS();
    ~FUNCP_SIMULATED_MEMORY_CLASS();

    void Read(CONTEXT_ID ctx_id, UINT64 addr, UINT64 size, void *dest);
    void Write(CONTEXT_ID ctx_id, UINT64 addr, UINT64 size, void *src);

    FUNCP_MEM_VTOP_RESP VtoP(CONTEXT_ID ctx_id, UINT64 va, bool allocOnFault)
    {
        FUNCP_MEM_VTOP_RESP resp;
        resp.pa = va;
        resp.pageFault = false;
        resp.ioSpace = false;
        return resp;
    };

  private:
    //
    // VMH-specific code...
    //
    UINT8 *memory;
};

#endif // __VMH_MEMORY__
