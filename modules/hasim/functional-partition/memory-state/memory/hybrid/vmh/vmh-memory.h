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
#include "vmh-utils.h"

typedef class FUNCP_SIMULATED_MEMORY_CLASS *FUNCP_SIMULATED_MEMORY;

class FUNCP_SIMULATED_MEMORY_CLASS
{
  public:
    //
    // Required public interface
    //

    FUNCP_SIMULATED_MEMORY_CLASS();
    ~FUNCP_SIMULATED_MEMORY_CLASS();

    void Read(UINT64 addr, UINT64 size, void *dest);
    void Write(UINT64 addr, UINT64 size, void *src);

    UINT64 VtoP(UINT64 va) { return va; };

  private:
    //
    // VMH-specific code...
    //
    UINT8 *memory;
};

#endif // __VMH_MEMORY__
