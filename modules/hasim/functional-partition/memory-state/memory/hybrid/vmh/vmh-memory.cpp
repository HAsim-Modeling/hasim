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
 * @file vmh-memory.cpp
 * @brief Implementation of funcp_simulated_memory using VMH images.
 * @author Michael Adler
 */

#include <strings.h>
#include <string.h>
#include "asim/syntax.h"
#include "asim/mesg.h"

#include "asim/provides/command_switches.h"
#include "asim/provides/funcp_simulated_memory.h"


enum { MEM_SIZE = 1048576 };    // 1MB


FUNCP_SIMULATED_MEMORY_CLASS::FUNCP_SIMULATED_MEMORY_CLASS()
{
    memory = new UINT8[MEM_SIZE];
    ASSERT(memory != NULL, "Out of memory");

    bzero(memory, MEM_SIZE);

    // Load image
    char *benchmark = "program.vmh";
    if (globalArgs->FuncPlatformArgc() > 0)
    {
        benchmark = globalArgs->FuncPlatformArgv()[0];
    }

    bool s = vmh_load_image(benchmark, memory, MEM_SIZE);
    VERIFY(s, "Failed to load VMH image");
}


FUNCP_SIMULATED_MEMORY_CLASS::~FUNCP_SIMULATED_MEMORY_CLASS()
{
    delete memory;
}


bool
FUNCP_SIMULATED_MEMORY_CLASS::Read(
    UINT64 addr,
    UINT64 size,
    bool isSpeculative,
    void *dest)
{
    if (addr + size > MEM_SIZE)
    {
        // Might be bad path or speculative
        bzero(dest, size);
        ASSERT(isSpeculative, "VMF-MEMORY: Reference to illegal address");
        return false;
    }

    switch (size)
    {
      case 8:
        *(UINT64*)dest = *(UINT64*)(&memory[addr]);
        break;
      case 4:
        *(UINT32*)dest = *(UINT32*)(&memory[addr]);
        break;
      case 2:
        *(UINT16*)dest = *(UINT16*)(&memory[addr]);
        break;
      case 1:
        *(UINT8*)dest = *(UINT8*)(&memory[addr]);
        break;
      default:
        memcpy(dest, &memory[addr], size);
        break;
    }

    return true;
}


void
FUNCP_SIMULATED_MEMORY_CLASS::Write(
    UINT64 addr,
    UINT64 size,
    void *src)
{
    ASSERTX(addr + size <= MEM_SIZE);
    switch (size)
    {
      case 8:
        *(UINT64*)(&memory[addr]) = *(UINT64*)src;
        break;
      case 4:
        *(UINT32*)(&memory[addr]) = *(UINT32*)src;
        break;
      case 2:
        *(UINT16*)(&memory[addr]) = *(UINT16*)src;
        break;
      case 1:
        *(UINT8*)(&memory[addr]) = *(UINT8*)src;
        break;
      default:
        memcpy(&memory[addr], src, size);
        break;
    }
}
