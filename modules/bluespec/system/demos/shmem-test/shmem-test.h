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

#ifndef __SHMEM_TEST_SYSTEM__
#define __SHMEM_TEST_SYSTEM__

#include "platforms-module.h"
#include "asim/provides/hasim_controller.h"
#include "asim/provides/shared_memory.h"

#include "asim/rrr/client_stub_SHMEM_TEST.h"

// SHMEM_Test system

typedef class BLUESPEC_SYSTEM_CLASS* BLUESPEC_SYSTEM;
class BLUESPEC_SYSTEM_CLASS: public SYSTEM_CLASS,
                             public PLATFORMS_MODULE_CLASS
{
  private:

    // client stub
    SHMEM_TEST_CLIENT_STUB clientStub;

    // shared memory virtual device
    SHARED_MEMORY_CLASS sharedMemoryDevice;

  public:

    BLUESPEC_SYSTEM_CLASS(LLPI llpi);
    ~BLUESPEC_SYSTEM_CLASS();

    // main
    void Main();
};

#endif
