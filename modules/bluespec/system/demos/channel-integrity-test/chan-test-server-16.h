//
// Copyright (C) 2010 Intel Corporation
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

#ifndef __CHANTEST_SERVER__
#define __CHANTEST_SERVER__

#include <stdio.h>
#include <sys/time.h>

#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/rrr.h"

// Get the data types from the server stub
#define TYPES_ONLY
#include "asim/rrr/server_stub_CHANTEST.h"
#undef TYPES_ONLY

// This module provides the CHANTEST server
typedef class CHANTEST_SERVER_CLASS* CHANTEST_SERVER;

class CHANTEST_SERVER_CLASS: public RRR_SERVER_CLASS,
                             public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static CHANTEST_SERVER_CLASS instance;

    // server stub
    RRR_SERVER_STUB serverStub;

    UINT64 f2hRecvMsgs;
    UINT64 f2hRecvErrors;

    UINT64 h2fRecvErrors;
    UINT64 h2fRecvBitErrors;

  public:
    CHANTEST_SERVER_CLASS();
    ~CHANTEST_SERVER_CLASS();

    // static methods
    static CHANTEST_SERVER GetInstance() { return &instance; }

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();

    // Error checking
    UINT64 GetF2HRecvMsgCnt() const { return f2hRecvMsgs; };
    UINT64 GetF2HRecvErrCnt() const { return f2hRecvErrors; };

    UINT64 GetH2FRecvErrCnt() const { return h2fRecvErrors; };
    UINT64 GetH2FRecvBitErrCnt() const { return h2fRecvBitErrors; };

    //
    // RRR service methods
    //
    void F2HOneWayMsg16(UINT64 payload0,
                        UINT64 payload1,
                        UINT64 payload2,
                        UINT64 payload3,
                        UINT64 payload4,
                        UINT64 payload5,
                        UINT64 payload6,
                        UINT64 payload7,
                        UINT64 payload8,
                        UINT64 payload9,
                        UINT64 payload10,
                        UINT64 payload11,
                        UINT64 payload12,
                        UINT64 payload13,
                        UINT64 payload14,
                        UINT64 payload15);

    void H2FNoteError(UINT32 numBitsFlipped,
                      UINT32 chunkIdx,
                      UINT64 payload0,
                      UINT64 payload1,
                      UINT64 payload2,
                      UINT64 payload3,
                      UINT64 payload4,
                      UINT64 payload5,
                      UINT64 payload6,
                      UINT64 payload7,
                      UINT64 payload8,
                      UINT64 payload9,
                      UINT64 payload10,
                      UINT64 payload11,
                      UINT64 payload12,
                      UINT64 payload13,
                      UINT64 payload14,
                      UINT64 payload15);
};


// Include the server stub
#include "asim/rrr/server_stub_CHANTEST.h"

#endif
