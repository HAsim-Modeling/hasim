#ifndef __RRRTEST_SERVER__
#define __RRRTEST_SERVER__

#include <stdio.h>
#include <sys/time.h>

#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/rrr.h"

// Get the data types from the server stub
#define TYPES_ONLY
#include "asim/rrr/server_stub_RRRTEST.h"
#undef TYPES_ONLY

// this module provides the RRRTest server functionalities
typedef class RRRTEST_SERVER_CLASS* RRRTEST_SERVER;

class RRRTEST_SERVER_CLASS: public RRR_SERVER_CLASS,
                            public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static RRRTEST_SERVER_CLASS instance;

    // server stub
    RRR_SERVER_STUB serverStub;

  public:
    RRRTEST_SERVER_CLASS();
    ~RRRTEST_SERVER_CLASS();

    // static methods
    static RRRTEST_SERVER GetInstance() { return &instance; }

    // required RRR methods
    void Init(PLATFORMS_MODULE);
    void Uninit();
    void Cleanup();

    //
    // RRR service methods
    //
    void   F2HOneWayMsg1(UINT64 payload);

    void   F2HOneWayMsg8(UINT64 payload0,
                         UINT64 payload1,
                         UINT64 payload2,
                         UINT64 payload3,
                         UINT64 payload4,
                         UINT64 payload5,
                         UINT64 payload6,
                         UINT64 payload7);

    void  F2HOneWayMsg16(UINT64 payload0,
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

    void  F2HOneWayMsg32(UINT64 payload0,
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
                         UINT64 payload15,
                         UINT64 payload16,
                         UINT64 payload17,
                         UINT64 payload18,
                         UINT64 payload19,
                         UINT64 payload20,
                         UINT64 payload21,
                         UINT64 payload22,
                         UINT64 payload23,
                         UINT64 payload24,
                         UINT64 payload25,
                         UINT64 payload26,
                         UINT64 payload27,
                         UINT64 payload28,
                         UINT64 payload29,
                         UINT64 payload30,
                         UINT64 payload31);

    UINT64 F2HTwoWayMsg1(UINT64 payload);

    OUT_TYPE_F2HTwoWayMsg16 F2HTwoWayMsg16(UINT64 payload0,
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
#include "asim/rrr/server_stub_RRRTEST.h"

#endif
