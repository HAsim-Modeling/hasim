#ifndef __RRRTEST_SERVER__
#define __RRRTEST_SERVER__

#include <stdio.h>
#include <sys/time.h>

#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/rrr.h"

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
    void Poll();

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
    UINT64 F2HTwoWayMsg(UINT64 payload);
};

// include server stub
#include "asim/rrr/server_stub_RRRTEST.h"

#endif
