#ifndef __RRRTEST_SYSTEM__
#define __RRRTEST_SYSTEM__

#include "asim/provides/virtual_platform.h"
#include "asim/rrr/client_stub_RRRTEST.h"

// RRRTest system

typedef class HYBRID_APPLICATION_CLASS* HYBRID_APPLICATION;
class HYBRID_APPLICATION_CLASS
{
  private:

    // client stub
    RRRTEST_CLIENT_STUB clientStub;

  public:

    HYBRID_APPLICATION_CLASS(VIRTUAL_PLATFORM vp);
    ~HYBRID_APPLICATION_CLASS();

    // main
    void Init();
    void Main();
};

#endif
