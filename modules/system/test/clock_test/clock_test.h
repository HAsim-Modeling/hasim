#ifndef __CLOCKTEST_SYSTEM__
#define __CLOCKTEST_SYSTEM__

#include "asim/provides/virtual_platform.h"
#include "platforms-module.h"
#include "asim/rrr/client_stub_CLOCKTEST.h"


typedef class CONNECTED_APPLICATION_CLASS* CONNECTED_APPLICATION;
class CONNECTED_APPLICATION_CLASS
{
  private:

    // client stub
    CLOCKTEST_CLIENT_STUB clientStub;

  public:

    CONNECTED_APPLICATION_CLASS(VIRTUAL_PLATFORM vp);
    ~CONNECTED_APPLICATION_CLASS();

    // main
    void Init();
    void Main();
};

#endif
