#ifndef __RRRTEST_SYSTEM__
#define __RRRTEST_SYSTEM__

#include "platforms-module.h"
#include "asim/provides/hasim_controller.h"
#include "asim/rrr/client_stub_RRRTEST.h"

// RRRTest system

typedef class BLUESPEC_SYSTEM_CLASS* BLUESPEC_SYSTEM;
class BLUESPEC_SYSTEM_CLASS: public SYSTEM_CLASS,
                             public PLATFORMS_MODULE_CLASS
{
  private:

    // client stub
    RRRTEST_CLIENT_STUB clientStub;

  public:

    BLUESPEC_SYSTEM_CLASS(LLPI llpi);
    ~BLUESPEC_SYSTEM_CLASS();

    // main
    void Main();
};

#endif
