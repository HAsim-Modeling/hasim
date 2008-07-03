#ifndef __RRRTEST_SYSTEM__
#define __RRRTEST_SYSTEM__

#include "platforms-module.h"
#include "rrrtest-client.h"
#include "asim/provides/hasim_controller.h"

// RRRTest system

typedef class BLUESPEC_SYSTEM_CLASS* BLUESPEC_SYSTEM;
class BLUESPEC_SYSTEM_CLASS: public SYSTEM_CLASS,
                             public PLATFORMS_MODULE_CLASS
{
  private:
    // instantiate RRR client
    RRRTEST_CLIENT_CLASS client;

  public:
    BLUESPEC_SYSTEM_CLASS();
    ~BLUESPEC_SYSTEM_CLASS();

    // main
    void Main();
};

#endif
