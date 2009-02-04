#ifndef __BLUESPEC_SYSTEM__
#define __BLUESPEC_SYSTEM__

#include "platforms-module.h"
#include "asim/provides/hasim_controller.h"

typedef class BLUESPEC_SYSTEM_CLASS* BLUESPEC_SYSTEM;
class BLUESPEC_SYSTEM_CLASS: public SYSTEM_CLASS,
                             public PLATFORMS_MODULE_CLASS
{
  public:
    BLUESPEC_SYSTEM_CLASS();
    ~BLUESPEC_SYSTEM_CLASS();

    // main
    void Main();
};

#endif
