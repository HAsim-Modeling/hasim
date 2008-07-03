#ifndef __RRRTEST_CLIENT__
#define __RRRTEST_CLIENT__

#include <stdio.h>
#include <sys/time.h>

#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/rrr.h"

// RRRTest client functionalities

typedef class RRRTEST_CLIENT_CLASS* RRRTEST_CLIENT;
class RRRTEST_CLIENT_CLASS: public PLATFORMS_MODULE_CLASS
{
  private:

  public:
    RRRTEST_CLIENT_CLASS(PLATFORMS_MODULE);
    ~RRRTEST_CLIENT_CLASS();

    // client methods
    UINT64 F2HOneWayTest(UINT64);
    UINT64 F2HTwoWayTest(UINT64);
    UINT64 F2HTwoWayPipeTest(UINT64);
};

#endif
