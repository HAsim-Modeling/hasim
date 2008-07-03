#ifndef __RRRTEST_SERVER__
#define __RRRTEST_SERVER__

#include <stdio.h>
#include <sys/time.h>

#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/rrr.h"

// this module provides the RRRTest server functionalities

typedef class RRRTEST_SERVER_CLASS* RRRTEST_SERVER;
class RRRTEST_SERVER_CLASS: public RRR_SERVICE_CLASS,
                             public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static RRRTEST_SERVER_CLASS instance;

  public:
    RRRTEST_SERVER_CLASS();
    ~RRRTEST_SERVER_CLASS();

    // static methods
    static RRRTEST_SERVER GetInstance() { return &instance; }

    // required RRR service methods
    void Init(PLATFORMS_MODULE);
    UMF_MESSAGE Request(UMF_MESSAGE);
    void Poll();
};

#endif
