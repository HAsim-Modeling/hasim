#ifndef __HYBRID_APPLICATION__
#define __HYBRID_APPLICATION__

#include "asim/provides/virtual_platform.h"

typedef class HYBRID_APPLICATION_CLASS* HYBRID_APPLICATION;
class HYBRID_APPLICATION_CLASS
{
  public:
    HYBRID_APPLICATION_CLASS(VIRTUAL_PLATFORM vp);
    ~HYBRID_APPLICATION_CLASS();

    // init
    void Init();
    // main
    void Main();
};

#endif
