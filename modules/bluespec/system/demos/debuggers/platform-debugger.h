#ifndef __PLATFORM_DEBUGGER__
#define __PLATFORM_DEBUGGER__

#include "asim/provides/command_switches.h"
#include "asim/provides/virtual_platform.h"
#include "asim/rrr/client_stub_PLATFORM_DEBUGGER.h"

typedef class HYBRID_APPLICATION_CLASS* HYBRID_APPLICATION;
class HYBRID_APPLICATION_CLASS
{
  private:

    // client stub
    PLATFORM_DEBUGGER_CLIENT_STUB clientStub;

  public:

    HYBRID_APPLICATION_CLASS(VIRTUAL_PLATFORM vp);
    ~HYBRID_APPLICATION_CLASS();

    // main
    void Init();
    void Main();
};

#endif
