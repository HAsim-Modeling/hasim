#ifndef _STARTER_
#define _STARTER_

#include "asim/provides/low_level_platform_interface.h"

// this module provides both client and service functionalities

typedef class STARTER_CLASS* STARTER;
class STARTER_CLASS: public RRR_SERVER_CLASS,
                     public PLATFORMS_MODULE_CLASS
{
  private:
    // self-instantiation
    static STARTER_CLASS instance;

  public:
    STARTER_CLASS();
    ~STARTER_CLASS();

    // static methods
    static STARTER GetInstance() { return &instance; }

    // required RRR service methods
    void Init(PLATFORMS_MODULE);
    UMF_MESSAGE Request(UMF_MESSAGE);

    // client methods
    void Run();
    void Pause();
    void Sync();
    void DumpStats();
};

#endif
