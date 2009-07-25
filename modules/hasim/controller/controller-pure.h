#ifndef __CONTROLLER__
#define __CONTROLLER__

#include <stdio.h>
#include <pthread.h>

#include "platforms-module.h"
#include "asim/provides/starter.h"
#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/model.h"


// controller
typedef class CONTROLLER_CLASS* CONTROLLER;
class CONTROLLER_CLASS: public PLATFORMS_MODULE_CLASS
{
  private:
    // links to LLPI and SYSTEM
    LLPI    llpi;
    SYSTEM  system;
    
  public:
    CONTROLLER_CLASS(LLPI, SYSTEM);
    ~CONTROLLER_CLASS();
    
    void Main();
    void Uninit();
    void Cleanup();
    
    // static methods
    static CONTROLLER GetInstance();
};

// globally visible monitor threadID
extern pthread_t monitorThreadID;

#endif
