#ifndef __CONTROLLER__
#define __CONTROLLER__

#include <stdio.h>
#include <pthread.h>

#include "platforms-module.h"
#include "asim/provides/starter.h"
#include "asim/provides/low_level_platform_interface.h"
#include "asim/provides/software_system.h"

// FIXME FIXME FIXME: we need a SYSTEM base class
/*typedef class SYSTEM_CLASS* SYSTEM;
class SYSTEM_CLASS
{
  private:
    pthread_mutex_t lock;
    pthread_cond_t  cond;

  public:
    SYSTEM_CLASS()
    {
        pthread_mutex_init(&lock, NULL);
        pthread_cond_init(&cond, NULL);
    }

    virtual void Main()
    {
        // go off to sleep by waiting on a cond var that
        // will never be signaled
        pthread_mutex_lock(&lock);
        pthread_cond_wait(&cond, &lock);
        pthread_mutex_unlock(&lock);
    }
    };*/

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
