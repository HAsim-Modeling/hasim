#ifndef _CONTROLLER_
#define _CONTROLLER_

#include <stdio.h>

#include "platforms-module.h"
#include "asim/provides/starter.h"
#include "asim/provides/low_level_platform_interface.h"

typedef class CONTROLLER_CLASS* CONTROLLER;
class CONTROLLER_CLASS: public PLATFORMS_MODULE_CLASS
{
    private:
        // link to LLPI
        LLPI    llpi;

        // link to starter
        STARTER starter;

    public:
        CONTROLLER_CLASS(LLPI);
        ~CONTROLLER_CLASS();

        int  Main();
        void Uninit();
        void Cleanup();
        void SchedulerLoop();

        // static methods
        static CONTROLLER GetInstance();

};

#endif
