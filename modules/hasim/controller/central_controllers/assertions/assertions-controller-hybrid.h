#ifndef _ASSERTIONS_CONTROLLER_
#define _ASSERTIONS_CONTROLLER_

#include <stdio.h>

#include "platforms-module.h"
#include "asim/provides/rrr.h"

// this module handles reporting assertion failures.

typedef class ASSERTIONS_CONTROLLER_CLASS* ASSERTIONS_CONTROLLER;
class ASSERTIONS_CONTROLLER_CLASS: public RRR_SERVICE_CLASS,
                     public PLATFORMS_MODULE_CLASS
{
    private:
        // self-instantiation
        static ASSERTIONS_CONTROLLER_CLASS instance;
        
        // File for output until we use DRAL.
        FILE* assertionsFile;

    public:
        ASSERTIONS_CONTROLLER_CLASS();
        ~ASSERTIONS_CONTROLLER_CLASS();

        // static methods
        static ASSERTIONS_CONTROLLER GetInstance() { return &instance; }

        // required RRR service methods
        void Init(PLATFORMS_MODULE);
        void Uninit();
        bool Request(UINT32, UINT32, UINT32, UINT32, UINT32 *);
        void Poll();

};

#endif
