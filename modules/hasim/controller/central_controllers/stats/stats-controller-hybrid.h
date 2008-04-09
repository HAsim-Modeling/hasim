#ifndef _STATS_CONTROLLER_
#define _STATS_CONTROLLER_

#include <stdio.h>

#include "platforms-module.h"
#include "asim/provides/rrr.h"

// this module handles gathering statistics. 
// Eventually this will interact with standard tools.

typedef class STATS_CONTROLLER_CLASS* STATS_CONTROLLER;
class STATS_CONTROLLER_CLASS: public RRR_SERVICE_CLASS,
                     public PLATFORMS_MODULE_CLASS
{
    private:
        // self-instantiation
        static STATS_CONTROLLER_CLASS instance;
        
        // File for output until we use DRAL.
        FILE* statsFile;

    public:
        STATS_CONTROLLER_CLASS();
        ~STATS_CONTROLLER_CLASS();

        // static methods
        static STATS_CONTROLLER GetInstance() { return &instance; }

        // required RRR service methods
        void Init(PLATFORMS_MODULE);
        void Uninit();
        bool Request(UINT32, UINT32, UINT32, UINT32, UINT32 *);
        void Poll();

};

#endif
