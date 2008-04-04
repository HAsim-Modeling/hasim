#ifndef _EVENTS_CONTROLLER_
#define _EVENTS_CONTROLLER_

#include <stdio.h>

#include "main.h"
#include "asim/provides/rrr.h"

// this module handles events and will eventually interact with DRAL.

typedef class EVENTS_CONTROLLER_CLASS* EVENTS_CONTROLLER;
class EVENTS_CONTROLLER_CLASS: public RRR_SERVICE_CLASS,
                     public HASIM_MODULE_CLASS
{
    private:
        // self-instantiation
        static EVENTS_CONTROLLER_CLASS instance;
        
        // File for output until we use DRAL.
        FILE* eventFile;

    public:
        EVENTS_CONTROLLER_CLASS();
        ~EVENTS_CONTROLLER_CLASS();

        // static methods
        static EVENTS_CONTROLLER GetInstance() { return &instance; }

        // required RRR service methods
        void Init(HASIM_MODULE);
        void Uninit();
        bool Request(UINT32, UINT32, UINT32, UINT32, UINT32 *);
        void Poll();

};

#endif
