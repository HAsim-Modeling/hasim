#ifndef _CONTROLLER_
#define _CONTROLLER_

#include <stdio.h>

#include "hasim-module.h"
#include "asim/provides/channelio.h"
#include "asim/provides/rrr.h"
#include "asim/provides/starter.h"
#include "asim/provides/hasim_events_controller.h"
#include "asim/provides/streams.h"
#include "asim/provides/low_level_platform_interface.h"

typedef class CONTROLLER_CLASS* CONTROLLER;
class CONTROLLER_CLASS: public HASIM_MODULE_CLASS,
                        public STREAMS_CALLBACK_MODULE_CLASS
{
    private:
        // link to LLPI
        LLPI    llpi;

        // link to starter
        STARTER starter;

        // events controller
        EVENTS_CONTROLLER eventsController;

        // event and stat files
        FILE *eventfile;
        FILE *statfile;

    public:
        CONTROLLER_CLASS(LLPI);
        ~CONTROLLER_CLASS();

        int  Main();
        void Uninit();
        void Cleanup();
        void SchedulerLoop();

        // streams callback
        void StreamsCallback(UINT32, UINT32, UINT32);
};

#endif
