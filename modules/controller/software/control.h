#ifndef _CONTROLLER_
#define _CONTROLLER_

#include <stdio.h>

#include "main.h"
#include "asim/provides/channelio.h"
#include "asim/provides/rrr.h"
#include "asim/provides/starter.h"

typedef class CONTROLLER_CLASS* CONTROLLER;
class CONTROLLER_CLASS: public HASIM_MODULE_CLASS
{
    private:
        CHANNELIO_CLASS  channelio;
        RRR_SERVER_CLASS rrrServer;
        RRR_CLIENT_CLASS rrrClient;

        // link to starter
        STARTER starter;

    public:
        CONTROLLER_CLASS();
        ~CONTROLLER_CLASS();
        int  Main();
        void Uninit();
        void SchedulerLoop();
        void CallbackExit(int);
};

#endif
