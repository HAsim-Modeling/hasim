#ifndef __SOFTWARE_CONTROLLER__
#define __SOFTWARE_CONTROLLER__

#include <stdio.h>

#include "main.h"
#include "sim-channelio-sw.h"
#include "software-server.h"

class SWCON_SERVICE_CLASS;

// *********** software controller ***********
typedef class SOFTWARE_CONTROLLER_CLASS* SOFTWARE_CONTROLLER;
class SOFTWARE_CONTROLLER_CLASS: public HASIM_SW_MODULE_CLASS
{
    private:
        CHANNELIO               channelio;
        RRR_SERVER              rrrServer;
        SWCON_SERVICE_CLASS*    myService;

        FILE*   eventfile;
        FILE*   statfile;

    public:
        SOFTWARE_CONTROLLER_CLASS();
        ~SOFTWARE_CONTROLLER_CLASS();
        void Main();
        void Uninit();
        void SchedulerLoop();
        void PrintMessage(UINT32, UINT32);
        void PrintEvent(UINT32, UINT32);
        void PrintStat(UINT32, UINT32);
        void PrintAssert(UINT32, UINT32);
        void CallbackExit(int);
};

#endif
