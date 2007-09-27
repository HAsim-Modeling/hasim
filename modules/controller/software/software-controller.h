#ifndef __SOFTWARE_CONTROLLER__
#define __SOFTWARE_CONTROLLER__

#include "main.h"
#include "sim-channelio-sw.h"
#include "software-rrr-server.h"

class SWCON_SERVICE_CLASS;

// *********** software controller ***********
typedef class SOFTWARE_CONTROLLER_CLASS* SOFTWARE_CONTROLLER;
class SOFTWARE_CONTROLLER_CLASS: public HASIM_SW_MODULE_CLASS
{
    private:
        CHANNELIO       channelio;
        RRR_SERVER      rrrServer;
        SWCON_SERVICE_CLASS*   myService;

    public:
        SOFTWARE_CONTROLLER_CLASS();
        ~SOFTWARE_CONTROLLER_CLASS();
        void Main();
        void Uninit();
        void SchedulerLoop();
        void PrintMessage(UINT32, UINT32);
        void CallbackExit(int);
};

#endif
