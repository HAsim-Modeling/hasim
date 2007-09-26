#ifndef __SOFTWARE_CONTROLLER__
#define __SOFTWARE_CONTROLLER__

#include "main.h"
#include "sim-channelio-sw.h"
#include "software-rrr-server.h"


// *********** software controller ***********
typedef class SOFTWARE_CONTROLLER_CLASS* SOFTWARE_CONTROLLER;
class SOFTWARE_CONTROLLER_CLASS: public HASIM_SW_MODULE_CLASS
{
    private:
        CHANNELIO   channelio;
        RRR_SERVER  rrrServer;

    public:
        SOFTWARE_CONTROLLER_CLASS();
        ~SOFTWARE_CONTROLLER_CLASS();
        void Init();
        void Uninit();
        void Poll();
        void CallbackExit(int);
};

#endif
