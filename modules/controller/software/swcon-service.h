#ifndef __SOFTWARE_CONTROLLER_RRR_SERVICE__
#define __SOFTWARE_CONTROLLER_RRR_SERVICE__

#include "software-rrr-server.h"
#include "software-controller.h"

typedef class SWCON_SERVICE_CLASS* SWCON_SERVICE;
class SWCON_SERVICE_CLASS:  public RRR_SERVICE_CLASS,
                            public HASIM_SW_MODULE_CLASS
{
    private:
        int                     hwState;
        SOFTWARE_CONTROLLER     controller;
        bool                    connected;

    public:
        SWCON_SERVICE_CLASS();
        ~SWCON_SERVICE_CLASS();

        // generic RRR methods
        void    Init(HASIM_SW_MODULE, int);
        void    Uninit();
        bool    Request(UINT32, UINT32, UINT32, UINT32 *);
        void    Poll();

        // methods specific to controller
        void    Connect(SOFTWARE_CONTROLLER);
        void    StartHardware();
        void    StopHardware();
};

#endif
