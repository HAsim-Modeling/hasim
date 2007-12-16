#ifndef __SOFTWARE_CONTROLLER_RRR_SERVICE__
#define __SOFTWARE_CONTROLLER_RRR_SERVICE__

#include "basic-rrr-server.h"
#include "software-controller.h"

typedef class SWCON_SERVICE_CLASS* SWCON_SERVICE;
class SWCON_SERVICE_CLASS:  public RRR_SERVICE_CLASS,
                            public HASIM_MODULE_CLASS
{
    private:
        // self-instantiation
        static SWCON_SERVICE_CLASS  instance;

        int                     hwState;
        SOFTWARE_CONTROLLER     controller;
        bool                    connected;

    public:
        SWCON_SERVICE_CLASS();
        ~SWCON_SERVICE_CLASS();

        // generic RRR methods
        void    Init(HASIM_MODULE);
        void    Uninit();
        bool    Request(UINT32, UINT32, UINT32, UINT32 *);
        void    Poll();

        // methods specific to controller
        void    Connect(SOFTWARE_CONTROLLER);
        void    StartHardware();
        void    StopHardware();

        // static methods
        static SWCON_SERVICE   GetInstance() { return &instance; }
};

#endif
