#ifndef _ISA_EMULATOR_
#define _ISA_EMULATOR_

#include <stdio.h>

#include "../platforms-module.h"
#include "asim/provides/rrr.h"

// this module provides both client and service functionalities

typedef class ISA_EMULATOR_CLASS* ISA_EMULATOR;
class ISA_EMULATOR_CLASS: public RRR_SERVICE_CLASS,
                     public PLATFORMS_MODULE_CLASS
{
    private:
        // self-instantiation
        static ISA_EMULATOR_CLASS instance;

    public:
        ISA_EMULATOR_CLASS();
        ~ISA_EMULATOR_CLASS();

        // static methods
        static ISA_EMULATOR GetInstance() { return &instance; }

        // required RRR service methods
        void Init(PLATFORMS_MODULE);
        bool Request(UINT32, UINT32, UINT32, UINT32, UINT32 *);
        void Poll();

        // client methods
        void updateRegister(UINT32 rname, UINT32 rval);
        void emulationFinished();
};

#endif
