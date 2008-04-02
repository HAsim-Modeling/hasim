#ifndef _STARTER_
#define _STARTER_

#include <stdio.h>

#include "main.h"
#include "asim/provides/rrr.h"

// this module provides both client and service functionalities

typedef class STARTER_CLASS* STARTER;
class STARTER_CLASS: public RRR_SERVICE_CLASS,
                     public HASIM_MODULE_CLASS
{
    private:
        // self-instantiation
        static STARTER_CLASS instance;

    public:
        STARTER_CLASS();
        ~STARTER_CLASS();

        // static methods
        static STARTER GetInstance() { return &instance; }

        // required RRR service methods
        void Init(HASIM_MODULE);
        bool Request(UINT32, UINT32, UINT32, UINT32, UINT32 *);
        void Poll();

        // client methods
        void Run();
        void Pause();
        void Sync();
        void DumpStats();
};

#endif
