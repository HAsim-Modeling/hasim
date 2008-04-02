#ifndef _STARTER_
#define _STARTER_

#include "hasim-module.h"

typedef class STARTER_CLASS* STARTER;
class STARTER_CLASS: public HASIM_MODULE_CLASS
{
    private:
        // self-instantiation is required for this module
        static STARTER_CLASS instance;

    public:
        STARTER_CLASS();
        ~STARTER_CLASS();

        static STARTER GetInstance() { return &instance; }
        void Run()       {}
        void Pause()     {}
        void Sync()      {}
        void DumpStats() {}
};

#endif
