#ifndef _STARTER_
#define _STARTER_

#include "main.h"

typedef class STARTER_CLASS* STARTER;
class STARTER_CLASS: public HASIM_MODULE_CLASS
{
    public:
        void StartHardware() {}
        void StopHardware()  {}
};

#endif
