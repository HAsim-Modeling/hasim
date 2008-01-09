#ifndef _STARTER_
#define _STARTER_

#include <stdio.h>

#include "main.h"

typedef class STARTER_CLASS* STARTER;
class STARTER_CLASS: public HASIM_MODULE_CLASS
{
    private:

    public:
        STARTER_CLASS();
        ~STARTER_CLASS();
        void StartHardware();
        void StopHardware();
};

#endif
