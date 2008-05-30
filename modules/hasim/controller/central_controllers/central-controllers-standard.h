#ifndef _CENTRAL_CONTROLLERS_
#define _CENTRAL_CONTROLLERS_

#include <stdio.h>

#include "platforms-module.h"
#include "asim/provides/events_controller.h"
#include "asim/provides/stats_controller.h"
#include "asim/provides/assertions_controller.h"
#include "asim/provides/params_controller.h"


typedef class CENTRAL_CONTROLLERS_CLASS* CENTRAL_CONTROLLERS;

class CENTRAL_CONTROLLERS_CLASS: public PLATFORMS_MODULE_CLASS
{
    public:

        CENTRAL_CONTROLLERS_CLASS();
        ~CENTRAL_CONTROLLERS_CLASS();

};

#endif
