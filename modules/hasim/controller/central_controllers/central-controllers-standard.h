#ifndef _CENTRAL_CONTROLLERS_
#define _CENTRAL_CONTROLLERS_

#include <stdio.h>

#include "platforms-module.h"
#include "asim/provides/events_controller.h"
#include "asim/provides/stats_controller.h"
#include "asim/provides/assertions_controller.h"


typedef class CENTRAL_CONTROLLERS_CLASS* CENTRAL_CONTROLLERS;

class CENTRAL_CONTROLLERS_CLASS: public PLATFORMS_MODULE_CLASS
{
    private:

        // self-instantiation
        static CENTRAL_CONTROLLERS_CLASS instance;

        // link to events controller
        EVENTS_CONTROLLER eventsController;
	
	// link to stats controller
	STATS_CONTROLLER statsController;
	
	// link to asserts controller
	ASSERTIONS_CONTROLLER assertsController;

    public:

        CENTRAL_CONTROLLERS_CLASS();
        ~CENTRAL_CONTROLLERS_CLASS();

        // static methods
        static CENTRAL_CONTROLLERS GetInstance() { return &instance; }

};

#endif
