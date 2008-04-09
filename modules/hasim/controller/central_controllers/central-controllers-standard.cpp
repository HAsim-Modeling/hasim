#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "central-controllers-standard.h"

using namespace std;

// ===== service instantiation =====
CENTRAL_CONTROLLERS_CLASS CENTRAL_CONTROLLERS_CLASS::instance;

// constructor
CENTRAL_CONTROLLERS_CLASS::CENTRAL_CONTROLLERS_CLASS() :
        PLATFORMS_MODULE_CLASS(NULL)
{

    // setup events controller
    eventsController = EVENTS_CONTROLLER_CLASS::GetInstance();
  
    // setup stats controller
    statsController = STATS_CONTROLLER_CLASS::GetInstance();
    
    // setup asserts controller
    assertsController = ASSERTIONS_CONTROLLER_CLASS::GetInstance();

}

// destructor
CENTRAL_CONTROLLERS_CLASS::~CENTRAL_CONTROLLERS_CLASS()
{
}
