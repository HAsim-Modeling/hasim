
#include <iostream>
#include <math.h>
#include <fstream>

#include "asim/provides/bluespec_system.h"

using namespace std;

// constructor
BLUESPEC_SYSTEM_CLASS::BLUESPEC_SYSTEM_CLASS(
    LLPI llpi) :
        PLATFORMS_MODULE_CLASS(NULL)
{
}

// destructor
BLUESPEC_SYSTEM_CLASS::~BLUESPEC_SYSTEM_CLASS()
{
}

// main
void
BLUESPEC_SYSTEM_CLASS::Main()
{
    SYSTEM_CLASS::Main();
}

