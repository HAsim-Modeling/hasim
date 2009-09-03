
#include "asim/provides/commands_service.h"
#include "asim/provides/connected_application.h"

CONNECTED_APPLICATION_CLASS::CONNECTED_APPLICATION_CLASS(VIRTUAL_PLATFORM vp) :
    traceFlagParser(),
    modelServices(new MODEL_SERVICES_CLASS()),
    functionalPartition(new HASIM_FUNCP_CLASS()),
    timingPartition(new HASIM_TIMEP_CLASS())
{
}


CONNECTED_APPLICATION_CLASS::~CONNECTED_APPLICATION_CLASS()
{
    delete timingPartition;
    delete functionalPartition;
    delete modelServices;
}


void
CONNECTED_APPLICATION_CLASS::Init()
{
    modelServices->Init();
    functionalPartition->Init();
    timingPartition->Init();
}

int
CONNECTED_APPLICATION_CLASS::Main()
{
    return timingPartition->SimulateModel();
}
