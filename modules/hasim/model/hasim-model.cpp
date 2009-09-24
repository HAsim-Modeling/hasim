
#include "asim/provides/commands_service.h"
#include "asim/provides/connected_application.h"

CONNECTED_APPLICATION_CLASS::CONNECTED_APPLICATION_CLASS(VIRTUAL_PLATFORM vp) :
    contextsSwitch(),
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
    // Setup context mapping. Find out how many contexts the benchmark expects.
    int num_ctxts = contextsSwitch.NumContexts();
    
    // Verify that this number is actually supported by the functional partition.
    VERIFY(num_ctxts <= functionalPartition->GetMaxNumContexts(), "Told to run benchmark with more contexts than statically supported by the functional partition!");
    
    // Tell the timing partition to map those contexts onto its topology however it wants.
    timingPartition->MapContexts(num_ctxts);

    return timingPartition->SimulateModel();
}
