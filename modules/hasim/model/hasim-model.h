#ifndef __HASIM_MODEL_H__
#define __HASIM_MODEL_H__

#include "asim/provides/command_switches.h"
#include "asim/provides/virtual_platform.h"
#include "asim/provides/hasim_model_services.h"
#include "asim/provides/hasim_funcp.h"
#include "asim/provides/hasim_timep.h"

class CONTEXTS_SWITCH_CLASS : public COMMAND_SWITCH_INT_CLASS
{
    private:
        UINT32 numContexts;
    public:
        CONTEXTS_SWITCH_CLASS() : numContexts(1), COMMAND_SWITCH_INT_CLASS("workload-contexts") {}
        ~CONTEXTS_SWITCH_CLASS() {}
        UINT32 NumContexts() { return numContexts; }
        
        void ProcessSwitchInt(int arg) { numContexts = arg; }
        void ShowSwitch(std::ostream& ostr, const string& prefix)
        {
            ostr << prefix << "[--workload-contexts=<n>]        Number of contexts for this benchmark." << endl;
        }
};

typedef class CONNECTED_APPLICATION_CLASS* CONNECTED_APPLICATION;

class CONNECTED_APPLICATION_CLASS
{

    private:
        CONTEXTS_SWITCH_CLASS contextsSwitch;
        MODEL_SERVICES modelServices;
        HASIM_FUNCP functionalPartition;
        HASIM_TIMEP timingPartition;
    public:
    
        CONNECTED_APPLICATION_CLASS(VIRTUAL_PLATFORM vp);
        ~CONNECTED_APPLICATION_CLASS();

        void Init();
        int  Main();
};


#endif
