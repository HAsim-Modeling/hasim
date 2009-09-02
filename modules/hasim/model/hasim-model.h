#ifndef __HASIM_MODEL_H__
#define __HASIM_MODEL_H__

#include "asim/provides/command_switches.h"
#include "asim/provides/virtual_platform.h"
#include "asim/provides/hasim_model_services.h"
#include "asim/provides/hasim_funcp.h"
#include "asim/provides/hasim_timep.h"

class HASIM_TRACE_FLAG_CLASS : public COMMAND_SWITCH_STRING_CLASS
{
    public:
        HASIM_TRACE_FLAG_CLASS();
        ~HASIM_TRACE_FLAG_CLASS() {}
        
        void ProcessSwitchString(char* arg);
        bool ShowSwitch(char* buff);
};

typedef class CONNECTED_APPLICATION_CLASS* CONNECTED_APPLICATION;

class CONNECTED_APPLICATION_CLASS
{

    private:
        HASIM_TRACE_FLAG_CLASS traceFlagParser;
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