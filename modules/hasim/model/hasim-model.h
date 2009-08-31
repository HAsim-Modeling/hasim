#ifndef __HASIM_MODEL_H__
#define __HASIM_MODEL_H__

typedef class CONNECTED_APPLICATION_CLASS* CONNECTED_APPLICATION;

#include "asim/provides/virtual_platform.h"
#include "asim/provides/hasim_model_services.h"
#include "asim/provides/hasim_funcp.h"
#include "asim/provides/hasim_timep.h"

class CONNECTED_APPLICATION_CLASS
{

    private:
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
