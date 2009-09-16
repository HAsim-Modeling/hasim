#ifndef __HASIM_TIMEP_H__
#define __HASIM_TIMEP_H__

#include "asim/provides/hasim_chip.h"


typedef class HASIM_TIMEP_CLASS* HASIM_TIMEP;

class HASIM_TIMEP_CLASS
{
    private:
        HASIM_CHIP_CLASS* chip;
    public:
    
        HASIM_TIMEP_CLASS() : chip(new HASIM_CHIP_CLASS()) {}
        ~HASIM_TIMEP_CLASS() { delete chip; }

        void Init() { chip->Init(); }
        void MapContexts(int num_ctxts) { chip->MapContexts(num_ctxts); }
        int SimulateModel() 
        {
            // For now the number of hardware threads is simply the number of cores.
            // TODO: check if the given number is over some max.
            // TODO: This number should probably be queried from the core itself.
            //       something like core->GetRequiredNumHardwareThreads();

            // COMMANDS_SERVER commandRelay = COMMANDS_SERVER_CLASS::GetInstance();
            // commandRelay->SetNumHardwareThreads(coresSwitch.NumCores());
            COMMANDS_SERVER_CLASS::GetInstance()->Run();
            return 0;
        }
};

#endif
