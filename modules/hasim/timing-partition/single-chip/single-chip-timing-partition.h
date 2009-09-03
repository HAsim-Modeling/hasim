#ifndef __HASIM_TIMEP_H__
#define __HASIM_TIMEP_H__


/* For now nothing under this level has an explicit software component. */
/* Eventually it may make sense to instantiate an explicit Chip and     */
/* submodules. For now this just instantiates the contexts switch.      */

class CORES_SWITCH_CLASS : public COMMAND_SWITCH_INT_CLASS
{
    private:
        UINT32 numCores;
    public:
        CORES_SWITCH_CLASS() : numCores(1), COMMAND_SWITCH_INT_CLASS("num-cores") {}
        ~CORES_SWITCH_CLASS() {}
        UINT32 NumCores() { return numCores; }
        
        void ProcessSwitchInt(int arg) { numCores = arg; }
        bool ShowSwitch(char* buff)
        {
            strcpy(buff, "[--num-cores=<n>]        Number of cores to simulate.");
            return true;
        }
};

typedef class HASIM_TIMEP_CLASS* HASIM_TIMEP;

class HASIM_TIMEP_CLASS
{
    private:
        CORES_SWITCH_CLASS coresSwitch;
    public:
    
        HASIM_TIMEP_CLASS() : coresSwitch() {}
        ~HASIM_TIMEP_CLASS() {}

        void Init() {}
        int SimulateModel() 
        {
            // For now the number of hardware threads is simply the number of cores.
            // TODO: check if the given number is over some max.
            // TODO: This number should probably be queried from the core itself.
            //       something like core->GetRequiredNumHardwareThreads();

            COMMANDS_SERVER commandRelay = COMMANDS_SERVER_CLASS::GetInstance();
            commandRelay->SetNumHardwareThreads(coresSwitch.NumCores());
            commandRelay->Run();
            return 0;
        }
};

#endif
