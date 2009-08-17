#ifndef __COMMAND_SWITCH_PROCESSOR_H__
#define __COMMAND_SWITCH_PROCESSOR_H__

#include "command-switches.h"
#include "default-switches.h"

// Default command-line switches are globally visible.
extern GLOBAL_ARGS globalArgs;

typedef class COMMAND_SWITCH_PROCESSOR_CLASS* COMMAND_SWITCH_PROCESSOR;

class HELP_SWITCH_CLASS : public COMMAND_SWITCH_VOID_CLASS
{
    private:
        COMMAND_SWITCH_PROCESSOR proc;
    public:
        HELP_SWITCH_CLASS(char* helpflag, COMMAND_SWITCH_PROCESSOR p);
        ~HELP_SWITCH_CLASS();
        
        void ProcessSwitchVoid();
};


class COMMAND_SWITCH_PROCESSOR_CLASS
{
  private:
    HELP_SWITCH_CLASS helpSwitch;
    HELP_SWITCH_CLASS helpAppendSwitch;

  public:
    COMMAND_SWITCH_PROCESSOR_CLASS();
    ~COMMAND_SWITCH_PROCESSOR_CLASS();

    void Usage();
    void ShowArgsHelp(bool fromRunScript = false);
    
    void ProcessArgs(int argc, char *argv[]);

};

#endif
