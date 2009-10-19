#ifndef __COMMAND_SWITCHES_H__
#define __COMMAND_SWITCHES_H__

#include <vector>
#include <getopt.h>

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"
#include "asim/param.h"
#include "asim/atoi.h"
#include "asim/syntax.h"

// COMMAND_SWITCH

// superclass for all command switches.

typedef class COMMAND_SWITCH_CLASS* COMMAND_SWITCH;
class COMMAND_SWITCH_CLASS
{
    private:
        COMMAND_SWITCH nextProcessor;
        char *switchName;
        int   argType;

    public:
        
        static COMMAND_SWITCH switchProcessors;

        COMMAND_SWITCH_CLASS(char* switch_name, int arg_type);
        ~COMMAND_SWITCH_CLASS() {}
        
        char* GetSwitchName() { return switchName; }
        int GetArgType() { return argType; }
        COMMAND_SWITCH GetNextProcessor() { return nextProcessor; }
        
        virtual void ProcessSwitch(char *arg) {}
        virtual bool ShowSwitch(char *buff) { return false; }
};

// COMMAND_SWITCH_VOID

// a flag which is either present or not. 
// It takes no arguments from the user.

class COMMAND_SWITCH_VOID_CLASS : public COMMAND_SWITCH_CLASS
{

    public:
       
        COMMAND_SWITCH_VOID_CLASS(char* switch_name) : COMMAND_SWITCH_CLASS(switch_name, no_argument) {}
        ~COMMAND_SWITCH_VOID_CLASS() {}

        void ProcessSwitch(char *arg) { ProcessSwitchVoid(); }
        virtual void ProcessSwitchVoid() {}
};

// COMMAND_SWITCH_INT

// A command switch which must be accompanied by an integer number.

class COMMAND_SWITCH_INT_CLASS : public COMMAND_SWITCH_CLASS
{

    public:
       
        COMMAND_SWITCH_INT_CLASS(char* switch_name) : COMMAND_SWITCH_CLASS(switch_name, required_argument) {}
        ~COMMAND_SWITCH_INT_CLASS() {}
        
        void ProcessSwitch(char *arg) { ProcessSwitchInt(atoi_general_unsigned(arg)); }
        virtual void ProcessSwitchInt(int arg_val) {}
};

// COMMAND_SWITCH_FP

// A command switch which must be accompanied by a floating point value.

class COMMAND_SWITCH_FP_CLASS : public COMMAND_SWITCH_CLASS
{

    public:
       
        COMMAND_SWITCH_FP_CLASS(char* switch_name) : COMMAND_SWITCH_CLASS(switch_name, required_argument) {}
        ~COMMAND_SWITCH_FP_CLASS() {}
        
        void ProcessSwitch(char *arg) { ProcessSwitchFp(atof(arg)); }
        virtual void ProcessSwitchFp(double arg_val) {}
};

// COMMAND_SWITCH_STRING

// A command switch which must be accompanied by a single string.

class COMMAND_SWITCH_STRING_CLASS : public COMMAND_SWITCH_CLASS
{

    public:
       
        COMMAND_SWITCH_STRING_CLASS(char* switch_name) : COMMAND_SWITCH_CLASS(switch_name, required_argument) {}
        ~COMMAND_SWITCH_STRING_CLASS() {}
        
        void ProcessSwitch(char *arg) { ProcessSwitchString(arg); }
        virtual void ProcessSwitchString(char *arg_val) {}
};

// COMMAND_SWITCH_OPTIONAL_INT

// A command switch which may or may not be accompanied by a number
// If the flag is present, but no argument given, ProcessSwitchVoid
// is invoked. Otherwise ProcessSwitchInt is invoked.

class COMMAND_SWITCH_OPTIONAL_INT_CLASS : public COMMAND_SWITCH_CLASS
{

    public:
       
        COMMAND_SWITCH_OPTIONAL_INT_CLASS(char* switch_name) : COMMAND_SWITCH_CLASS(switch_name, optional_argument) {}
        ~COMMAND_SWITCH_OPTIONAL_INT_CLASS() {}

        void ProcessSwitch(char *arg) { if (arg == NULL) ProcessSwitchVoid(); else ProcessSwitchInt(atoi_general_unsigned(arg)); }
        
        virtual void ProcessSwitchVoid() {}
        virtual void ProcessSwitchInt(int arg_val) {}
};

// COMMAND_SWITCH_OPTIONAL_STRING

// A command switch which may or may not be accompanied by a string.
// If the flag is present, but no argument given, ProcessSwitchVoid
// is invoked. Otherwise ProcessSwitchString is invoked.

class COMMAND_SWITCH_OPTIONAL_STRING_CLASS : public COMMAND_SWITCH_CLASS
{

    public:
       
        COMMAND_SWITCH_OPTIONAL_STRING_CLASS(char* switch_name) : COMMAND_SWITCH_CLASS(switch_name, optional_argument) {}
        ~COMMAND_SWITCH_OPTIONAL_STRING_CLASS() {}
        
        void ProcessSwitch(char *arg) { if (arg == NULL) ProcessSwitchVoid(); else ProcessSwitchString(arg); }
        
        virtual void ProcessSwitchVoid() {}
        virtual void ProcessSwitchString(char *arg_val) {}
};

// COMMAND_SWITCH_LIST

// A command switch which requires a quoted list of arguments.
// These arguments are parsed into a new argc/argv structure
// and passed into ProcessStringList.

class COMMAND_SWITCH_LIST_CLASS : public COMMAND_SWITCH_CLASS
{
    private:
        // internal list processing function.
        vector<string> ParseStringToArgs(const string& line);

    public:
       
        COMMAND_SWITCH_LIST_CLASS(char* switch_name) : COMMAND_SWITCH_CLASS(switch_name, required_argument) {}
        ~COMMAND_SWITCH_LIST_CLASS() {}
        
        void ProcessSwitch(char *arg);
        
        virtual void ProcessSwitchList(int switch_argc, char **switch_argv) {}
};

#endif
