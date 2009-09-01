#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>

#include "command-switch-processor.h"

// Handle printing out the help messages.
HELP_SWITCH_CLASS::HELP_SWITCH_CLASS(char* helpflag, COMMAND_SWITCH_PROCESSOR p) : 
    COMMAND_SWITCH_VOID_CLASS(helpflag), 
    proc(p)
{
}

HELP_SWITCH_CLASS::~HELP_SWITCH_CLASS()
{
}

void HELP_SWITCH_CLASS::ProcessSwitchVoid() 
{ 
    proc->ShowArgsHelp(false); 
    exit(0); 
}

// Process command-line options

COMMAND_SWITCH_PROCESSOR_CLASS::COMMAND_SWITCH_PROCESSOR_CLASS() :
    helpSwitch("help", this),
    helpAppendSwitch("help-run-append", this)
{
}

COMMAND_SWITCH_PROCESSOR_CLASS::~COMMAND_SWITCH_PROCESSOR_CLASS()
{
}

void
COMMAND_SWITCH_PROCESSOR_CLASS::ProcessArgs(int argc, char *argv[])
{
    int c;

    COMMAND_SWITCH first_switch = COMMAND_SWITCH_CLASS::switchProcessors;
    COMMAND_SWITCH cur_switch = first_switch;
    
    int num_switches = 0;
    
    // Record the executable name
    globalArgs->SetExecutableName(argv[0]);
    
    // cout << "Parsing switches..." << endl;

    while (cur_switch != NULL)
    {
        //cout << "Found switch " << num_switches << endl;
        //char * buff = new char[128];
        //cur_switch->ShowSwitch(buff);
        //cout << buff << endl;
        //delete[] buff;
        num_switches++;
        cur_switch = cur_switch->GetNextProcessor();
    }
    
    
    if (num_switches == 0)
    {
        cerr << "ERROR: no switches found!" << endl;
        exit(1);
    }
    
    // instantiate an array with room for all the switches,
    // plus room for an all-zero element.

    option* long_options = new option[num_switches + 3];
    
    cur_switch = first_switch;
    int cur_idx = 0;

    while (cur_idx < num_switches)
    {
        struct option new_opt = {cur_switch->GetSwitchName(), cur_switch->GetArgType(), NULL, cur_idx};
        long_options[cur_idx] = new_opt;
        cur_idx++;
        cur_switch = cur_switch->GetNextProcessor();
    }

    cur_idx++;
    struct option end_opt = {0, 0, 0, 0};
    long_options[cur_idx] = end_opt;
    
    while (c != -1)
    {
        int option_index = 0;
        cur_switch = first_switch;
        c = getopt_long_only(argc, argv, "", long_options, &option_index);
        if (c != -1)
        {
            if (c == '?')
            {
                Usage();
            }
            while (c != 0)
            {
                cur_switch = cur_switch->GetNextProcessor();
                c--;
            }
            cur_switch->ProcessSwitch(optarg);
        }
    }
    
    if (optind < argc)
    {
        fprintf(stderr, "Unexpected argument: %s\n", argv[optind]);
        Usage();
    }


    return;
}



void
COMMAND_SWITCH_PROCESSOR_CLASS::Usage()
{
    fprintf(stderr, "\nArguments:\n");
    ShowArgsHelp();
    exit(1);
}

void
COMMAND_SWITCH_PROCESSOR_CLASS::ShowArgsHelp(bool fromRunScript)
{

    
    COMMAND_SWITCH first_switch = COMMAND_SWITCH_CLASS::switchProcessors;
    COMMAND_SWITCH cur_switch = first_switch;
    
    while (cur_switch != NULL)
    {
        char* cur_line = new char[255];
        bool disp_switch = cur_switch->ShowSwitch(cur_line);
        if (disp_switch)
        {
            cerr << "   " << cur_line << endl;
        }
        delete cur_line;
        cur_switch = cur_switch->GetNextProcessor();
    }
    cerr << endl;
}
