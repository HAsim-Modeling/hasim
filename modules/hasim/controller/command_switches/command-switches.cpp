
#include <string.h>

#include "command-switches.h"

COMMAND_SWITCH COMMAND_SWITCH_CLASS::switchProcessors = NULL;

COMMAND_SWITCH_CLASS::COMMAND_SWITCH_CLASS(char* switch_name, int arg_type) : 
    switchName(switch_name), 
    argType(arg_type), 
    nextProcessor(COMMAND_SWITCH_CLASS::switchProcessors) 
{ 
    COMMAND_SWITCH_CLASS::switchProcessors = this; 
}

void
COMMAND_SWITCH_LIST_CLASS::ProcessSwitch(char* arg)
{
    // Convert arguments to an argv array
    vector<string> av = ParseStringToArgs(arg);
    int argc = av.size();
    char **argv = new char *[argc + 1];

    for (int i = 0; i < av.size(); i++)
    {
        argv[i] = new char[av[i].length() + 1];
        strcpy(argv[i], av[i].c_str());
    }
    argv[argc] = NULL;
    
    ProcessSwitchList(argc, argv);
}

vector<string>
COMMAND_SWITCH_LIST_CLASS::ParseStringToArgs(const string& line)
{
    vector<string> result;

    string item;
    stringstream ss(line);

    while(ss >> item)
    {
        if (item[0]=='"')
        {
            // Drop the leading quote
            item = item.substr(1);

            int lastItemPosition = item.length() - 1;
            if (item[lastItemPosition] != '"')
            {
                // Read the rest of the double-quoted item
                string restOfItem;
                getline(ss, restOfItem, '"');
                item += restOfItem;
            }
            else
            {
                // A single quoted word.  Drop trailing quote
                item = item.substr(0, lastItemPosition);
            }
        }

        result.push_back(item);
    }

    return result;
}
