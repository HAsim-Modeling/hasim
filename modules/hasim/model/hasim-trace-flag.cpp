
#include "asim/provides/connected_application.h"

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"
#include "asim/param.h"
#include "asim/atoi.h"


HASIM_TRACE_FLAG_CLASS::HASIM_TRACE_FLAG_CLASS() :
    COMMAND_SWITCH_STRING_CLASS("tr")
{
}

void
HASIM_TRACE_FLAG_CLASS::ProcessSwitchString(char *command) 
{
    string regex;
    int level;

    if (command == NULL)
    {
        // no regex given, match every name
        regex = ".*";
        level = 1;
    }
    else
    {
        regex = command;
        int pos = regex.size() - 1;
        // the last char should be '0', '1', or '2'
        if (regex[pos] != '0' && regex[pos] != '1' && regex[pos] != '2') 
        {
            // If a level was not given, defaul to 1.
            level = 1;
        } 
        else 
        {
            level = regex[pos] - '0';
            pos--;

            if ((pos >= 0) && regex[pos] != '=')
            {
                cerr << "\nExpected -tr=[/regex/[=012]]" << endl;
                exit(1);
            }
            pos--;
        }

        // remove the '/' at front and back
        if ((pos >= 0) && (regex[pos] != '/' || regex[0] != '/'))
        {
            cerr << "\nExpected -tr=[/regex/[=012]]" << endl;
            exit(1);
        }

        if (pos <= 1)
        {
            // Empty regular expression.  Use default.
            regex = ".*";
        }
        else
        {
            // Drop everything but the text inside the slashes.
            regex.erase(pos);
            regex.erase(0,1);
        }
        
    }

    TRACEABLE_CLASS::EnableTraceByRegex(regex, level);
}

bool
HASIM_TRACE_FLAG_CLASS::ShowSwitch(char *buff) 
{
    strcpy(buff, "[--tr=[</regex/[=012]]] Set trace level by regular expression. Can be given\n \
                          multiple times.  If not specified, the trace level will\n \
                          default to 1 and the regex to .*\n");
    return true;
}
