#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>
#include <getopt.h>

#include "asim/syntax.h"
#include "asim/mesg.h"
#include "asim/trace.h"

#include "main.h"
#include "asim/provides/hasim_controller.h"
#include "asim/dict/init.h"
#include "asim/provides/virtual_platform.h"
#include "asim/provides/low_level_platform_interface.h"

// =======================================
//                 MAIN
// =======================================

// globally visible variables
GLOBAL_ARGS globalArgs;

// main
int main(int argc, char *argv[])
{
    // parse args and place in global array
    globalArgs = new GLOBAL_ARGS_CLASS(argc, argv);

    // instantiate:
    // 1. Virtual platform
    // 2. LLPI
    // 3. Controller
    VIRTUAL_PLATFORM vp   = new VIRTUAL_PLATFORM_CLASS();
    LLPI       llpi       = new LLPI_CLASS();
    CONTROLLER controller = new CONTROLLER_CLASS(llpi);

    // transfer control to controller
    int exitcode = controller->Main();

    // cleanup and exit
    delete controller;
    delete llpi;
    delete vp;

    return exitcode;
}

// process command-line options
GLOBAL_ARGS_CLASS::GLOBAL_ARGS_CLASS(int argc, char *argv[]) :
    modelDir("."),
    showFrontPanel(false)
{
    enum 
    {
        OPT_FUNCP,
        OPT_MODELDIR,
        OPT_SHOWFP,
        OPT_TR,
        OPT_NOSHOWFP
    };

    vector<string> funcp;
    int c;

    funcpArgc = 0;

    while (true)
    {
        int option_index = 0;
        static struct option long_options[] =
        {
            {"funcp", required_argument, NULL, OPT_FUNCP},
            {"modeldir", required_argument, NULL, OPT_MODELDIR},
            {"showfp", no_argument, NULL, OPT_SHOWFP},
            {"tr", optional_argument, NULL, OPT_TR},
            {"noshowfp", no_argument, NULL, OPT_NOSHOWFP},
            {0, 0, 0, 0}
        };

        c = getopt_long_only(argc, argv, "", long_options, &option_index);
        if (c == -1)
        {
            break;
        }

        switch (c)
        {
          case OPT_FUNCP:
            // Convert functional platform arguments to an argv array
            funcp = ParseStringToArgs(optarg);
            funcpArgc = funcp.size() + 1;
            funcpArgv = new char *[funcpArgc + 2];

            // First argument remains program path
            funcpArgv[0] = new char[strlen(argv[0])+1];
            strcpy(funcpArgv[0], argv[0]);

            for (int i = 0; i < funcp.size(); i++)
            {
                funcpArgv[i+1] = new char[funcp[i].length() + 1];
                strcpy(funcpArgv[i+1], funcp[i].c_str());
            }
            funcpArgv[funcpArgc+1] = NULL;
            break;

          case OPT_MODELDIR:
            modelDir = strdup(optarg);
            break;

          case OPT_SHOWFP:
            showFrontPanel = true;
            break;

          case OPT_TR:
            ParseTraceCmd(optarg);
            break;

          case OPT_NOSHOWFP:
            showFrontPanel = false;
            break;

          case '?':
            Usage();

          default:
            Usage();
        }
    }

    if (optind < argc)
    {
        fprintf(stderr, "Unexpected argument: %s\n", argv[optind]);
        Usage();
    }

    if (funcpArgc == 0)
    {
        funcpArgc = 1;
        funcpArgv = new char*[2];
        funcpArgv[0] = new char[strlen(argv[0])+1];
        strcpy(funcpArgv[0], argv[0]);
        funcpArgv[1] = NULL;
    }
}

vector<string>
GLOBAL_ARGS_CLASS::ParseStringToArgs(const string& line)
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
                item = item.substr(1, lastItemPosition-1);
            }
        }

        result.push_back(item);
    }

    return result;
}


void
GLOBAL_ARGS_CLASS::ParseTraceCmd(const char *command) 
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


GLOBAL_ARGS_CLASS::~GLOBAL_ARGS_CLASS()
{
    for (int i = 0; i < funcpArgc; i++)
    {
        delete[] funcpArgv[i];
    }
    delete[] funcpArgv;
}


void
GLOBAL_ARGS_CLASS::Usage()
{
    fprintf(stderr, "\nArguments:\n");
    fprintf(stderr, "   [--[no]showfp]          Show/don't show front panel\n");
    fprintf(stderr, "   [--modeldir=<dir>]      Model directory\n");
    fprintf(stderr, "   [--funcp=\"<args>\"]    Arguments for the functional partition\n");
    fprintf(stderr, "   [--tr=[</regex/[=012]]] Set trace level by regular expression. Can be given\n");
    fprintf(stderr, "                           multiple times.  If not specified, the trace level will\n");
    fprintf(stderr, "                           default to 1 and the regex to .*\n");
    exit(1);
}
