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
#include "asim/param.h"
#include "asim/atoi.h"

#include "asim/provides/command_switches.h"

// process command-line options

GLOBAL_ARGS_CLASS::GLOBAL_ARGS_CLASS(int argc, char *argv[]) :
    modelDir("."),
    workload(APM_NAME),
    stopCycle(0),
    progressMsgInterval(1),
    showFrontPanel(false),
    showLEDsOnStdOut(true)
{
    enum 
    {
        OPT_BLUESIM_ARGS,
        OPT_CYCLES,
        OPT_FUNCP,
        OPT_HELP,
        OPT_HELP_RUN_APPEND,
        OPT_MODELDIR,
        OPT_PC,
        OPT_SHOWFP,
        OPT_NOSHOWFP,
        OPT_TR,
        OPT_WORKLOAD,
        OPT_PARAM,
        OPT_LISTPARAM,
    };

    int c;
    char * name;
    char * eq;
    char * value;

    funcpArgc = 0;

    while (true)
    {
        int option_index = 0;
        static struct option long_options[] =
        {
            {"bluesimargs", required_argument, NULL, OPT_BLUESIM_ARGS},
            {"cycles", required_argument, NULL, OPT_CYCLES},
            {"funcp", required_argument, NULL, OPT_FUNCP},
            {"help", no_argument, NULL, OPT_HELP},
            {"help-run-append", no_argument, NULL, OPT_HELP_RUN_APPEND},
            {"modeldir", required_argument, NULL, OPT_MODELDIR},
            {"pc", required_argument, NULL, OPT_PC},
            {"showfp", optional_argument, NULL, OPT_SHOWFP},
            {"noshowfp", no_argument, NULL, OPT_NOSHOWFP},
            {"tr", optional_argument, NULL, OPT_TR},
            {"workload", required_argument, NULL, OPT_WORKLOAD},
            {"param", required_argument, NULL, OPT_PARAM},
            {"listparam", no_argument, NULL, OPT_LISTPARAM},
            {0, 0, 0, 0}
        };

        c = getopt_long_only(argc, argv, "", long_options, &option_index);
        if (c == -1)
        {
            break;
        }

        switch (c)
        {
          case OPT_BLUESIM_ARGS:
            InitArgcArgvPair(optarg, argv[0], bluesimArgc, bluesimArgv);
            break;

          case OPT_CYCLES:
            stopCycle = atoi_general_unsigned(optarg);
            break;

          case OPT_FUNCP:
            InitArgcArgvPair(optarg, argv[0], funcpArgc, funcpArgv);
            break;

          case OPT_HELP_RUN_APPEND:
            // Run script already printed some help.  Add model specific help.
            ShowArgsHelp(true);
            exit(0);
            break;

          case OPT_MODELDIR:
            modelDir = strdup(optarg);
            break;

          case OPT_PC:
            // Progress (hearbeat) interval
            progressMsgInterval = atoi_general_unsigned(optarg);
            break;

          case OPT_SHOWFP:
            if (optarg == NULL)
            {
                showFrontPanel = true;
                showLEDsOnStdOut = false;
            }
            else if (strcmp(optarg, "stdout") == 0)
            {
                showFrontPanel = false;
                showLEDsOnStdOut = true;
            }
            else if (strncmp(optarg, "no", 2) == 0)
            {
                showFrontPanel = false;
                showLEDsOnStdOut = false;
            }
            else
            {
                showFrontPanel = true;
                showLEDsOnStdOut = false;
            }
            break;

          case OPT_NOSHOWFP:
            showFrontPanel = false;
            showLEDsOnStdOut = false;
            break;

          case OPT_TR:
            ParseTraceCmd(optarg);
            if (! BUILT_WITH_TRACE_FLAGS)
            {
                ASIMWARNING("--tr IGNORED!  Model built without TRACE support." << endl);
            }
            break;

          case OPT_WORKLOAD:
            workload = strdup(optarg);
            break;

          case OPT_PARAM:
            name = strdup(optarg);
            eq = index(name, '=');
            if ( ! eq )
            {
                ASIMERROR("Invalid parameter specification in '"
                          << "--param " << name << "'" << endl
                          << "    Correct syntax: -param <name>=<value>" << endl);
            }
            else
            {
                value = eq + 1;
                *eq = '\0';
                if ( ! SetParam(name, value))
                {
                    *eq = '=';
                    ASIMERROR("Don't know about dynamic parameter "
                              << name << endl
                              << "    ignoring command line portion '"
                              << argv[0] << " " << argv[1]  << "'" << endl);
                }
                *eq = '=';
            }
            free(name);
            break;

          case OPT_LISTPARAM:
            ListParams();
            exit(0);
            break;

          case OPT_HELP:
          case '?':
          default:
            Usage();
        }
    }

    if (optind < argc)
    {
        fprintf(stderr, "Unexpected argument: %s\n", argv[optind]);
        Usage();
    }

    if (bluesimArgc == 0)
    {
        InitArgcArgvPair("", argv[0], bluesimArgc, bluesimArgv);
    }
    if (funcpArgc == 0)
    {
        InitArgcArgvPair("", argv[0], funcpArgc, funcpArgv);
    }
}

void
GLOBAL_ARGS_CLASS::InitArgcArgvPair(
    const string& line,
    const char *orig_argv0,
    int& argc,
    char**& argv)
{
            // Convert functional platform arguments to an argv array
    vector<string> av = ParseStringToArgs(line);
    argc = av.size() + 1;
    argv = new char *[argc + 2];

    // First argument remains program path.
    argv[0] = new char[strlen(orig_argv0) + 1];
    strcpy(argv[0], orig_argv0);

    for (int i = 0; i < av.size(); i++)
    {
        argv[i + 1] = new char[av[i].length() + 1];
        strcpy(argv[i + 1], av[i].c_str());
    }
    argv[argc] = NULL;
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
                item = item.substr(0, lastItemPosition);
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
    ShowArgsHelp();
    exit(1);
}

void
GLOBAL_ARGS_CLASS::ShowArgsHelp(bool fromRunScript)
{
    if (! fromRunScript)
    {
        // Hide arguments set automatically by a run script
        fprintf(stderr, "   [--modeldir=<dir>]      Model directory\n");
        fprintf(stderr, "   [--funcp=\"<args>\"]      Arguments for the functional partition\n");
        fprintf(stderr, "   [--workload=\"<args>\"]   Workload name (affects .stats file name)\n");
    }
    fprintf(stderr, "   [--noshowfp]            Don't show front panel\n");
    fprintf(stderr, "   [--showfp[=gui|stdout|none]]\n");
    fprintf(stderr, "                           Front panel control: GUI, LEDs to stdout or none\n");
    fprintf(stderr, "   [--bluesim=\"<args>\"]    Arguments to Bluesim\n");
    fprintf(stderr, "   [--listparam]           List dynamic parameters\n");
    fprintf(stderr, "   [--param NAME=VALUE]    Set a dynamic parameter\n");
    fprintf(stderr, "   [--cycles=<n>]          Stop simulation after n cycles\n");
    fprintf(stderr, "   [--pc=<interval>]       Progress message (hearbeat) interval.\n");
    fprintf(stderr, "                           Messages are triggered by heartbeats that arrive\n");
    fprintf(stderr, "                           from the hardware side, so messages can be no more\n");
    fprintf(stderr, "                           frequent than the hardware heartbeat interval.\n");
    fprintf(stderr, "                           0 means no messages.  Any value <= the heartbeat\n");
    fprintf(stderr, "                           interval (e.g. 1) triggers a message on every beat.\n");
    fprintf(stderr, "   [--tr=[</regex/[=012]]] Set trace level by regular expression. Can be given\n");
    fprintf(stderr, "                           multiple times.  If not specified, the trace level will\n");
    fprintf(stderr, "                           default to 1 and the regex to .*\n");
    fprintf(stderr, "\n");
}
