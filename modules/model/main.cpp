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
    VIRTUAL_PLATFORM vp   = new VIRTUAL_PLATFORM_CLASS(
        globalArgs->FuncPlatformArgc(),
        globalArgs->FuncPlatformArgv());
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
        OPT_NOSHOWFP
    };

    vector<string> funcp;
    int c;

    funcpArgc = 0;

    while (true)
    {
        int this_option_optind = optind ? optind : 1;
        int option_index = 0;
        static struct option long_options[] =
        {
            {"funcp", required_argument, NULL, OPT_FUNCP},
            {"modeldir", required_argument, NULL, OPT_MODELDIR},
            {"showfp", no_argument, NULL, OPT_SHOWFP},
            {"noshowfp", no_argument, NULL, OPT_NOSHOWFP},
            {0, 0, 0, 0}
        };

        c = getopt_long (argc, argv, "", long_options, &option_index);
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

          case OPT_NOSHOWFP:
            showFrontPanel = false;
            break;

          case '?':
            Usage();

          default:
            Usage();
        }
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
    fprintf(stderr, "   [--[no]showfp]         Show/don't show front panel\n");
    fprintf(stderr, "   [--modeldir <dir>]     Model directory\n");
    fprintf(stderr, "   [--funcp \"<args>\"]   Arguments for the functional partition\n");
    exit(1);
}
