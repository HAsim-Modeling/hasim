
#include <string.h>

#include "default-switches.h"


GLOBAL_ARGS globalArgs;

MODEL_DIR_SWITCH_CLASS::MODEL_DIR_SWITCH_CLASS() :
    modelDir("."),
    COMMAND_SWITCH_STRING_CLASS("modeldir")
{
}

void
MODEL_DIR_SWITCH_CLASS::ProcessSwitchString(char *arg)
{
    modelDir = strdup(arg);
}

bool
MODEL_DIR_SWITCH_CLASS::ShowSwitch(char *buff)
{
    strcpy(buff, "[--modeldir=<dir>]      Model directory");
    return true;
}

WORKLOAD_SWITCH_CLASS::WORKLOAD_SWITCH_CLASS() :
    workload(APM_NAME),
    COMMAND_SWITCH_STRING_CLASS("workload")
{
}

void
WORKLOAD_SWITCH_CLASS::ProcessSwitchString(char *arg)
{
    workload = strdup(arg);
}

bool
WORKLOAD_SWITCH_CLASS::ShowSwitch(char *buff)
{
    strcpy(buff, "[--workload=\"<args>\"]   Workload name (affects .stats file name)");
    return true;
}

FUNCP_SWITCH_CLASS::FUNCP_SWITCH_CLASS() :
    funcpArgc(0),
    funcpArgv(new char* [1]),
    COMMAND_SWITCH_LIST_CLASS("funcp")
{
    funcpArgv[0] = NULL;
}

void
FUNCP_SWITCH_CLASS::ProcessSwitchList(int switch_argc, char **switch_argv)
{
    funcpArgc = switch_argc;
    delete[] funcpArgv;
    funcpArgv = switch_argv;
}

bool
FUNCP_SWITCH_CLASS::ShowSwitch(char *buff)
{
    strcpy(buff, "[--funcp=\"<args>\"]      Arguments for the functional platform");
    return true;
}

DYN_PARAM_SWITCH_CLASS::DYN_PARAM_SWITCH_CLASS() :
    COMMAND_SWITCH_STRING_CLASS("param")
{
}

void
DYN_PARAM_SWITCH_CLASS::ProcessSwitchString(char *arg)
{
    char *name = strdup(arg);
    char *eq = index(name, '=');
    if ( ! eq )
    {
        ASIMERROR("Invalid parameter specification in '"
                  << "--param " << name << "'" << endl
                  << "    Correct syntax: -param <name>=<value>" << endl);
    }
    else
    {
        char *value = eq + 1;
        *eq = '\0';
        if ( ! SetParam(name, value))
        {
            *eq = '=';
            ASIMERROR("Don't know about dynamic parameter "
                      << name << endl);
        }
        *eq = '=';
    }
    free(name);
}

bool
DYN_PARAM_SWITCH_CLASS::ShowSwitch(char *buff)
{
    strcpy(buff, "[--param NAME=VALUE]    Set a dynamic parameter");
    return true;
}


LISTPARAM_SWITCH_CLASS::LISTPARAM_SWITCH_CLASS() :
    COMMAND_SWITCH_VOID_CLASS("listparam")
{
}

void
LISTPARAM_SWITCH_CLASS::ProcessSwitchVoid()
{
    ListParams();
    exit(0);
}

bool
LISTPARAM_SWITCH_CLASS::ShowSwitch(char *buff)
{
    strcpy(buff, "[--listparam]           List dynamic parameters");
    return true;
}

GLOBAL_ARGS_CLASS::GLOBAL_ARGS_CLASS() :
    modelDirSwitch(),
    workloadSwitch(),
    funcpSwitch(),
    dynParamSwitch(),
    listParamSwitch()
{
}

GLOBAL_ARGS_CLASS::~GLOBAL_ARGS_CLASS()
{
}
