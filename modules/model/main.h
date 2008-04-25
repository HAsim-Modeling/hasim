#ifndef __MAIN_H__
#define __MAIN_H__

#include <cstdlib>
#include <vector>
#include <string>
#include <sstream>

using namespace std;

// ============= global args ==============
typedef class GLOBAL_ARGS_CLASS* GLOBAL_ARGS;
class GLOBAL_ARGS_CLASS
{
  public:
    const char *ModelDir() const { return modelDir; };
    bool ShowFrontPanel() const { return showFrontPanel; };

    int FuncPlatformArgc() const { return funcpArgc; }
    char **FuncPlatformArgv() const { return funcpArgv; }

    void Usage();

    GLOBAL_ARGS_CLASS(int argc, char *argv[]);
    ~GLOBAL_ARGS_CLASS();

  private:
    char* modelDir;             // Model (pm) directory
    bool showFrontPanel;

    int funcpArgc;
    char **funcpArgv;

    vector<string> ParseStringToArgs(const string& line);
    void ParseTraceCmd(const char *command);
};

extern GLOBAL_ARGS globalArgs;

#endif
