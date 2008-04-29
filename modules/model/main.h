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

    int BluesimArgc() const { return bluesimArgc; }
    char **BluesimArgv() const { return bluesimArgv; }

    void Usage();

    GLOBAL_ARGS_CLASS(int argc, char *argv[]);
    ~GLOBAL_ARGS_CLASS();

  private:
    char* modelDir;             // Model (pm) directory
    bool showFrontPanel;

    // Functional partition arguments
    int funcpArgc;
    char **funcpArgv;

    // Bluesim arguments
    int bluesimArgc;
    char **bluesimArgv;

    vector<string> ParseStringToArgs(const string& line);
    void ParseTraceCmd(const char *command);

    typedef char **ArgVector;
    void InitArgcArgvPair(
        const string& line,
        const char *orig_argv0,
        int& argc,
        char**& argv);
};

extern GLOBAL_ARGS globalArgs;

#endif
