#ifndef __MAIN_H__
#define __MAIN_H__

#include <cstdlib>

using namespace std;

// ============= global args ==============
typedef class HASIM_GLOBAL_ARGS_CLASS* HASIM_GLOBAL_ARGS;
class HASIM_GLOBAL_ARGS_CLASS
{
  public:
    const char *Benchmark() const { return benchmark; };
    const char *ModelDir() const { return modelDir; };
    bool ShowFrontPanel() const { return showFrontPanel; };

    void Usage();

    HASIM_GLOBAL_ARGS_CLASS(int argc, char *argv[]);
    ~HASIM_GLOBAL_ARGS_CLASS() {};

  private:
    char* benchmark;            // Benchmark image (user-mode) 
    char* modelDir;             // Model (pm) directory
    bool showFrontPanel;
};

extern HASIM_GLOBAL_ARGS globalArgs;

#endif
