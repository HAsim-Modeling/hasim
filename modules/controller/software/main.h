#ifndef __MAIN_H__
#define __MAIN_H__

#include <cstdlib>

using namespace std;

// ============= global args ==============
struct GlobalArgs
{
    char benchmark[256];
    bool showFrontPanel;
};

extern GlobalArgs globalArgs;

#endif
