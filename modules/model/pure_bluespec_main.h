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


// =========== HAsim software module =========
typedef class HASIM_MODULE_CLASS* HASIM_MODULE;
class HASIM_MODULE_CLASS
{
    protected:
        HASIM_MODULE parent;

    public:
        // constructors
        HASIM_MODULE_CLASS()               { parent = NULL; }
        HASIM_MODULE_CLASS(HASIM_MODULE p) { parent = p; }

        // destructor
        ~HASIM_MODULE_CLASS() { Uninit(); }

        // uninit
        virtual void Uninit() {}

        // callback function
        virtual void CallbackExit(int exitcode)
        {
            if (parent == NULL)
            {
                // chain-uninit, then exit
                Uninit();
                exit(exitcode);
            }
            else
            {
                // transfer to parent
                parent->CallbackExit(exitcode);
            }
        }
};

#endif
