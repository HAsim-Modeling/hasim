#ifndef __MAIN_H__
#define __MAIN_H__

// ************* global args **************
struct GlobalArgs
{
    char benchmark[256];
    bool showFrontPanel;
};

extern GlobalArgs globalArgs;


// *********** HAsim software module *********
typedef class HASIM_MODULE_CLASS* HASIM_MODULE;
class HASIM_MODULE_CLASS
{
    protected:
        HASIM_MODULE parent;

    public:
        virtual void CallbackExit(int exitcode) { parent->CallbackExit(exitcode); }
};

#endif
