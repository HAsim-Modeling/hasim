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
typedef class HASIM_SW_MODULE_CLASS* HASIM_SW_MODULE;
class HASIM_SW_MODULE_CLASS
{
    protected:
        HASIM_SW_MODULE parent;

    public:
        virtual void CallbackExit(int exitcode) { parent->CallbackExit(exitcode); }
};

#endif
