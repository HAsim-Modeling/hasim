#ifndef __HASIM_MODULE_H__
#define __HASIM_MODULE_H__

#include <string>

using namespace std;

// =========== HAsim software module =========
typedef class HASIM_MODULE_CLASS* HASIM_MODULE;
class HASIM_MODULE_CLASS
{
    protected:
        HASIM_MODULE parent;   // parent module
        HASIM_MODULE next;     // sibling child module
        HASIM_MODULE children; // list of children
        string       name;     // descriptive name

    public:
        // constructor - destructor
        HASIM_MODULE_CLASS();
        HASIM_MODULE_CLASS(HASIM_MODULE);
        HASIM_MODULE_CLASS(HASIM_MODULE, string);
        ~HASIM_MODULE_CLASS();

        // common methods
        void AddChild(HASIM_MODULE);

        // common virtual methods
        virtual void Uninit();
        virtual void CallbackExit(int);
};

#endif
