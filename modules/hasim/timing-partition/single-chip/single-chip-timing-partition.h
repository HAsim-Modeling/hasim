#ifndef __HASIM_TIMEP_H__
#define __HASIM_TIMEP_H__


/* For now nothing under this level has an explicit software component. */
/* Eventually it may make sense to instantiate an explicit Chip and     */
/* submodules. For now this just instantiates the contexts switch.      */

typedef class HASIM_TIMEP_CLASS* HASIM_TIMEP;

class HASIM_TIMEP_CLASS
{
    public:
    
        HASIM_TIMEP_CLASS()  {}
        ~HASIM_TIMEP_CLASS() {}

        void Init() {}
        int SimulateModel() 
        {
            return 0;
        }
};

#endif
