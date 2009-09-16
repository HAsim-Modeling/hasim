#ifndef __HASIM_FUNCP_H__
#define __HASIM_FUNCP_H__

#include "asim/provides/hasim_common.h"

/* For now nothing under this level has an explicit software component. */
/* Eventually it may explicitly launch M5.     */

typedef class HASIM_FUNCP_CLASS* HASIM_FUNCP;

class HASIM_FUNCP_CLASS
{
    public:
    
        HASIM_FUNCP_CLASS()  {}
        ~HASIM_FUNCP_CLASS() {}

        void Init() {}
        int GetMaxNumContexts() 
        {
          return 1 << CONTEXT_ID_BITS;
        }
};

#endif
