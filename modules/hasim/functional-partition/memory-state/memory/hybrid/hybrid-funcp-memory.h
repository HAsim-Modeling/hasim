#ifndef __FUNCP_MEMORY__
#define __FUNCP_MEMORY__

#include "asim/provides/rrr.h"

// types
#if   (MEMORY_ADDR_SIZE == 32)
typedef UINT32 MEM_ADDRESS;
#elif (MEMORY_ADDR_SIZE == 64)
typedef UINT64 MEM_ADDRESS;
#else
#error "invalid memory address size"
#endif

#if   (MEMORY_VALUE_SIZE == 32)
typedef UINT32 MEM_VALUE;
#elif (MEMORY_VALUE_SIZE == 64)
typedef UINT64 MEM_VALUE;
#else
#error "invalid memory value size"
#endif


#define MEM_SIZE    262144 // number of words
#define CMD_LOAD    0
#define CMD_STORE   1

class FUNCP_MEMORY_CLASS: public RRR_SERVICE_CLASS,
                          public PLATFORMS_MODULE_CLASS
{
    private:
        // self-instantiation
        static FUNCP_MEMORY_CLASS instance;

        MEM_VALUE* M;
        bool       vmhLoaded;

    public:
        FUNCP_MEMORY_CLASS();
        ~FUNCP_MEMORY_CLASS();

        void    Init(PLATFORMS_MODULE);
        void    Uninit();
        void    Cleanup();
        UMF_MESSAGE Request(UMF_MESSAGE);
        void    Poll();
};

#endif
