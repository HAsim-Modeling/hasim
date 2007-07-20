#ifndef __SOFTWARE_RPC_SERVER__
#define __SOFTWARE_RPC_SERVER__

#define CHANNELIO_PACKET_SIZE   4
#define STDIN                   0
#define STDOUT                  1
#define MAX_SERVICES            1
#define MAX_ARGS                3

typedef unsigned int UINT32;
typedef void   (*InitFunction)    (void);
typedef void   (*MainFunction)    (void);
typedef UINT32 (*ServiceFunction) (UINT32, UINT32, UINT32);

typedef struct _Service
{
    int                 ID;         /* unique service ID */
    int                 params;     /* number of UINT32 parameters */
    InitFunction        init;       /* init function */
    MainFunction        main;       /* main function */
    ServiceFunction     request;    /* service function */
} Service;

#endif
