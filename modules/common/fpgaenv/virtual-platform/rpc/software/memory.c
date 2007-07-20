#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>
#include <stdlib.h>

#include "services.h"

/* our memory, for now, is a UINT32 aligned array */

#define MEM_SIZE    256 /* 256 * 4 = 1KB memory size */
#define CMD_LOAD    0
#define CMD_STORE   1

static UINT32   M[MEM_SIZE];

/* internal methods */
void memory_init()
{
    /* zero out memory */
    bzero(M, MEM_SIZE * sizeof(UINT32));
}

/* main exported interface method */
UINT32 memory_process_request(int argc, UINT32 argv[])
{
    /* make sure we have the correct number of args */
    if (argc != 3)
    {
        fprintf(stderr, "memory: invalid argc: %d\n", argc);
        exit(1);
    }

    /* decode */
    if (argv[0] == CMD_LOAD)
    {
        return M[argv[1]];
    }
    else if (argv[0] == CMD_STORE)
    {
        M[argv[1]] = argv[2];
        return 0;
    }
    else
    {
        fprintf(stderr, "memory: invalid command\n");
        exit(1);
    }

    return 0;
}
