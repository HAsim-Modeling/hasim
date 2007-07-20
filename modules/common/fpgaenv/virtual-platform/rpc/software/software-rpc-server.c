#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>
#include <stdlib.h>

#include "software-rpc-server.h"
#include "front-panel.h"

/* this is the main HAsim software server */

/* service map */
Service ServiceMap[MAX_SERVICES];

/* internal methods */
static void init()
{
    int i;

    /* initialize service table. For now, we hard-code
     * the service names, but we should come up with a
     * more sophisticated way to do this */
    ServiceMap[0].ID        = 0;
    ServiceMap[0].params    = 1;
    ServiceMap[0].init      = front_panel_init;
    ServiceMap[0].main      = NULL;
    ServiceMap[0].request   = front_panel_request;

    /*
    ServiceMap[1].ID        = 1;
    ServiceMap[1].params    = 2;
    ServiceMap[1].main_func = NULL;
    ServiceMap[1].req_func  = NULL;

    ServiceMap[2].ID        = 2;
    ServiceMap[2].params    = 3;
    ServiceMap[2].main_func = NULL;
    ServiceMap[2].req_func  = memory_request;
    memory_init();
    */

    /* initialize individual services */
    for (i = 0; i < MAX_SERVICES; i++)
    {
        ServiceMap[i].init();
    }
}

static void unpack(UINT32 src, unsigned char dst[])
{
    /* unpack UINT32 into byte sequence */
    unsigned int mask = 0xFF;
    int i;
    for (i = 0; i < CHANNELIO_PACKET_SIZE; i++)
    {
        unsigned char byte = (mask & src) >> (i * 8);
        dst[i] = (unsigned char)byte;
        mask = mask << 8;
    }
}

static UINT32 pack(unsigned char dst[])
{
    UINT32 retval = 0;
    int i;
    for (i = 0; i < CHANNELIO_PACKET_SIZE; i++)
    {
        unsigned int byte = (unsigned int)dst[i];
        retval |= (byte << (i * 8));
    }
    return retval;
}

/* main */
void software_main()
{
    unsigned char   buf[CHANNELIO_PACKET_SIZE];
    int             nbytes;
    UINT32          command;

    /* the server side is fully serialized and has no
     * notion of virtual channels. It simply picks up
     * one request from ChannelIO, ships it off to the
     * service function, waits for the service function
     * to complete, and returns the result to STDOUT.
     * The client side can perform whatever virtualization
     * it chooses to */
    init();

    /* go into an infinite loop, scanning stdin for commands */
    while ((nbytes = read(STDIN, buf, CHANNELIO_PACKET_SIZE)) != 0)
    {
        int argc;
        int i;
        UINT32 argv[MAX_ARGS];
        UINT32 result;

        /* make sure we've read the full command... for now, we'll
         * just crash, but later add a loop to complete the read TODO */
        if (nbytes != CHANNELIO_PACKET_SIZE)
        {
            fprintf(stderr, "software server: incomplete command\n");
            exit(1);
        }

        /* decode command */
        command = pack(buf);
        if (command >= MAX_SERVICES)
        {
            fprintf(stderr, "software server: invalid command: %u\n", command);
            exit(1);
        }

        /* figure out the expected number of arguments */
        // argc = ServiceMap[command].params;
        argc = 3;

        /* read args from pipe and place into args array */
        for (i = 0; i < argc; i++)
        {
            nbytes = read(STDIN, buf, CHANNELIO_PACKET_SIZE);
            assert(nbytes == CHANNELIO_PACKET_SIZE);  /* ugh... TODO */
            argv[i] = pack(buf);
        }

        /* invoke local service method to obtain result */
        // result = ServiceMap[command].request(argc, argv);
        result = ServiceMap[command].request(argv[0], argv[1], argv[2]);

        /* send result to ChannelIO */
        unpack(result, buf);
        nbytes = write(STDOUT, buf, CHANNELIO_PACKET_SIZE);
        assert(nbytes == CHANNELIO_PACKET_SIZE);    /* ugh */
    }

    exit(0);
}
