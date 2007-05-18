#include <sys/select.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* virtualized I/O happens at the granularity of "packets",
 * but to reduce overheads we physically do selects, reads
 * and writes at the granularity of "blocks" */
#define PACKET_SIZE     32
#define BLOCK_SIZE      32

static int      terminated = 0;
static int      initialized = 0;

static int      inPipe[2],
                outPipe[2];

static fd_set   readfds;
static int      childpid;

/* need caches */
static unsigned int     inputCache;
static unsigned int     outputCache;

/* unprocessed input: use pair of buffers for speed */
static char             unprocBuffer[2][PACKET_SIZE];
static int              ubIndex = 0;
static int              ubActive = 0;

#define PARENT_READ     inPipe[0]
#define CHILD_WRITE     inPipe[1]
#define CHILD_READ      outPipe[0]
#define PARENT_WRITE    outPipe[1]

/* create process and initialize data structures */
unsigned char cio_open(unsigned char programID)
{
    int i;

    if (initialized == 1)
    {
        /* already initialized */
        return 0;
    }

    initialized = 1;

    /* create I/O pipes */
    if (pipe(inPipe) < 0 || pipe(outPipe) < 0)
    {
        /* TODO: insert exception handler */
        return 0;
    }

    /* fork off to create dialog box */
    childpid = fork();
    if (childpid < 0)
    {
        /* TODO: insert exception handler */
        return 0;
    }

    if (childpid == 0)
    {
        /* CHILD: setup pipes for dialog box side */
        close(PARENT_READ);
        close(PARENT_WRITE);

        dup2(CHILD_READ, 0);
        dup2(CHILD_WRITE, 1);

        /* exec */
        execlp("hasim-front-panel", "hasim-front-panel", NULL);
    }
    else
    {
        /* PARENT: setup pipes for model side */
        close(CHILD_READ);
        close(CHILD_WRITE);

        /* flags */
        terminated = 0;
    }

    /* for now, only allow 1 instance, return handle = 1 for this */
    return 1;
}

/* check if dialog box has quit */
int termCheck()
{
    /* sanity check */
    if (initialized == 0)
    {
        /* invalid call */
        fprintf(stderr, "termCheck called without initialization\n");
        exit(1);
    }

    /* find out if child has exited */
    if (terminated == 0)
    {
        if (waitpid(childpid, NULL, WNOHANG|WUNTRACED) == childpid)
        {
            /* uh-oh... child has exited... what to do?
             * for now, lets use this code as a sink and
             * allow the BSV model to continue */
            terminated = 1;

            /* close pipes */
            close(PARENT_READ);
            close(PARENT_WRITE);

            return 1;
        }
    }
    else
    {
        /* terminated earlier */
        return 1;
    }

    return 0;
}

/* read one packet of data */
unsigned int cio_read(unsigned char handle)
{
    struct timeval timeout;
    unsigned int retval;
    int done;

    /* this code needs to be present at the beginning of
     * all interface methods visible to the BSV model */
    if (initialized == 0)
    {
        return 0;
    }
    if (termCheck())
    {
        return 0;
    }

    /* note that our input technique is not a continuous
     * polling loop; the BSV model calls this method on
     * demand, and we scan the buffered inputs on the pipe.
     * A pipe communication delay could mean that two inputs
     * that were (hypothetically) sent simultaneously from
     * the dialog box could become visible to the BSV model
     * on successive clock cycles */

    /* also, multiple inputs on the pipe will overwrite
     * each other; we only return the latest state */

    /* scan input on pipe: for each select, we read in a
     * block's worth of data. Since there is no bound on
     * how frequently (or infrequently) this method is going
     * to be called, we need to loop and read in as much data
     * from the pipe as possible. */
    done = 0;
    do
    {
        int data_available;
        int i;
        int bytes_requested;
        int bytes_read;

        FD_ZERO(&readfds);
        FD_SET(PARENT_READ, &readfds);

        timeout.tv_sec  = 0;
        timeout.tv_usec = 100000;

        data_available = select(PARENT_READ + 1, &readfds,
                                NULL, NULL, &timeout);

        if (data_available == -1)
        {
            perror("select");
            exit(1);
        }

        if (data_available == 0)
        {
            done = 1;
            break;
        }

        /* incoming! */

        /* sanity check */
        if (data_available != 1 || FD_ISSET(PARENT_READ, &readfds) == 0)
        {
            fprintf(stderr, "activity detected on unknown descriptor\n");
            exit(1);
        }

        /* read in data, but attempt to keep it aligned */
        bytes_requested = BLOCK_SIZE - ubIndex;

        bytes_read = read(PARENT_READ,
                          &unprocBuffer[ubActive][ubIndex],
                          bytes_requested);

        ubIndex += bytes_read;

        if (bytes_read < bytes_requested)
        {
            /* we will stop attempting to read anything else from the
             * pipe during this method call. This *could* be an EOF */
            done = 1;
        }

        /* attempt to transfer data from the unprocessed data buffer
         * into the input state cache
         *
         * OPTIMIZATION: ignore all but the last available packets of
         * data. Note that this is only applicable because of the
         * particular properties of the input device we are modeling */
        if (ubIndex >= PACKET_SIZE)
        {
            /* locate index of last full packet */
            int incomplete = ubIndex % PACKET_SIZE;
            int loc = ubIndex - PACKET_SIZE - incomplete;

            /* Perl quirk: incoming data is text bit stream, convert to int */
            int bit_index;
            unsigned int mask = 1;
            inputCache = 0;
            for (bit_index = 0; bit_index < PACKET_SIZE; bit_index++)
            {
                if (unprocBuffer[ubActive][loc + bit_index] == '1')
                {
                    inputCache = inputCache + (mask << bit_index);
                }
            }

            // memcpy(&inputCache, &unprocBuffer[ubActive][loc], PACKET_SIZE);

            /* copy over incomplete chunk into the other unprocBuffer,
             * and swap active buffers */
            if (incomplete > 0)
            {
                memcpy(&unprocBuffer[1 - ubActive][0],
                       &unprocBuffer[ubActive][ubIndex - incomplete],
                       incomplete);
            }
            ubActive = 1 - ubActive;
            ubIndex = incomplete;
        }
    }
    while (!done);

    /* scan complete; return state of cache */
    return inputCache;
}

/* write one LED */
void cio_write(unsigned char handle, unsigned int data)
{   
    /* this code needs to be present at the beginning of
     * all interface methods visible to the BSV model */
    if (termCheck())
    {
        return;
    }

    /* probe cache to see if the output state has indeed changed */
    if (data != outputCache)
    {
        int bytes_written;
        char databuf[PACKET_SIZE+1]; // +1 for debug, remove later
        int i, mask;

        /* yes, it has... update internal state */
        outputCache = data;

        /* Perl doesn't seem to play nice with binary data, so convert
         * the data into an ASCII text bit stream (ugh) */
        mask = 1;
        for (i = 0; i < PACKET_SIZE; i++)
        {
            char bit = (mask & outputCache) ? '1' : '0';
            databuf[i] = bit;
            mask = mask << 1;
        }

        /* send message on pipe */
        bytes_written = write(PARENT_WRITE, databuf, PACKET_SIZE);
        if (bytes_written == -1)
        {
            perror("write");
            exit(1);
        }
        else if (bytes_written < PACKET_SIZE)
        {
            fprintf(stderr, "could not write complete packet.\n");
            exit(1);
        }
    }
}

/* The BSV model can probe this flag to figure out if the *
 * user has exited from the dialog box, and, if it so     *
 * chooses, terminate simulation                          */
char cio_isdestroyed(unsigned char handle)
{
    /* this code needs to be present at the beginning of
     * all interface methods visible to the BSV model */
    if (termCheck())
    {
        return 1;
    }
    else
    {
        return 0;
    }
}
