#include <stdio.h>
#include <stdlib.h>

#include "vmh-utils.h"

/* load image into memory */
bool vmh_load_image(const char *filename, void *memory, UINT64 memsize)
{
    /* assume file is in VMH format */
    char *line;
    size_t len = 0;
    int read;

    /* deal in chunks of 32 bits */
    UINT32 *M = (UINT32*)(memory);
    memsize >>= 2;

    FILE *file = fopen(filename, "r");

    if (file == NULL)
    {
        fprintf(stderr, "vmh-utils: could not open VMH file %s.\n",
                filename);
        return false;
    }

    /* current address pointer */
    UINT32 addr = 0;

    while ((read = getline(&line, &len, file)) != -1)
    {
        if (read != 0)
        {
            /* is it a new address segment? */
            if (line[0] == '@')
            {
                /* recover the rest of the address */
                addr = strtoul(&line[1], NULL, 16);

                /* address in VMH file is already word
                 * aligned, so no shifting required */
            }
            else
            {
                /* read in the 32-bit aligned data element
                   and store it in memory at the current
                   address
                   
                   NOTE: at the moment, since we only allow
                   32-bit aligned loads/stores, we do not
                   care about endianness.
                          *** TODO: ENDIANNESS ***
                   */
                UINT32 data = strtoul(line, NULL, 16);
                M[addr] = data;

                /* increment address pointer - since our
                   array is UINT32-aligned, we have to
                   increment the address by 1 to get to
                   the next word */
                addr = addr + 1;
            }

            /* make sure we don't overflow! */
            if (addr >= memsize)
            {
                fprintf(stderr, "memory overflow: image is too large.\n");
                fclose(file);
                return false;
            }
        }
    }

    /* all done */
    if (line)
        free(line);
    fclose(file);

    return true;
}

