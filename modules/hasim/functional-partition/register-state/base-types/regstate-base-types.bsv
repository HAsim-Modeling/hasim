//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

import FShow::*;

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/funcp_base_types.bsh"

//
// Base types for register state in the functional partition
//

//
// FUNCP_REGSTATE_MEMPATH --
//     Used in the register state memory connection module to route memory
//     requests back to the right register state pipeline.  This data type
//     is here because it is part of the FUNCP_MEMREF_TOKEN passed to
//     the functional memory subsystem.
//
typedef enum
{
    FUNCP_REGSTATE_MEMPATH_GETINST,
    FUNCP_REGSTATE_MEMPATH_DOLOADS,
    FUNCP_REGSTATE_MEMPATH_DOSTORES,
    FUNCP_REGSTATE_MEMPATH_COMMIT
}
FUNCP_REGSTATE_MEMPATH
    deriving (Eq, Bits);


//
// Number of entries in each of the register state memory connection
// scoreboard FIFOs.  There is one FIFO per MEMPATH, defined above.
// Defined here because it affects the FUNCP_MEMREF_TOKEN.
//
// The scoreboard is stored in BRAM, so there is no harm in allowing
// a relatively large number of references to be in flight.  Small
// values would just waste most of a BRAM chunk.
//
typedef 64 MAX_FUNCP_INFLIGHT_MEMREFS;
typedef SCOREBOARD_FIFO_ENTRY_ID#(MAX_FUNCP_INFLIGHT_MEMREFS) FUNCP_MEMRESP_SCOREBOARD_ID;


//
// FUNCP_MEMREF_TOKEN --
//     Token defined by the register state memory connection module and passed
//     to the memory subsystem.  It is returned to the register state connection
//     manager along with the data.  This permits the memory subsytem to return
//     results out of order as soon as they are ready.
//
typedef struct
{
    FUNCP_REGSTATE_MEMPATH memPath;
    FUNCP_MEMRESP_SCOREBOARD_ID entryId;
}
FUNCP_MEMREF_TOKEN
    deriving (Eq, Bits);

function FUNCP_MEMREF_TOKEN memRefToken(FUNCP_REGSTATE_MEMPATH memPath,
                                        FUNCP_MEMRESP_SCOREBOARD_ID entryId);
    return FUNCP_MEMREF_TOKEN { memPath: memPath, entryId: entryId };
endfunction

instance FShow#(FUNCP_MEMREF_TOKEN);
    function Fmt fshow(FUNCP_MEMREF_TOKEN memRefTok);
        return $format("MEMREF (%0d, %0d)", memRefTok.memPath, memRefTok.entryId);
    endfunction
endinstance
