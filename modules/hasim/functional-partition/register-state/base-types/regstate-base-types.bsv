//
// Copyright (C) 2009 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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
    // DoStores pipeline may request a load if doing a read-modify-write
    FUNCP_REGSTATE_MEMPATH_DOSTORES_LOAD,
    FUNCP_REGSTATE_MEMPATH_DOSTORES_STORE
}
FUNCP_REGSTATE_MEMPATH
    deriving (Eq, Bits);


//
// Number of entries in each of the register state memory connection
// scoreboard FIFOs.  There is one FIFO per MEMPATH, defined above.
// Defined here because it affects the FUNCP_MEMREF_TOKEN.
//
typedef 8 MAX_FUNCP_INFLIGHT_MEMREFS;
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
