//
// Copyright (C) 2009 Massachusetts Institute of Technology
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

`include "asim/provides/hasim_common.bsh"

//
// mkStatCounter_Multiplexed --
//     Multiplexed statistics.  This used to be specially written for HAsim,
//     but the LEAP statistics now provide a suitable version.
//
module [CONNECTED_MODULE] mkStatCounter_Multiplexed#(STAT_ID statID)
    // interface:
    (STAT_VECTOR#(n_STATS))
    provisos
        (NumAlias#(TMax#(TLog#(n_STATS), 1), t_STAT_IDX_SZ),
         Add#(t_STAT_IDX_SZ, k, STAT_VECTOR_INDEX_SZ));

    let m <- mkStatCounter_RAM(statID);
    return m;
endmodule
