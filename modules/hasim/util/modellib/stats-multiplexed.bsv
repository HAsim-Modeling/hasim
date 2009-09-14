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
//     Public module for the STAT_RECORDER_MULTIPLEXED multiple instances of
//     a single ID interface.
//


typedef STAT_VECTOR#(n_STATS) STAT_RECORDER_MULTIPLEXED#(numeric type n_STATS);


module [Connected_Module] mkStatCounter_Multiplexed#(STATS_DICT_TYPE myID)
    // interface:
    (STAT_RECORDER_MULTIPLEXED#(ni))
    provisos
        (Add#(TLog#(ni), k, STAT_VECTOR_INDEX_SZ));

    STAT_RECORDER_MULTIPLEXED#(ni) m <- mkStatCounter_MultiEntry(myID);
    return m;
endmodule
