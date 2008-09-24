//
// Copyright (C) 2008 Massachusetts Institute of Technology
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
// Tokens are the main way for HAsim to track data across simulator      
// partitions. The token type includes an index for token tables, epochs,
// and scratchpads which partitions can use as they see fit.             


typedef `TOKEN_INDEX_BITS TOKEN_INDEX_SIZE;
typedef TExp#(TOKEN_INDEX_SIZE) NUM_TOKENS;
typedef Bit#(TOKEN_INDEX_SIZE) TOKEN_INDEX;

typedef Bit#(`TOKEN_TIMEP_EPOCH_BITS)      TOKEN_TIMEP_EPOCH;
typedef Bit#(`TOKEN_TIMEP_SCRATCHPAD_BITS) TOKEN_TIMEP_SCRATCHPAD;

typedef Bit#(`TOKEN_FUNCP_EPOCH_BITS)     TOKEN_FUNCP_EPOCH;
typedef Bit#(`TOKEN_FUNCP_SCRATCHPAD_BITS)TOKEN_FUNCP_SCRATCHPAD;

typedef struct
{
    TOKEN_TIMEP_EPOCH      epoch;
    TOKEN_TIMEP_SCRATCHPAD scratchpad;
}
    TOKEN_TIMEP_INFO 
        deriving 
            (Eq, Bits);

typedef struct
{
    TOKEN_FUNCP_EPOCH      epoch;
    TOKEN_FUNCP_SCRATCHPAD scratchpad;
}
    TOKEN_FUNCP_INFO 
        deriving (Eq, Bits);

typedef struct
{
    TOKEN_INDEX       index;
    TOKEN_TIMEP_INFO  timep_info;
    TOKEN_FUNCP_INFO  funcp_info;
}
    TOKEN 
        deriving (Eq, Bits);

//isOlder: predicated on the idea that only half the tokens are in flight at once.

function Bool isOlder(TOKEN_INDEX t1, TOKEN_INDEX t2);

  return (t1 - t2) > (t2 - t1);

endfunction
