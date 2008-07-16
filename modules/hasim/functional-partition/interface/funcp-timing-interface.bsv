//
// Copyright (C) 2008 Intel Corporation
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

//
// Interface from a timing model to the functional partition.
//

// Project foundation includes

`include "asim/provides/hasim_common.bsh"

//
// Functional partition includes.  BEWARE!  Timing models include this file.
// Do not include anything but functional partition data definitions.
//
`include "asim/provides/funcp_base_types.bsh"

// ISA includes

`include "asim/provides/hasim_isa.bsh"
// `include "asim/provides/hasim_isa_datapath.bsh"


//----------------------------------------------------------------------------
//
// funcp_getResults -- execute stage
//
//     Input:   TOKEN
//
//     Output:  FUNCP_GET_RESULTS_MSG
//
//----------------------------------------------------------------------------

typedef struct
{
    TOKEN token;
    ISA_ADDRESS instrAddr;             // Address of the executed instruction
    Bit#(4) instrSize;                 // Size of the executed instruction
    ISA_EXECUTION_RESULT result;       // Result from ALU
}
    FUNCP_GET_RESULTS_MSG
        deriving (Eq, Bits);
