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

// ========================================================================
//
//  Global types
//
// ========================================================================

// UP_TO_TWO

typedef union tagged
{
    a ONE;
    Tuple2#(a, a) TWO;
}
    UP_TO_TWO#(parameter type a)
        deriving (Eq, Bits);

function a getFirst(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return x;
        tagged TWO {.x1, .x2}: return x1;
    endcase

endfunction

function Bool hasSecond(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return False;
        tagged TWO {.x1, .x2}: return True;
    endcase

endfunction

function Maybe#(a) getSecond(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return tagged Invalid;
        tagged TWO {.x1, .x2}: return tagged Valid x2;
    endcase

endfunction

function a getSecondOfTwo(UP_TO_TWO#(a) d);

    case (d) matches
        tagged ONE .x:         return ?;
        tagged TWO {.x1, .x2}: return x2;
    endcase

endfunction
