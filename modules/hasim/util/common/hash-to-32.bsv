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
// Hash an arbitrary number of bits into a 32 bit result.  Each bit is
// supposed to be random, so feel free to truncate the result and use
// however many bits are appropriate.
//

function Bit#(32) hashTo32(Bit#(n) x)
    // A very silly pair of provisos required by the compiler: a__+32 == n+32
    provisos(Add#(32, n, TAdd#(a__, 32)),
             Add#(a__, 32, TAdd#(32, n)));

    //
    // Hash function really wants a 32 bit input.  Construct one.
    //
    Bit#(32) d = truncate({ 32'b0, x });
    for (Integer i = 32; i < valueOf(n); i = i + 1)
    begin
        d[i % 32] = d[i % 32] ^ x[i];
    end

    //
    // CRC-32 (IEEE802.3), polynomial 0 1 2 4 5 7 8 10 11 12 16 22 23 26 32
    //   Define a large CRC result so we can grow the caches without changing
    //   this code.  Bluespec optimizer will drop extra bits.
    //
    Bit#(32) set;
    set[0] = d[31] ^ d[30] ^ d[29] ^ d[28] ^ d[26] ^ d[25] ^ d[24] ^ 
             d[16] ^ d[12] ^ d[10] ^ d[9] ^ d[6] ^ d[0];
    set[1] = d[28] ^ d[27] ^ d[24] ^ d[17] ^ d[16] ^ d[13] ^ d[12] ^ 
             d[11] ^ d[9] ^ d[7] ^ d[6] ^ d[1] ^ d[0];
    set[2] = d[31] ^ d[30] ^ d[26] ^ d[24] ^ d[18] ^ d[17] ^ d[16] ^ 
             d[14] ^ d[13] ^ d[9] ^ d[8] ^ d[7] ^ d[6] ^ d[2] ^ 
             d[1] ^ d[0];
    set[3] = d[31] ^ d[27] ^ d[25] ^ d[19] ^ d[18] ^ d[17] ^ d[15] ^ 
             d[14] ^ d[10] ^ d[9] ^ d[8] ^ d[7] ^ d[3] ^ d[2] ^ 
             d[1];
    set[4] = d[31] ^ d[30] ^ d[29] ^ d[25] ^ d[24] ^ d[20] ^ d[19] ^ 
             d[18] ^ d[15] ^ d[12] ^ d[11] ^ d[8] ^ d[6] ^ d[4] ^ 
             d[3] ^ d[2] ^ d[0];
    set[5] = d[29] ^ d[28] ^ d[24] ^ d[21] ^ d[20] ^ d[19] ^ d[13] ^ 
             d[10] ^ d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[3] ^ d[1] ^ d[0];
    set[6] = d[30] ^ d[29] ^ d[25] ^ d[22] ^ d[21] ^ d[20] ^ d[14] ^ 
             d[11] ^ d[8] ^ d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[2] ^ d[1];
    set[7] = d[29] ^ d[28] ^ d[25] ^ d[24] ^ d[23] ^ d[22] ^ d[21] ^ 
             d[16] ^ d[15] ^ d[10] ^ d[8] ^ d[7] ^ d[5] ^ d[3] ^ 
             d[2] ^ d[0];
    set[8] = d[31] ^ d[28] ^ d[23] ^ d[22] ^ d[17] ^ d[12] ^ d[11] ^ 
             d[10] ^ d[8] ^ d[4] ^ d[3] ^ d[1] ^ d[0];
    set[9] = d[29] ^ d[24] ^ d[23] ^ d[18] ^ d[13] ^ d[12] ^ d[11] ^ 
             d[9] ^ d[5] ^ d[4] ^ d[2] ^ d[1];
    set[10] = d[31] ^ d[29] ^ d[28] ^ d[26] ^ d[19] ^ d[16] ^ d[14] ^ 
              d[13] ^ d[9] ^ d[5] ^ d[3] ^ d[2] ^ d[0];
    set[11] = d[31] ^ d[28] ^ d[27] ^ d[26] ^ d[25] ^ d[24] ^ d[20] ^ 
              d[17] ^ d[16] ^ d[15] ^ d[14] ^ d[12] ^ d[9] ^ d[4] ^ 
              d[3] ^ d[1] ^ d[0];
    set[12] = d[31] ^ d[30] ^ d[27] ^ d[24] ^ d[21] ^ d[18] ^ d[17] ^ 
              d[15] ^ d[13] ^ d[12] ^ d[9] ^ d[6] ^ d[5] ^ d[4] ^ 
              d[2] ^ d[1] ^ d[0];
    set[13] = d[31] ^ d[28] ^ d[25] ^ d[22] ^ d[19] ^ d[18] ^ d[16] ^ 
              d[14] ^ d[13] ^ d[10] ^ d[7] ^ d[6] ^ d[5] ^ d[3] ^ 
              d[2] ^ d[1];
    set[14] = d[29] ^ d[26] ^ d[23] ^ d[20] ^ d[19] ^ d[17] ^ d[15] ^ 
              d[14] ^ d[11] ^ d[8] ^ d[7] ^ d[6] ^ d[4] ^ d[3] ^ 
              d[2];
    set[15] = d[30] ^ d[27] ^ d[24] ^ d[21] ^ d[20] ^ d[18] ^ d[16] ^ 
              d[15] ^ d[12] ^ d[9] ^ d[8] ^ d[7] ^ d[5] ^ d[4] ^ 
              d[3];
    set[16] = d[30] ^ d[29] ^ d[26] ^ d[24] ^ d[22] ^ d[21] ^ d[19] ^ 
              d[17] ^ d[13] ^ d[12] ^ d[8] ^ d[5] ^ d[4] ^ d[0];
    set[17] = d[31] ^ d[30] ^ d[27] ^ d[25] ^ d[23] ^ d[22] ^ d[20] ^ 
              d[18] ^ d[14] ^ d[13] ^ d[9] ^ d[6] ^ d[5] ^ d[1];
    set[18] = d[31] ^ d[28] ^ d[26] ^ d[24] ^ d[23] ^ d[21] ^ d[19] ^ 
              d[15] ^ d[14] ^ d[10] ^ d[7] ^ d[6] ^ d[2];
    set[19] = d[29] ^ d[27] ^ d[25] ^ d[24] ^ d[22] ^ d[20] ^ d[16] ^ 
              d[15] ^ d[11] ^ d[8] ^ d[7] ^ d[3];
    set[20] = d[30] ^ d[28] ^ d[26] ^ d[25] ^ d[23] ^ d[21] ^ d[17] ^ 
              d[16] ^ d[12] ^ d[9] ^ d[8] ^ d[4];
    set[21] = d[31] ^ d[29] ^ d[27] ^ d[26] ^ d[24] ^ d[22] ^ d[18] ^ 
              d[17] ^ d[13] ^ d[10] ^ d[9] ^ d[5];
    set[22] = d[31] ^ d[29] ^ d[27] ^ d[26] ^ d[24] ^ d[23] ^ d[19] ^ 
              d[18] ^ d[16] ^ d[14] ^ d[12] ^ d[11] ^ d[9] ^ d[0];
    set[23] = d[31] ^ d[29] ^ d[27] ^ d[26] ^ d[20] ^ d[19] ^ d[17] ^ 
              d[16] ^ d[15] ^ d[13] ^ d[9] ^ d[6] ^ d[1] ^ d[0];
    set[24] = d[30] ^ d[28] ^ d[27] ^ d[21] ^ d[20] ^ d[18] ^ d[17] ^ 
              d[16] ^ d[14] ^ d[10] ^ d[7] ^ d[2] ^ d[1];
    set[25] = d[31] ^ d[29] ^ d[28] ^ d[22] ^ d[21] ^ d[19] ^ d[18] ^ 
              d[17] ^ d[15] ^ d[11] ^ d[8] ^ d[3] ^ d[2];
    set[26] = d[31] ^ d[28] ^ d[26] ^ d[25] ^ d[24] ^ d[23] ^ d[22] ^ 
              d[20] ^ d[19] ^ d[18] ^ d[10] ^ d[6] ^ d[4] ^ d[3] ^ 
              d[0];
    set[27] = d[29] ^ d[27] ^ d[26] ^ d[25] ^ d[24] ^ d[23] ^ d[21] ^ 
              d[20] ^ d[19] ^ d[11] ^ d[7] ^ d[5] ^ d[4] ^ d[1];
    set[28] = d[30] ^ d[28] ^ d[27] ^ d[26] ^ d[25] ^ d[24] ^ d[22] ^ 
              d[21] ^ d[20] ^ d[12] ^ d[8] ^ d[6] ^ d[5] ^ d[2];
    set[29] = d[31] ^ d[29] ^ d[28] ^ d[27] ^ d[26] ^ d[25] ^ d[23] ^ 
              d[22] ^ d[21] ^ d[13] ^ d[9] ^ d[7] ^ d[6] ^ d[3];
    set[30] = d[30] ^ d[29] ^ d[28] ^ d[27] ^ d[26] ^ d[24] ^ d[23] ^ 
              d[22] ^ d[14] ^ d[10] ^ d[8] ^ d[7] ^ d[4];
    set[31] = d[31] ^ d[30] ^ d[29] ^ d[28] ^ d[27] ^ d[25] ^ d[24] ^ 
              d[23] ^ d[15] ^ d[11] ^ d[9] ^ d[8] ^ d[5];

    return set;

endfunction
