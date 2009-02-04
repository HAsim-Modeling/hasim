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
// Bit hash functions designed to produce outputs with higher entropy than
// the inputs.
//
// All the functions here preserve two important properties:
//   1.  They always return the same number of bits as the input.
//   2.  There is a unique output for every unique input.
// Given these properties it is legal to hash an address, use some bits of
// the result as a cache set index and use the remaining result bits as a
// tag.  It is not necessary to use the whole input as the tag.  Note,
// however, that the hash function is not reversible.  A writeback cache
// can't recover the original address from a tag/set derived from a hash.
//
//
// The first function, hashBits, tries to pick the best hash available for
// input sizes up to 64 bits.  The remaining functions hash specific sizes.
//


//
// hashBits --
//     Pick the best hash available for the input size.  All of the input bits
//     participate in the hash for the lower bits, even the inputs beyond the
//     base hash function's reach.  The assumption is that the low bits of the
//     result will be used as a set index and the remainder may be the tag.
//
function Bit#(n) hashBits(Bit#(n) x)
    provisos (Add#(n, a__, 64));

    let n_bits = valueOf(n);
    // Work at 64 bits, maximum.  Assume some optimization phase will drop
    // high zero bits.
    Bit#(64) h = zeroExtend(x);

    //
    // Pick the hash function nearest in size to the input.
    //

    if (n_bits >= 32)
    begin
        // Bits beyond the base hash function participate in the hash of the
        // low bits, while still preserving the property that all inputs have
        // unique outputs.
        if (n_bits > 32)
            h[31:0] = h[31:0] ^ h[63:32];

        h[31:0] = hash32(h[31:0]);
    end
    else if (n_bits >= 24)
    begin
        if (n_bits > 24)
            h[7:0] = h[7:0] ^ h[31:24];

        h[23:0] = hash24(h[23:0]);
    end
    else if (n_bits >= 16)
    begin
        if (n_bits > 16)
            h[7:0] = h[7:0] ^ h[23:16];

        h[15:0] = hash16(h[15:0]);
    end
    else if (n_bits >= 8)
    begin
        if (n_bits > 8)
            h[7:0] = h[7:0] ^ h[15:8];

        h[7:0] = hash8(h[7:0]);
    end
    
    return truncate(h);
endfunction



//
// The remaining functions hash specific sizes.
//


function Bit#(32) hash32(Bit#(32) d);
    //
    // CRC-32 (IEEE802.3), polynomial 0 1 2 4 5 7 8 10 11 12 16 22 23 26 32.
    //
    Bit#(32) hash;
    hash[0] = d[31] ^ d[30] ^ d[29] ^ d[28] ^ d[26] ^ d[25] ^ d[24] ^ 
              d[16] ^ d[12] ^ d[10] ^ d[9] ^ d[6] ^ d[0];
    hash[1] = d[28] ^ d[27] ^ d[24] ^ d[17] ^ d[16] ^ d[13] ^ d[12] ^ 
              d[11] ^ d[9] ^ d[7] ^ d[6] ^ d[1] ^ d[0];
    hash[2] = d[31] ^ d[30] ^ d[26] ^ d[24] ^ d[18] ^ d[17] ^ d[16] ^ 
              d[14] ^ d[13] ^ d[9] ^ d[8] ^ d[7] ^ d[6] ^ d[2] ^ 
              d[1] ^ d[0];
    hash[3] = d[31] ^ d[27] ^ d[25] ^ d[19] ^ d[18] ^ d[17] ^ d[15] ^ 
              d[14] ^ d[10] ^ d[9] ^ d[8] ^ d[7] ^ d[3] ^ d[2] ^ 
              d[1];
    hash[4] = d[31] ^ d[30] ^ d[29] ^ d[25] ^ d[24] ^ d[20] ^ d[19] ^ 
              d[18] ^ d[15] ^ d[12] ^ d[11] ^ d[8] ^ d[6] ^ d[4] ^ 
              d[3] ^ d[2] ^ d[0];
    hash[5] = d[29] ^ d[28] ^ d[24] ^ d[21] ^ d[20] ^ d[19] ^ d[13] ^ 
              d[10] ^ d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[3] ^ d[1] ^ d[0];
    hash[6] = d[30] ^ d[29] ^ d[25] ^ d[22] ^ d[21] ^ d[20] ^ d[14] ^ 
              d[11] ^ d[8] ^ d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[2] ^ d[1];
    hash[7] = d[29] ^ d[28] ^ d[25] ^ d[24] ^ d[23] ^ d[22] ^ d[21] ^ 
              d[16] ^ d[15] ^ d[10] ^ d[8] ^ d[7] ^ d[5] ^ d[3] ^ 
              d[2] ^ d[0];
    hash[8] = d[31] ^ d[28] ^ d[23] ^ d[22] ^ d[17] ^ d[12] ^ d[11] ^ 
              d[10] ^ d[8] ^ d[4] ^ d[3] ^ d[1] ^ d[0];
    hash[9] = d[29] ^ d[24] ^ d[23] ^ d[18] ^ d[13] ^ d[12] ^ d[11] ^ 
              d[9] ^ d[5] ^ d[4] ^ d[2] ^ d[1];
    hash[10] = d[31] ^ d[29] ^ d[28] ^ d[26] ^ d[19] ^ d[16] ^ d[14] ^ 
               d[13] ^ d[9] ^ d[5] ^ d[3] ^ d[2] ^ d[0];
    hash[11] = d[31] ^ d[28] ^ d[27] ^ d[26] ^ d[25] ^ d[24] ^ d[20] ^ 
               d[17] ^ d[16] ^ d[15] ^ d[14] ^ d[12] ^ d[9] ^ d[4] ^ 
               d[3] ^ d[1] ^ d[0];
    hash[12] = d[31] ^ d[30] ^ d[27] ^ d[24] ^ d[21] ^ d[18] ^ d[17] ^ 
               d[15] ^ d[13] ^ d[12] ^ d[9] ^ d[6] ^ d[5] ^ d[4] ^ 
               d[2] ^ d[1] ^ d[0];
    hash[13] = d[31] ^ d[28] ^ d[25] ^ d[22] ^ d[19] ^ d[18] ^ d[16] ^ 
               d[14] ^ d[13] ^ d[10] ^ d[7] ^ d[6] ^ d[5] ^ d[3] ^ 
               d[2] ^ d[1];
    hash[14] = d[29] ^ d[26] ^ d[23] ^ d[20] ^ d[19] ^ d[17] ^ d[15] ^ 
               d[14] ^ d[11] ^ d[8] ^ d[7] ^ d[6] ^ d[4] ^ d[3] ^ 
               d[2];
    hash[15] = d[30] ^ d[27] ^ d[24] ^ d[21] ^ d[20] ^ d[18] ^ d[16] ^ 
               d[15] ^ d[12] ^ d[9] ^ d[8] ^ d[7] ^ d[5] ^ d[4] ^ 
               d[3];
    hash[16] = d[30] ^ d[29] ^ d[26] ^ d[24] ^ d[22] ^ d[21] ^ d[19] ^ 
               d[17] ^ d[13] ^ d[12] ^ d[8] ^ d[5] ^ d[4] ^ d[0];
    hash[17] = d[31] ^ d[30] ^ d[27] ^ d[25] ^ d[23] ^ d[22] ^ d[20] ^ 
               d[18] ^ d[14] ^ d[13] ^ d[9] ^ d[6] ^ d[5] ^ d[1];
    hash[18] = d[31] ^ d[28] ^ d[26] ^ d[24] ^ d[23] ^ d[21] ^ d[19] ^ 
               d[15] ^ d[14] ^ d[10] ^ d[7] ^ d[6] ^ d[2];
    hash[19] = d[29] ^ d[27] ^ d[25] ^ d[24] ^ d[22] ^ d[20] ^ d[16] ^ 
               d[15] ^ d[11] ^ d[8] ^ d[7] ^ d[3];
    hash[20] = d[30] ^ d[28] ^ d[26] ^ d[25] ^ d[23] ^ d[21] ^ d[17] ^ 
               d[16] ^ d[12] ^ d[9] ^ d[8] ^ d[4];
    hash[21] = d[31] ^ d[29] ^ d[27] ^ d[26] ^ d[24] ^ d[22] ^ d[18] ^ 
               d[17] ^ d[13] ^ d[10] ^ d[9] ^ d[5];
    hash[22] = d[31] ^ d[29] ^ d[27] ^ d[26] ^ d[24] ^ d[23] ^ d[19] ^ 
               d[18] ^ d[16] ^ d[14] ^ d[12] ^ d[11] ^ d[9] ^ d[0];
    hash[23] = d[31] ^ d[29] ^ d[27] ^ d[26] ^ d[20] ^ d[19] ^ d[17] ^ 
               d[16] ^ d[15] ^ d[13] ^ d[9] ^ d[6] ^ d[1] ^ d[0];
    hash[24] = d[30] ^ d[28] ^ d[27] ^ d[21] ^ d[20] ^ d[18] ^ d[17] ^ 
               d[16] ^ d[14] ^ d[10] ^ d[7] ^ d[2] ^ d[1];
    hash[25] = d[31] ^ d[29] ^ d[28] ^ d[22] ^ d[21] ^ d[19] ^ d[18] ^ 
               d[17] ^ d[15] ^ d[11] ^ d[8] ^ d[3] ^ d[2];
    hash[26] = d[31] ^ d[28] ^ d[26] ^ d[25] ^ d[24] ^ d[23] ^ d[22] ^ 
               d[20] ^ d[19] ^ d[18] ^ d[10] ^ d[6] ^ d[4] ^ d[3] ^ 
               d[0];
    hash[27] = d[29] ^ d[27] ^ d[26] ^ d[25] ^ d[24] ^ d[23] ^ d[21] ^ 
               d[20] ^ d[19] ^ d[11] ^ d[7] ^ d[5] ^ d[4] ^ d[1];
    hash[28] = d[30] ^ d[28] ^ d[27] ^ d[26] ^ d[25] ^ d[24] ^ d[22] ^ 
               d[21] ^ d[20] ^ d[12] ^ d[8] ^ d[6] ^ d[5] ^ d[2];
    hash[29] = d[31] ^ d[29] ^ d[28] ^ d[27] ^ d[26] ^ d[25] ^ d[23] ^ 
               d[22] ^ d[21] ^ d[13] ^ d[9] ^ d[7] ^ d[6] ^ d[3];
    hash[30] = d[30] ^ d[29] ^ d[28] ^ d[27] ^ d[26] ^ d[24] ^ d[23] ^ 
               d[22] ^ d[14] ^ d[10] ^ d[8] ^ d[7] ^ d[4];
    hash[31] = d[31] ^ d[30] ^ d[29] ^ d[28] ^ d[27] ^ d[25] ^ d[24] ^ 
               d[23] ^ d[15] ^ d[11] ^ d[9] ^ d[8] ^ d[5];

    return hash;

endfunction


function Bit#(24) hash24(Bit#(24) d);
    //
    // CRC-24, polynomial 0 1 3 4 5 6 7 10 11 14 17 18 23 24.
    //
    Bit#(24) hash;
    hash[0] = d[23] ^ d[22] ^ d[21] ^ d[20] ^ d[19] ^ d[18] ^ d[17] ^ 
              d[16] ^ d[14] ^ d[10] ^ d[5] ^ d[4] ^ d[3] ^ d[2] ^ 
              d[1] ^ d[0];
    hash[1] = d[16] ^ d[15] ^ d[14] ^ d[11] ^ d[10] ^ d[6] ^ d[0];
    hash[2] = d[17] ^ d[16] ^ d[15] ^ d[12] ^ d[11] ^ d[7] ^ d[1];
    hash[3] = d[23] ^ d[22] ^ d[21] ^ d[20] ^ d[19] ^ d[14] ^ d[13] ^ 
              d[12] ^ d[10] ^ d[8] ^ d[5] ^ d[4] ^ d[3] ^ d[1] ^ 
              d[0];
    hash[4] = d[19] ^ d[18] ^ d[17] ^ d[16] ^ d[15] ^ d[13] ^ d[11] ^ 
              d[10] ^ d[9] ^ d[6] ^ d[3] ^ d[0];
    hash[5] = d[23] ^ d[22] ^ d[21] ^ d[12] ^ d[11] ^ d[7] ^ d[5] ^ 
              d[3] ^ d[2] ^ d[0];
    hash[6] = d[21] ^ d[20] ^ d[19] ^ d[18] ^ d[17] ^ d[16] ^ d[14] ^ 
              d[13] ^ d[12] ^ d[10] ^ d[8] ^ d[6] ^ d[5] ^ d[2] ^ 
              d[0];
    hash[7] = d[23] ^ d[16] ^ d[15] ^ d[13] ^ d[11] ^ d[10] ^ d[9] ^ 
              d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[2] ^ d[0];
    hash[8] = d[17] ^ d[16] ^ d[14] ^ d[12] ^ d[11] ^ d[10] ^ d[8] ^ 
              d[7] ^ d[6] ^ d[5] ^ d[3] ^ d[1];
    hash[9] = d[18] ^ d[17] ^ d[15] ^ d[13] ^ d[12] ^ d[11] ^ d[9] ^ 
              d[8] ^ d[7] ^ d[6] ^ d[4] ^ d[2];
    hash[10] = d[23] ^ d[22] ^ d[21] ^ d[20] ^ d[17] ^ d[13] ^ d[12] ^ 
               d[9] ^ d[8] ^ d[7] ^ d[4] ^ d[2] ^ d[1] ^ d[0];
    hash[11] = d[20] ^ d[19] ^ d[17] ^ d[16] ^ d[13] ^ d[9] ^ d[8] ^ 
               d[4] ^ d[0];
    hash[12] = d[21] ^ d[20] ^ d[18] ^ d[17] ^ d[14] ^ d[10] ^ d[9] ^ 
               d[5] ^ d[1];
    hash[13] = d[22] ^ d[21] ^ d[19] ^ d[18] ^ d[15] ^ d[11] ^ d[10] ^ 
               d[6] ^ d[2];
    hash[14] = d[21] ^ d[18] ^ d[17] ^ d[14] ^ d[12] ^ d[11] ^ d[10] ^ 
               d[7] ^ d[5] ^ d[4] ^ d[2] ^ d[1] ^ d[0];
    hash[15] = d[22] ^ d[19] ^ d[18] ^ d[15] ^ d[13] ^ d[12] ^ d[11] ^ 
               d[8] ^ d[6] ^ d[5] ^ d[3] ^ d[2] ^ d[1];
    hash[16] = d[23] ^ d[20] ^ d[19] ^ d[16] ^ d[14] ^ d[13] ^ d[12] ^ 
               d[9] ^ d[7] ^ d[6] ^ d[4] ^ d[3] ^ d[2];
    hash[17] = d[23] ^ d[22] ^ d[19] ^ d[18] ^ d[16] ^ d[15] ^ d[13] ^ 
               d[8] ^ d[7] ^ d[2] ^ d[1] ^ d[0];
    hash[18] = d[22] ^ d[21] ^ d[18] ^ d[10] ^ d[9] ^ d[8] ^ d[5] ^ 
               d[4] ^ d[0];
    hash[19] = d[23] ^ d[22] ^ d[19] ^ d[11] ^ d[10] ^ d[9] ^ d[6] ^ 
               d[5] ^ d[1];
    hash[20] = d[23] ^ d[20] ^ d[12] ^ d[11] ^ d[10] ^ d[7] ^ d[6] ^ 
               d[2];
    hash[21] = d[21] ^ d[13] ^ d[12] ^ d[11] ^ d[8] ^ d[7] ^ d[3];
    hash[22] = d[22] ^ d[14] ^ d[13] ^ d[12] ^ d[9] ^ d[8] ^ d[4];
    hash[23] = d[22] ^ d[21] ^ d[20] ^ d[19] ^ d[18] ^ d[17] ^ d[16] ^ 
               d[15] ^ d[13] ^ d[9] ^ d[4] ^ d[3] ^ d[2] ^ d[1] ^ 
               d[0];

    return hash;

endfunction


function Bit#(16) hash16(Bit#(16) d);
    //
    // CRC-16, polynomial 0 2 15 16.
    //
    Bit#(16) hash;
    hash[0] = d[15] ^ d[13] ^ d[12] ^ d[11] ^ d[10] ^ d[9] ^ d[8] ^
              d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[3] ^ d[2] ^ d[1] ^ d[0];
    hash[1] = d[14] ^ d[13] ^ d[12] ^ d[11] ^ d[10] ^ d[9] ^ d[8] ^
              d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[3] ^ d[2] ^ d[1];
    hash[2] = d[14] ^ d[1] ^ d[0];
    hash[3] = d[15] ^ d[2] ^ d[1];
    hash[4] = d[3] ^ d[2];
    hash[5] = d[4] ^ d[3];
    hash[6] = d[5] ^ d[4];
    hash[7] = d[6] ^ d[5];
    hash[8] = d[7] ^ d[6];
    hash[9] = d[8] ^ d[7];
    hash[10] = d[9] ^ d[8];
    hash[11] = d[10] ^ d[9];
    hash[12] = d[11] ^ d[10];
    hash[13] = d[12] ^ d[11];
    hash[14] = d[13] ^ d[12];
    hash[15] = d[15] ^ d[14] ^ d[12] ^ d[11] ^ d[10] ^ d[9] ^ d[8] ^
               d[7] ^ d[6] ^ d[5] ^ d[4] ^ d[3] ^ d[2] ^ d[1] ^ d[0];

    return hash;

endfunction


function Bit#(8) hash8(Bit#(8) d);
    //
    // CRC-8 (ATM HEC), polynomial 0 1 2 8.
    //
    Bit#(8) hash;
    hash[0] = d[7] ^ d[6] ^ d[0];
    hash[1] = d[6] ^ d[1] ^ d[0];
    hash[2] = d[6] ^ d[2] ^ d[1] ^ d[0];
    hash[3] = d[7] ^ d[3] ^ d[2] ^ d[1];
    hash[4] = d[4] ^ d[3] ^ d[2];
    hash[5] = d[5] ^ d[4] ^ d[3];
    hash[6] = d[6] ^ d[5] ^ d[4];
    hash[7] = d[7] ^ d[6] ^ d[5];

    return hash;

endfunction


//
// Multiple variants of 8 bit hashes, useful for Bloom filters.
//

function Bit#(8) hash8a(Bit#(8) d);
    //
    // CRC-8 (CCITT), polynomial 0 2 3 7 8.
    //
    Bit#(8) hash;
    hash[0] = d[4] ^ d[3] ^ d[2] ^ d[1] ^ d[0];
    hash[1] = d[5] ^ d[4] ^ d[3] ^ d[2] ^ d[1];
    hash[2] = d[6] ^ d[5] ^ d[1] ^ d[0];
    hash[3] = d[7] ^ d[6] ^ d[4] ^ d[3] ^ d[0];
    hash[4] = d[7] ^ d[5] ^ d[4] ^ d[1];
    hash[5] = d[6] ^ d[5] ^ d[2];
    hash[6] = d[7] ^ d[6] ^ d[3];
    hash[7] = d[7] ^ d[3] ^ d[2] ^ d[1] ^ d[0];

    return hash;

endfunction


function Bit#(8) hash8b(Bit#(8) d);
    //
    // CRC-8 (Dallas/Maxim), polynomial 0 4 5 8.
    //
    Bit#(8) hash;
    hash[0] = d[6] ^ d[4] ^ d[3] ^ d[0];
    hash[1] = d[7] ^ d[5] ^ d[4] ^ d[1];
    hash[2] = d[6] ^ d[5] ^ d[2];
    hash[3] = d[7] ^ d[6] ^ d[3];
    hash[4] = d[7] ^ d[6] ^ d[3] ^ d[0];
    hash[5] = d[7] ^ d[6] ^ d[3] ^ d[1] ^ d[0];
    hash[6] = d[7] ^ d[4] ^ d[2] ^ d[1];
    hash[7] = d[5] ^ d[3] ^ d[2];

    return hash;

endfunction


function Bit#(8) hash8c(Bit#(8) d);
    //
    // CRC-8 (SAE J1850), polynomial 0 2 3 4 8.
    //
    Bit#(8) hash;
    hash[0] = d[6] ^ d[5] ^ d[4] ^ d[0];
    hash[1] = d[7] ^ d[6] ^ d[5] ^ d[1];
    hash[2] = d[7] ^ d[5] ^ d[4] ^ d[2] ^ d[0];
    hash[3] = d[4] ^ d[3] ^ d[1] ^ d[0];
    hash[4] = d[6] ^ d[2] ^ d[1] ^ d[0];
    hash[5] = d[7] ^ d[3] ^ d[2] ^ d[1];
    hash[6] = d[4] ^ d[3] ^ d[2];
    hash[7] = d[5] ^ d[4] ^ d[3];

    return hash;

endfunction


function Bit#(8) hash8d(Bit#(8) d);
    //
    // CRC-8, polynomial 0 2 4 6 7 8.
    //
    Bit#(8) hash;
    hash[0] = d[7] ^ d[6] ^ d[3] ^ d[1] ^ d[0];
    hash[1] = d[7] ^ d[4] ^ d[2] ^ d[1];
    hash[2] = d[7] ^ d[6] ^ d[5] ^ d[2] ^ d[1] ^ d[0];
    hash[3] = d[7] ^ d[6] ^ d[3] ^ d[2] ^ d[1];
    hash[4] = d[6] ^ d[4] ^ d[2] ^ d[1] ^ d[0];
    hash[5] = d[7] ^ d[5] ^ d[3] ^ d[2] ^ d[1];
    hash[6] = d[7] ^ d[4] ^ d[2] ^ d[1] ^ d[0];
    hash[7] = d[7] ^ d[6] ^ d[5] ^ d[2] ^ d[0];

    return hash;

endfunction
