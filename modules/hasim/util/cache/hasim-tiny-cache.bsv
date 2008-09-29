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
// Author: Michael Adler
//
// A generic n entry cache class with same-cycle results for read.  Don't
// make the number of entries too big since all of them are searched in a
// single cycle!  Two or four entries are usually the best choices.
//

// Library imports.

import FIFO::*;
import Vector::*;

// Project foundation imports.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/fpga_components.bsh"


// ===================================================================
//
// PUBLIC DATA STRUCTURES
//
// ===================================================================

//
// HAsim tiny cache interface.
//
interface HASIM_TINY_CACHE#(type t_CACHE_ADDR,
                            type t_CACHE_DATA,
                            numeric type nEntries);

    // Read a line, returns invalid if not found.
    method ActionValue#(Maybe#(t_CACHE_DATA)) read(t_CACHE_ADDR addr);
    
    // Write
    method Action write(t_CACHE_ADDR addr, t_CACHE_DATA data);

    // Invalidate entire cache
    method Action invalAll();

endinterface: HASIM_TINY_CACHE


// ===================================================================
//
// PRIVATE DATA STRUCTURES
//
// ===================================================================

typedef UInt#(TLog#(nEntries))
    HASIM_TINY_CACHE_IDX#(numeric type nEntries);

typedef Vector#(nEntries, HASIM_TINY_CACHE_IDX#(nEntries))
    HASIM_TINY_CACHE_LRU#(numeric type nEntries);

module [HASIM_MODULE] mkTinyCache
    // interface:
        (HASIM_TINY_CACHE#(t_CACHE_ADDR, t_CACHE_DATA, nEntries))
    provisos (Eq#(t_CACHE_ADDR),
              Bits#(t_CACHE_DATA, t_CACHE_DATA_SZ),
              Bits#(Maybe#(t_CACHE_ADDR), t_CACHE_ADDR_SZ),
              Log#(nEntries, TLog#(nEntries)),

              Alias#(HASIM_TINY_CACHE_IDX#(nEntries), t_IDX),
              Alias#(HASIM_TINY_CACHE_LRU#(nEntries), t_LRU));

    Reg#(Vector#(nEntries, Maybe#(t_CACHE_ADDR))) cacheTag <- mkReg(Vector::replicate(tagged Invalid));
    Reg#(Vector#(nEntries, t_CACHE_DATA)) cacheData <- mkRegU();
    Reg#(t_LRU) cacheLRU <- mkReg(Vector::genWith(fromInteger));

    // Used during invalAll() processing
    Reg#(t_IDX) invalIdx <- mkReg(0);

    //
    // inval_all
    //
    rule inval_all (invalIdx != 0);
        cacheTag[invalIdx] <= tagged Invalid;
        invalIdx <= invalIdx + 1;
    endrule


    // ***** LRU Management *****

    //
    // getLRU --
    //   Least recently used entry in the cache.
    //
    function t_IDX getLRU(t_LRU list);

        return list[valueOf(nEntries) - 1];

    endfunction


    //
    // pushMRU --
    //   Update MRU list, moving an entry to the head of the list.
    //
    //   We could optimize the LRU vector for a 2 entry cache by reducing
    //   the LRU to one bit, but that would save a single bit and the code
    //   here would be less general.
    //
    function t_LRU pushMRU(t_LRU curLRU, t_IDX mru);

        t_LRU new_list = curLRU;
    
        //
        // Find the new MRU value in the current list
        //
        if (findElem(mru, curLRU) matches tagged Valid .mru_pos)
        begin
            //
            // Shift older references out of the MRU slot
            //
            for (t_IDX w = 0; w < mru_pos; w = w + 1)
            begin
                new_list[w + 1] = curLRU[w];
            end

            // MRU is slot 0
            new_list[0] = mru;
        end

        return new_list;

    endfunction


    // Read a line, returns invalid if not found.
    method ActionValue#(Maybe#(t_CACHE_DATA)) read(t_CACHE_ADDR addr) if (invalIdx == 0);

        Bool hit = False;
        t_IDX hit_idx = 0;
        Maybe#(t_CACHE_DATA) result = tagged Invalid;

        for (Integer i = 0; i < valueOf(nEntries); i = i + 1)
        begin
            if (cacheTag[i] matches tagged Valid .tag &&& addr == tag)
            begin
                hit = True;
                hit_idx = fromInteger(i);
            end
        end
    
        if (hit)
        begin
            result = tagged Valid cacheData[hit_idx];
            cacheLRU <= pushMRU(cacheLRU, hit_idx);
        end

        return result;

    endmethod
    
    // Write
    method Action write(t_CACHE_ADDR addr, t_CACHE_DATA data) if (invalIdx == 0);

        let i = getLRU(cacheLRU);

        cacheTag[i] <= tagged Valid addr;
        cacheData[i] <= data;

        cacheLRU <= pushMRU(cacheLRU, i);

    endmethod

    // Invalidate entire cache
    method Action invalAll() if (invalIdx == 0);

        cacheTag[0] <= tagged Invalid;

        if (valueOf(nEntries) > 1)
            invalIdx <= 1;

    endmethod

endmodule
