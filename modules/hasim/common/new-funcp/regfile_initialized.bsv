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
// Initialized versions of mkRegFile and mkRegFileFull
//

module mkRegFileInitialized#(index_t lo_index,
                             index_t hi_index,
                             data_t init)
    (RegFile#(index_t, data_t))
    provisos(Bits#(data_t, data_SZ),
             Bits#(index_t, index_SZ),
             Bounded#(index_t),
             Eq#(index_t),
             Arith#(index_t));

    RegFile#(index_t, data_t) mem <- mkRegFile(lo_index, hi_index);

    //
    // Initialize storage
    //

    Reg#(Bool) initialized <- mkReg(False);
    Reg#(index_t) init_idx <- mkReg(lo_index);

    rule initializing (!initialized);
        mem.upd(init_idx, init);

        initialized <= (init_idx == hi_index);

        init_idx <= init_idx + 1;
    endrule

    //
    // Access methods
    //

    method Action upd(index_t addr, data_t d) if (initialized);
        mem.upd(addr, d);
    endmethod

    method data_t sub(index_t addr) if (initialized);
        return mem.sub(addr);
    endmethod

endmodule


module mkRegFileFullInitialized#(data_t init)
    (RegFile#(index_t, data_t))
    provisos(Bits#(data_t, data_SZ),
             Bits#(index_t, index_SZ),
             Bounded#(index_t),
             Eq#(index_t),
             Arith#(index_t));

    RegFile#(index_t, data_t) mem <- mkRegFileFull();

    //
    // Initialize storage
    //

    Reg#(Bool) initialized <- mkReg(False);
    Reg#(index_t) init_idx <- mkReg(minBound);

    rule initializing (!initialized);
        mem.upd(init_idx, init);

        initialized <= (init_idx == maxBound);

        init_idx <= init_idx + 1;
    endrule

    //
    // Access methods
    //

    method Action upd(index_t addr, data_t d) if (initialized);
        mem.upd(addr, d);
    endmethod

    method data_t sub(index_t addr) if (initialized);
        return mem.sub(addr);
    endmethod

endmodule
