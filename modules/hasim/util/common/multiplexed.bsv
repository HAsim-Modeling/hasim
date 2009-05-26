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

// MULTIPLEXED

// A MULTIPLEXED is a multiple-instance version of a model state element.
// For now this is just a typedef for a vector.

// The mkMultiplexed function is basically just a replication of the module.
// In the future we could make this a BSV typeclass so that we could do
// more intelligent things like turning registers into LUTRAM.

// Instantiation example:

// // Program counter: one per instance.
// MULTIPLEXED#(NUM_CPUS, Reg#(ADDRESS)) pcPool <- mkMultiplexed(mkReg(`STARTING_ADDR));

// Usage example:

// rule fetchPC (True);
//     // Get our local state based on the current instance id.
//     let pc = pcPool[CPU_ID];
//     // Fetch the current PC
//     imem.fetch(pc);
//     // Update the PC.
//     pc <= bpred.prediction(pc);

`include "asim/provides/fpga_components.bsh"

// INSTANCE_ID#(ni) is an instance ID to distinguish between ni different
// instances.
// TODO: is it okay to have n not be a power of 2?
typedef TLog#(ni) INSTANCE_ID_BITS#(type ni);
typedef Bit#(INSTANCE_ID_BITS#(ni)) INSTANCE_ID#(type ni);

typedef Vector#(ni, t) MULTIPLEXED#(type ni, parameter type t);

module [m] mkMultiplexed#(function m#(t) f)
    // Interface:
    (MULTIPLEXED#(ni, t))
    provisos (IsModule#(m, a));

    MULTIPLEXED#(ni, t) v = newVector();
    for (Integer x = 0; x < valueOf(ni); x = x + 1)
    begin
        v[x] <- f();
    end
    return v;

endmodule

//
// mkMultiplexedLUTRAM --
//     Special case: efficient, implementation of a multiplexed LUTRAM that
//     merges all the virtual LUTRAMs into a single one.
//
//     NOTE:  The constructor function should NOT have its initial value
//            be a function of the index, since the index of the
//            instantiated LUTRAM is different.
//
module [m] mkMultiplexedLUTRAM#(function m#(LUTRAM#(t_MERGED_IDX, t_DATA)) f)
    // Interface:
    (MULTIPLEXED#(ni, LUTRAM#(t_INDEX, t_DATA)))
    provisos (IsModule#(m, a),
              Alias#(Tuple2#(INSTANCE_ID#(ni), t_INDEX), t_MERGED_IDX));

    // Allocate a single, merged, LUTRAM for all instances.
    LUTRAM#(t_MERGED_IDX, t_DATA) mergedData <- f();

    MULTIPLEXED#(ni, LUTRAM#(t_INDEX, t_DATA)) v = newVector();
    for (Integer x = 0; x < valueOf(ni); x = x + 1)
    begin
        v[x] =
           (interface LUTRAM#(t_INDEX, t_DATA);
                method Action upd(t_INDEX addr, t_DATA val) =
                    mergedData.upd(tuple2(fromInteger(x), addr), val);
            
                method t_DATA sub(t_INDEX addr) =
                    mergedData.sub(tuple2(fromInteger(x), addr));
            endinterface);
    end
    return v;
endmodule
