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

// MULTICTX

// A MULTICTX is a multiple-context version of a model state element.
// For now this is just a typedef for a vector.

// The mkMultiCtx function is basically just a replication of the module.
// In the future we could make this a BSV typeclass so that we could do
// more intelligent things like turning registers into LUTRAM.

// Instantiation example:

// // Program counter: one per context.
// MULTICTX#(Reg#(ADDRESS)) ctx_pc <- mkMultiCtx(mkReg(`STARTING_ADDR));

// Usage example:

// rule fetchPC (True);
//     // Get our local state based on the current context id.
//     let pc = ctx_pc[ctx_id];
//     // Fetch the current PC
//     imem.fetch(pc);
//     // Update the PC.
//     pc <= bpred.prediction(pc);


typedef Vector#(NUM_CONTEXTS, t) MULTICTX#(parameter type t);

module [m] mkMultiCtx#(function m#(t) f) (MULTICTX#(t)) provisos (IsModule#(m, a));

    // MULTICTX#(t) v <- Vector::replicateM(f);
    MULTICTX#(t) v = newVector();
    for (Integer x = 0; x < valueOf(NUM_CONTEXTS); x = x + 1)
    begin
        v[x] <- f();
    end
    return v;

endmodule
