
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


// INSTANCE_ID#(ni) is an instance ID to distinguish between ni different
// instances.
// TODO: is it okay to have n not be a power of 2?
typedef TLog#(ni) INSTANCE_ID_BITS#(type ni);
typedef Bit#(INSTANCE_ID_BITS#(ni)) INSTANCE_ID#(type ni);

typedef Vector#(ni, t) MULTIPLEXED#(type ni, parameter type t);

module [m] mkMultiplexed#(function m#(t) f) (MULTIPLEXED#(ni, t)) provisos (IsModule#(m, a));

    MULTIPLEXED#(ni, t) v = newVector();
    for (Integer x = 0; x < valueOf(ni); x = x + 1)
    begin
        v[x] <- f();
    end
    return v;

endmodule
