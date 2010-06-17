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

// MULTIPLEXED_REG

// An efficient implementation for a multiplexed register. Max 5 read ports, 1 write port.

interface MULTIPLEXED_REG#(numeric type t_NUM_INSTANCES, type t_DATA);

    method Reg#(t_DATA) getReg(INSTANCE_ID#(t_NUM_INSTANCES) iid);

endinterface
                                                               
// A multiplexed register, implemented as a LUTRAM. We use some bluespec magic to make it
// look like a reg can be extracted, but really we're just accessing the RAM.

// NOTE: This version only has one write port, so if two different stages access the 
// write port then it will introduce a conflict. If this conflict is not desired, then
// use the more expensive version below.

module [m] mkMultiplexedReg#(t_DATA initval) 
    // interface:
        (MULTIPLEXED_REG#(t_NUM_INSTANCES, t_DATA))
    provisos 
        (Bits#(t_DATA, t_DATA_SZ),
         IsModule#(m, a));

    // The actual RAM to store all the values.
    LUTRAM#(INSTANCE_ID#(t_NUM_INSTANCES), t_DATA) regram <- mkLUTRAM(initval);
    
    method Reg#(t_DATA) getReg(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    
        // Some Bluespec trickery. Make a Reg interface which wraps the LUTRAM and
        // makes it look like a Reg. Keeping this as a method (as opposed to a subinterface
        // like a vector) means that we get the dynamic indexing of the LUTRAM.
        
        return interface Reg#(t_DATA);
                   method t_DATA _read() = regram.sub(iid);
                   method Action _write(t_DATA d) = regram.upd(iid, d);
               endinterface;

    endmethod

endmodule

// MULTIPLEXED_REG_MULTI_WRITE

// An abstraction of multiple write ports. More expensive than above, but fewer conflicts.

interface MULTIPLEXED_REG_MULTI_WRITE#(numeric type t_NUM_INSTANCES, numeric type t_NUM_PORTS, type t_DATA);

    method Reg#(t_DATA) getRegWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);

endinterface

// A multiplexed register, implemented as a vector. This version has unlimited
// read/write ports. We use some magic to ensure that writes are conflict-free
// thus two separate pipeline stages can write the register (for instance stage1
// may set it to Invalid, and stage3 may conditionally set it to Valid X) without
// introducing a conflict. 

// NOTE: This assumes that a given instance ID is not in the pipeline more than once.

module [m] mkMultiplexedRegMultiWrite#(t_DATA initval) 
    // interface:
        (MULTIPLEXED_REG_MULTI_WRITE#(t_NUM_INSTANCES, t_NUM_PORTS, t_DATA))
    provisos 
        (Bits#(t_DATA, t_DATA_SZ),
         IsModule#(m, a));

    // A register storing the vector of values.
    Reg#(Vector#(t_NUM_INSTANCES, t_DATA)) regvec <- mkReg(replicate(initval));
    
    // A group of wires to record all writes across all writeports.
    // Using wires ensure writes will be conflict-free.
    Vector#(t_NUM_PORTS, Vector#(t_NUM_INSTANCES, RWire#(t_DATA))) writeWires = newVector();

    for (Integer x = 0; x < valueof(t_NUM_PORTS); x = x + 1)
    begin
        writeWires[x] <- replicateM(mkRWire);
    end

    // Update each register. Favor larger-numbered write ports,
    // although really it's probably an error if two of the index are valid at the same time
    // across write ports.

    rule updateRegs (True);
        
        Vector#(t_NUM_INSTANCES, t_DATA) new_regs = regvec;

        for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
        begin
            
            Maybe#(t_DATA) mNewVal = tagged Invalid;
            
            for (Integer y = 0; y < valueof(t_NUM_PORTS); y = y + 1)
            begin
                if (writeWires[y][x].wget() matches tagged Valid .val)
                begin
                    mNewVal = tagged Valid val;
                end
            end
            
            if (mNewVal matches tagged Valid .new_val)
            begin
                new_regs[x] = new_val;
            end

        end
        
        regvec <= new_regs;
    
    endrule

    method Reg#(t_DATA) getRegWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);
    
        // Some Bluespec trickery. Make a Reg interface which wraps the Vector and RWires and
        // makes them look like a Reg. As long as different pipeline stages use different integer
        // indices, then the scheduler will not make them conflict.
        
        return interface Reg#(t_DATA);
                   method t_DATA _read() = regvec[iid];
                   method Action _write(t_DATA d) = writeWires[portnum][iid].wset(d);
               endinterface;

    endmethod

endmodule


// MULTIPLEXED_LUTRAM

// An abstraction of a multiplexed LUTRAM which is actually a single LUTRAM with a larger address space.

interface MULTIPLEXED_LUTRAM#(numeric type t_NUM_INSTANCES, type t_ADDR, type t_DATA);

    method LUTRAM#(t_ADDR, t_DATA) getRAM(INSTANCE_ID#(t_NUM_INSTANCES) iid);

endinterface


//
// mkMultiplexedLUTRAM --
//     Special case: efficient, implementation of a multiplexed LUTRAM that
//     merges all the virtual LUTRAMs into a single one.
//
//     NOTE:  The constructor function should NOT have its initial value
//            be a function of the index, since the index of the
//            instantiated LUTRAM is different.
//
module [m] mkMultiplexedLUTRAM#(t_DATA initVal)
    // Interface:
    (MULTIPLEXED_LUTRAM#(t_NUM_INSTANCES, t_INDEX, t_DATA))
    provisos (IsModule#(m, a),
              Bits#(t_DATA, t_DATA_SZ),
              Alias#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_INDEX), t_MERGED_IDX),
              Bounded#(t_MERGED_IDX),
              Bits#(t_MERGED_IDX, t_MERGED_IDX_SZ));

    // Make a dummy initalization function;
    function t_DATA initFunc(t_INDEX i);
    
        return initVal;
    
    endfunction

    let m <- mkMultiplexedLUTRAMInitializedWith(initFunc);
    return m;

endmodule


// mkMultiplexedLUTRAMInitializedWith --
//     Special case: merged LUTRAM where the LUTRAM's initial value is a function
//     of its index. We do this by transforming the initialization function.
//

module mkMultiplexedLUTRAMInitializedWith#(function t_DATA getInitVal(t_INDEX i))
    // Interface:
    (MULTIPLEXED_LUTRAM#(t_NUM_INSTANCES, t_INDEX, t_DATA))
    provisos (Bits#(t_DATA, t_DATA_SZ),
              Alias#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_INDEX), t_MERGED_IDX),
              Bounded#(t_MERGED_IDX),
              Bits#(t_MERGED_IDX, t_MERGED_IDX_SZ));

    // Create a new initialization function from the given function.
    // We do this just be dropping the extra indexing in the tuple,
    // then feeding this into the original function.
    function t_DATA new_getInitVal(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_INDEX) merged_idx);
        match {.iid, .idx} = merged_idx;
        return getInitVal(idx);
    endfunction

    // Allocate a single, merged, LUTRAM for all instances.
    LUTRAM#(t_MERGED_IDX, t_DATA) mergedData <- mkLUTRAMWith(new_getInitVal);

    method LUTRAM#(t_INDEX, t_DATA) getRAM(INSTANCE_ID#(t_NUM_INSTANCES) iid);

        // Some Bluespec trickery. Make a LUTRAM interface which wraps the larger address space 
        // LUTRAM and makes it look like a smaller one. Keeping this as a method (as opposed 
        // to a subinterface like a vector) means that we get the dynamic indexing of the LUTRAM.
        
        return interface LUTRAM#(t_INDEX, t_DATA);
                    method t_DATA sub(t_INDEX a) = mergedData.sub(tuple2(iid, a));
                    method Action upd(t_INDEX a, t_DATA d) = mergedData.upd(tuple2(iid, a), d);
                    method initialized = mergedData.initialized;
               endinterface;
    endmethod

endmodule

// MULTIPLEXED_LUTRAM_MULTI_WRITE

// An abstraction of LUTRAM with multiple write ports. More expensive than above, but fewer conflicts.

interface MULTIPLEXED_LUTRAM_MULTI_WRITE#(numeric type t_NUM_INSTANCES, numeric type t_NUM_PORTS, type t_ADDR, type t_DATA);

    method LUTRAM#(t_ADDR, t_DATA) getRAMWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);

endinterface

// A multiplexed LUTRAM, implemented as a vector of RAMS. This version has unlimited
// read/write ports. We use some magic to ensure that writes are conflict-free
// thus two separate pipeline stages can write the RAM (for instance stage1
// may set an address to Invalid, and stage3 may conditionally set it to Valid X) without
// introducing a conflict. The stages may even write the same address, since they are
// really writing different LUTRAMS in the vector.

// NOTE: This assumes that a given instance ID is not in the pipeline more than once.

module [m] mkMultiplexedLUTRAMMultiWrite#(t_DATA initval) 
    // interface:
        (MULTIPLEXED_LUTRAM_MULTI_WRITE#(t_NUM_INSTANCES, t_NUM_PORTS, t_ADDR, t_DATA))
    provisos 
        (Bits#(t_DATA, t_DATA_SZ),
         Bits#(t_ADDR, t_ADDR_SZ),
         Bounded#(t_ADDR),
         IsModule#(m, a));

    // The vector of LUTRAMs.
    Vector#(t_NUM_INSTANCES, LUTRAM#(t_ADDR, t_DATA)) ramvec <- replicateM(mkLUTRAM(initval));

    Bool ramsInitialized = True;    
    for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
    begin
        ramsInitialized = ramsInitialized && ramvec[x].initialized();
    end

    // A group of wires to record all writes across all writeports.
    // Using wires ensure writes will be conflict-free.
    Vector#(t_NUM_PORTS, Vector#(t_NUM_INSTANCES, RWire#(Tuple2#(t_ADDR, t_DATA)))) writeWires = newVector();

    for (Integer x = 0; x < valueof(t_NUM_PORTS); x = x + 1)
    begin
        writeWires[x] <- replicateM(mkRWire);
    end

    // Update each RAM. Favor larger-numbered write ports,
    // although really it's probably an error if two of the index are valid at the same time
    // across write ports.

    rule updateRAMs (ramsInitialized);
        
        for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
        begin
            
            Maybe#(Tuple2#(t_ADDR, t_DATA)) mNewVal = tagged Invalid;
            
            for (Integer y = 0; y < valueof(t_NUM_PORTS); y = y + 1)
            begin
                if (writeWires[y][x].wget() matches tagged Valid .tup)
                begin
                    mNewVal = tagged Valid tup;
                end
            end
            
            if (mNewVal matches tagged Valid {.addr, .val})
            begin
                ramvec[x].upd(addr, val);
            end

        end
    
    endrule

    method LUTRAM#(t_ADDR, t_DATA) getRAMWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);
    
        // Some Bluespec trickery. Make a LUTRAM interface which wraps the Vector and RWires and
        // makes them look like a RAM. As long as different pipeline stages use different integer
        // indices, then the scheduler will not make them conflict.
        
        return interface LUTRAM#(t_ADDR, t_DATA);
                   method t_DATA sub(t_ADDR a) = ramvec[iid].sub(a);
                   method Action upd(t_ADDR a, t_DATA d) = writeWires[portnum][iid].wset(tuple2(a, d));
                   method initialized = ramvec[iid].initialized;
               endinterface;

    endmethod

endmodule

// Generalized state pool, implemented as a fifo. A cheaper alternative to the Multi-Write structures above

interface MULTIPLEXED_STATE_POOL#(parameter numeric type t_NUM_INSTANCES, parameter type t_DATA);

    method Action insertState(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_DATA d);
    method ActionValue#(t_DATA) extractState(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    interface INSTANCE_CONTROL_IN#(t_NUM_INSTANCES) ctrl;

endinterface

module mkMultiplexedStatePool#(t_DATA initval) 
    // interface: 
        (MULTIPLEXED_STATE_POOL#(t_NUM_INSTANCES, t_DATA))
    provisos
        (Bits#(t_DATA, t_DATA_SZ));

    NumTypeParam#(TAdd#(t_NUM_INSTANCES, 1)) buffering = ?;
    FIFOF#(t_DATA) q <- mkSizedLUTRAMFIFOF(buffering);
    COUNTER#(TLog#(t_NUM_INSTANCES)) curIID <- mkLCounter(0);
    Reg#(Bool) initialized <- mkReg(False);
    Reg#(Bool) initializing <- mkReg(False);
    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) maxRunningInstance <- mkRegU();
    
    rule initialize (initializing && !initialized);

        q.enq(initval);
        
        if (curIID.value() == maxRunningInstance)
        begin
            initialized <= True;
            curIID.setC(0);
        end
        else
        begin
            curIID.up();
        end

    endrule
    
    method Action insertState(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_DATA d);
        q.enq(d);
    endmethod
    
    method ActionValue#(t_DATA) extractState(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        let t = q.first();
        q.deq();
        curIID.up();
        return t;
    endmethod

    interface INSTANCE_CONTROL_IN ctrl;

        method Bool empty() = initialized ? !q.notEmpty : True;
        method Bool balanced() = True;
        method Bool light() = False;

        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
            return (initialized && q.notEmpty()) ? tagged Valid curIID.value() : Invalid ;
        endmethod

        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);
            initializing <= True;
            maxRunningInstance <= iid;
        endmethod
    
    endinterface


endmodule

