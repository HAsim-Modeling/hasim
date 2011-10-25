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
typedef TMax#(TLog#(ni), 1) INSTANCE_ID_BITS#(type ni);
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
         IsModule#(m, a),
         Alias#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), t_DATA), t_WRITE_MSG));

    // A register storing the vector of values.
    Reg#(Vector#(t_NUM_INSTANCES, t_DATA)) regvec <- mkReg(replicate(initval));
    
    // A group of wires to record all writes across all writeports.
    // Using wires ensure writes will be conflict-free.
    Vector#(t_NUM_PORTS, RWire#(t_WRITE_MSG)) writeWires <- replicateM(mkRWire);

    // Update each register. Favor smaller-numbered write ports, although really
    // it's probably an error if two of the index are valid at the same time
    // across write ports.

    (* fire_when_enabled *)
    (* no_implicit_conditions *)
    rule updateRegs (True);
        // iidMatch() will be used as a predicate to a find() to detect messages
        // bound for a specific instance ID.
        function Bool iidMatch(Integer tgtIID, RWire#(t_WRITE_MSG) msg);
            let m = msg.wget();
            return isValid(m) && (fromInteger(tgtIID) == tpl_1(validValue(m)));
        endfunction

        // updateElem() returns either the current register or, if a write exists,
        // the new register value.
        function t_DATA updateElem(Vector#(t_NUM_PORTS, RWire#(t_WRITE_MSG)) newWrites,
                                   Tuple2#(Integer, t_DATA) curValue);
            match {.iid, .cur_val} = curValue;
            if (find(iidMatch(iid), newWrites) matches tagged Valid {.msg})
            begin
                // There is a write for this IID
                return tpl_2(validValue(msg.wget()));
            end
            else
            begin
                // No write.  Return current value.
                return cur_val;
            end
        endfunction

        // Do the work
        regvec <= map(updateElem(writeWires), zip(genVector(), regvec));
    endrule

    method Reg#(t_DATA) getRegWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);
    
        // Some Bluespec trickery. Make a Reg interface which wraps the Vector and RWires and
        // makes them look like a Reg. As long as different pipeline stages use different integer
        // indices, then the scheduler will not make them conflict.
        
        return interface Reg#(t_DATA);
                   method t_DATA _read() = regvec[iid];
                   method Action _write(t_DATA d) = writeWires[portnum].wset(tuple2(iid, d));
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
                    method Action upd(t_INDEX a, t_DATA d)  = mergedData.upd(tuple2(iid, a), d);
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
         IsModule#(m, a),
         Alias#(Tuple3#(INSTANCE_ID#(t_NUM_INSTANCES), t_ADDR, t_DATA), t_WRITE_MSG));

    // The vector of LUTRAMs.
    Vector#(t_NUM_INSTANCES, LUTRAM#(t_ADDR, t_DATA)) ramvec <- replicateM(mkLUTRAMU());

    // A group of wires to record all writes across all writeports.
    // Using wires ensure writes will be conflict-free.
    Vector#(t_NUM_PORTS, RWire#(t_WRITE_MSG)) writeWires <- replicateM(mkRWire);

    // Update each RAM. Favor smaller-numbered write ports, although really
    // it's probably an error if two of the index are valid at the same time
    // across write ports.

    //
    // Initialize storage.  We can't use standard initialized LUTRAMs because
    // that would leave a race between the initialization loop and write
    // requests flowing to updateRAMs below.
    //

    Reg#(Bool) initialized_m <- mkReg(False);
    Reg#(t_ADDR) init_idx <- mkReg(minBound);

    rule initializing (! initialized_m);
        for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
        begin
            ramvec[x].upd(init_idx, initval);
        end

        // Hack to avoid needing Eq proviso for comparison
        t_ADDR max = maxBound;
        initialized_m <= (pack(init_idx) == pack(max));

        // Hack to avoid needing Arith proviso
        init_idx <= unpack(pack(init_idx) + 1);
    endrule

    (* fire_when_enabled *)
    (* no_implicit_conditions *)
    rule updateRAMs (initialized_m);
        // iidMatch() will be used as a predicate to a find() to detect messages
        // bound for a specific instance ID.
        function Bool iidMatch(Integer tgtIID, RWire#(t_WRITE_MSG) msg);
            let m = msg.wget();
            return isValid(m) && (fromInteger(tgtIID) == tpl_1(validValue(m)));
        endfunction

        for (Integer x = 0; x < valueof(t_NUM_INSTANCES); x = x + 1)
        begin
            // Is there a write for this instance ID?
            if (find(iidMatch(x), writeWires) matches tagged Valid {.msg})
            begin
                match {.iid, .addr, .val} = validValue(msg.wget());
                ramvec[x].upd(addr, val);
            end
        end
    endrule

    method LUTRAM#(t_ADDR, t_DATA) getRAMWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);

        // Some Bluespec trickery. Make a LUTRAM interface which wraps the Vector
        // and RWires and makes them look like a RAM. As long as different pipeline
        // stages use different integer indices, then the scheduler will not make
        // them conflict.

        return interface LUTRAM#(t_ADDR, t_DATA);
                   method t_DATA sub(t_ADDR a) if (initialized_m);
                       return ramvec[iid].sub(a);
                   endmethod

                   method Action upd(t_ADDR a, t_DATA d) if (initialized_m);
                       writeWires[portnum].wset(tuple3(iid, a, d));
                   endmethod
               endinterface;

    endmethod

endmodule


//
// mkMultiplexedLUTRAMPseudoMultiWrite --
//     Same interface and function as mkMultiplexedLUTRAMMultiWrite but
//     with a single actual write port for a more efficient implementation.
//     Here we trade performance for space.  If multiple write requests arrive
//     in a cycle new requests are blocked until all writes are completed.
//
module [m] mkMultiplexedLUTRAMPseudoMultiWrite#(t_DATA initval) 
    // interface:
    (MULTIPLEXED_LUTRAM_MULTI_WRITE#(t_NUM_INSTANCES, t_NUM_PORTS, t_ADDR, t_DATA))
    provisos 
        (Bits#(t_DATA, t_DATA_SZ),
         Bits#(t_ADDR, t_ADDR_SZ),
         Bounded#(t_ADDR),
         IsModule#(m, a),
         Alias#(Tuple3#(INSTANCE_ID#(t_NUM_INSTANCES), t_ADDR, t_DATA), t_WRITE_MSG));

    // The storage
    MULTIPLEXED_LUTRAM#(t_NUM_INSTANCES, t_ADDR, t_DATA) ram <- mkMultiplexedLUTRAM(initval);

    // Incoming write requests
    MERGE_FIFOF#(t_NUM_PORTS, Tuple3#(INSTANCE_ID#(t_NUM_INSTANCES),
                                      t_ADDR,
                                      t_DATA)) writeQ <- mkMergeBypassFIFOF();

    // Block reads because writes are incomplete?  A BypassFIFO introduces a
    // conbinatorial path on the notEmpty() line, so we must be clever and
    // record whether any reads may potentially fire before the queued stores.
    Reg#(Bool) blockReads <- mkReg(False);
    PulseWire didWrite <- mkPulseWire();

    //
    // updateRAMs --
    //     Consume write requests and commit to memory.
    //
    rule updateRAMs (True);
        match {.iid, .addr, .data} = writeQ.first();
        writeQ.deq();

        ram.getRAM(iid).upd(addr, data);

        // Any more writes in this group?  A bypass FIFO only has one storage
        // slot, so this is equivalent to testing notEmpty.
        blockReads <= ! writeQ.lastInGroup();
        didWrite.send();
    endrule

    //
    // noWrite --
    //     Update the blockReads state when no write is serviced.  This should
    //     not fire for typical LUTRAM implementations, since nothing should
    //     block a write.
    //
    (* fire_when_enabled *)
    (* no_implicit_conditions *)
    rule noWrite (! didWrite);
        blockReads <= writeQ.notEmpty();
    endrule


    method LUTRAM#(t_ADDR, t_DATA) getRAMWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);

        return interface LUTRAM#(t_ADDR, t_DATA);
                   method t_DATA sub(t_ADDR a) if (! blockReads);
                       return ram.getRAM(iid).sub(a);
                   endmethod

                   method Action upd(t_ADDR a, t_DATA d);
                       writeQ.ports[portnum].enq(tuple3(iid, a, d));
                   endmethod
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

    //
    // Use a similar heuristic to the buffering in A-Ports.  Use block RAM
    // for large buffers.
    //
    FIFOF#(t_DATA) q;
    Integer buffering = max(1, valueOf(t_NUM_INSTANCES));
    if (((buffering >= 128) && (buffering * valueOf(t_DATA_SZ) > 30000)) ||
        ((buffering >= 512) && (buffering * valueOf(t_DATA_SZ) > 15000)))
    begin
        // Large buffer.  Use block RAM.
        q <- mkSizedBRAMFIFOF(buffering);
    end
    else
    begin
        // Small buffer.  Use distributed memory.
        q <- mkSizedFIFOF(buffering);
    end

    COUNTER#(INSTANCE_ID_BITS#(t_NUM_INSTANCES)) curIID <- mkLCounter(0);
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
    
    method Action insertState(INSTANCE_ID#(t_NUM_INSTANCES) iid, t_DATA d) if (initialized);
        q.enq(d);
    endmethod
    
    method ActionValue#(t_DATA) extractState(INSTANCE_ID#(t_NUM_INSTANCES) iid) if (initialized);
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

