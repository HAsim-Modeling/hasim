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

//
// Same as mkMultiplexedReg, but the storage is uninitialized.
//
module [m] mkMultiplexedRegU
    // interface:
        (MULTIPLEXED_REG#(t_NUM_INSTANCES, t_DATA))
    provisos 
        (Bits#(t_DATA, t_DATA_SZ),
         IsModule#(m, a));

    LUTRAM#(INSTANCE_ID#(t_NUM_INSTANCES), t_DATA) regram <- mkLUTRAMU();
    
    method Reg#(t_DATA) getReg(INSTANCE_ID#(t_NUM_INSTANCES) iid);
    
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


//
// mkMultiplexedRegPseudoMultiWrite --
//     Similar interface to the true multi-write multiplexed register, but much
//     less expensive for large arrays.  Introduces a read-bubble of multiple
//     writes arrive in the same cycle.
//
module [m] mkMultiplexedRegPseudoMultiWrite#(t_DATA initval) 
    // interface:
        (MULTIPLEXED_REG_MULTI_WRITE#(t_NUM_INSTANCES, t_NUM_PORTS, t_DATA))
    provisos 
        (Bits#(t_DATA, t_DATA_SZ),
         IsModule#(m, a));

    // A register storing the vector of values.
    MULTIPLEXED_REG#(t_NUM_INSTANCES, t_DATA) regvec <- mkMultiplexedReg(initval);
    
    MERGE_FIFOF#(t_NUM_PORTS, Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES),
                                      t_DATA)) writeQ <- mkMergeBypassFIFOF();

    // Block reads because writes are incomplete?  A BypassFIFO introduces a
    // conbinatorial path on the notEmpty() line, so we must be clever and
    // record whether any reads may potentially fire before the queued stores.
    Reg#(Bool) blockReads <- mkReg(False);
    PulseWire didWrite <- mkPulseWire();


    //
    // updateRegs --
    //     Process write requests.
    //
    rule updateRegs (True);
        match {.iid, .val} = writeQ.first();
        writeQ.deq();

        Reg#(t_DATA) r = regvec.getReg(iid);
        r <= val;

        // Any more writes in this group?  A bypass FIFO only has one storage
        // slot, so this is equivalent to testing notEmpty.
        blockReads <= ! writeQ.lastInGroup();
        didWrite.send();
    endrule


    //
    // noWrite --
    //     Update the blockReads state when no write is serviced.  This should
    //     not fire for typical implementations, since nothing should
    //     block a write.
    //
    (* fire_when_enabled *)
    (* no_implicit_conditions *)
    rule noWrite (! didWrite);
        blockReads <= writeQ.notEmpty();
    endrule


    method Reg#(t_DATA) getRegWithWritePort(INSTANCE_ID#(t_NUM_INSTANCES) iid, Integer portnum);
    
        // Some Bluespec trickery. Make a Reg interface which wraps the Vector and RWires and
        // makes them look like a Reg. As long as different pipeline stages use different integer
        // indices, then the scheduler will not make them conflict.
        
        return interface Reg#(t_DATA);
                   method t_DATA _read() if (! blockReads);
                       Reg#(t_DATA) r = regvec.getReg(iid);
                       return r;
                   endmethod

                   method Action _write(t_DATA d);
                       writeQ.ports[portnum].enq(tuple2(iid, d));
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

        method List#(PORT_INFO) portInfo() = List::nil;
    endinterface

endmodule
