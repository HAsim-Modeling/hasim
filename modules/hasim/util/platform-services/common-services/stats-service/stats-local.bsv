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

import FIFO::*;
import Counter::*;
import Vector::*;

`include "asim/provides/librl_bsv_base.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/stats_device.bsh"

//AWB Parameters
//name:                  default:
//STATS_ENABLED   True
//STATS_SIZE      32
`include "asim/dict/RINGID.bsh"
`include "asim/dict/STATS.bsh"

//
// Various statistics interfaces:
//

typedef Bit#(`STATS_SIZE) STAT_VALUE;

// Single statistic
interface STAT;
    method Action incr();

    // Be careful with this method.  Incrementing by values too close to
    // the `STATS_SIZE bit counter can cause data to be lost if the counter
    // rises faster than it can be dumped to the host.
    method Action incrBy(STAT_VALUE amount);
endinterface: STAT

// Vector of multiple instances of the same statistics ID
interface STAT_VECTOR#(type ni);
    method Action incr(Bit#(TLog#(ni)) iid);

    // Be careful with this method.  Incrementing by values too close to
    // the `STATS_SIZE bit counter can cause data to be lost if the counter
    // rises faster than it can be dumped to the host.
    method Action incrBy(Bit#(TLog#(ni)) iid, STAT_VALUE amount);
endinterface

//
// mkStatCounter --
//     Public module for the STAT single statistic interface.  Implement it
//     using the code for the vector interface.
//
module [Connected_Module] mkStatCounter#(STATS_DICT_TYPE statID)
    // interface:
    (STAT);

    Vector#(1, STATS_DICT_TYPE) id_vec = replicate(statID);
    STAT_VECTOR#(1) m <- mkStatCounter_Vector(id_vec);
    
    method Action incr() = m.incr(0);
    method Action incrBy(STAT_VALUE amount) = m.incrBy(0, amount);
endmodule

//
// mkStatCounterArrayElement --
//     Public module for the STAT single statistic interface.  In this case,
//     the software is expected to combine multiple counters sharing a
//     single statID into an array.  The arrayIdx values must be unique
//     for a given statID.
//
module [Connected_Module] mkStatCounterArrayElement#(STATS_DICT_TYPE statID,
                                                     STAT_VECTOR_INDEX arrayIdx)
    // interface:
    (STAT);

    Vector#(1, STATS_DICT_TYPE) id_vec = replicate(statID);
    STAT_VECTOR#(1) m <- mkStatCounterArray_Vector(id_vec, arrayIdx);
    
    method Action incr() = m.incr(0);
    method Action incrBy(STAT_VALUE amount) = m.incrBy(0, amount);
endmodule


//
// mkStatCounter_MultiEntry --
//     Public module for the STAT_VECTOR with multiple instances of a single
//     IDs interface.  This is most likely used to store separate counters
//     for the same statistic across multiple instances.
//
//     *** This method is the only way to instantiate multiple buckets ***
//     *** for a single statistic ID.                                  ***
//
module [Connected_Module] mkStatCounter_MultiEntry#(STATS_DICT_TYPE statID)
    // interface:
    (STAT_VECTOR#(n_STATS))
    provisos (Add#(TLog#(n_STATS), k, STAT_VECTOR_INDEX_SZ));

    Vector#(n_STATS, STATS_DICT_TYPE) statID_vec = replicate(statID);
    let m <- (`STATS_ENABLED) ? mkStatCounterVec_Enabled(statID_vec, STATS_VECTOR_INSTANCE) :
                                mkStatCounterVec_Disabled(statID_vec);
    return m;
endmodule


//
// mkStatCounter_Vector --
//     Public module for the STAT_VECTOR multiple instance IDs interface.
//
module [Connected_Module] mkStatCounter_Vector#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs)
    // interface:
    (STAT_VECTOR#(n_STATS))
    provisos (Add#(TLog#(n_STATS), k, STAT_VECTOR_INDEX_SZ));

    let m <- (`STATS_ENABLED) ? mkStatCounterVec_Enabled(myIDs, STATS_UNIQUE_INSTANCE) :
                                mkStatCounterVec_Disabled(myIDs);
    return m;
endmodule


//
// mkStatCounterArray_Vector --
//     Same as mkStatCounter_Vector, but multiple of these will be
//     created in the hardware sharing a dictionary ID with unique
//     arrayIdx values.  The software will combine dictionary IDs from
//     separate counters into arrays.
//
//     Don't be too confused by the two vectors here.  The "vector"
//     is the collection of multiple, independent dictionary IDs in
//     a single statistics ring stop.  The "array" is the combination of
//     counters from independent ring stops into a logical array in
//     software.
//
module [Connected_Module] mkStatCounterArray_Vector#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs,
                                                     STAT_VECTOR_INDEX arrayIdx)
    // interface:
    (STAT_VECTOR#(n_STATS))
    provisos (Add#(TLog#(n_STATS), k, STAT_VECTOR_INDEX_SZ));

    let m <- (`STATS_ENABLED) ? mkStatCounterVec_Enabled(myIDs, tagged STATS_BUILD_ARRAY arrayIdx) :
                                mkStatCounterVec_Disabled(myIDs);
    return m;
endmodule



// ========================================================================
//
// Implementation -- internal modules.
//
// ========================================================================

typedef union tagged
{
    void ST_GET_LENGTH;
    void ST_DUMP;
    void ST_TOGGLE;
    void ST_RESET;
    struct {STATS_DICT_TYPE statID; STAT_VECTOR_INDEX index; STAT_VALUE value;}  ST_VAL;
    struct {STATS_DICT_TYPE statID; STAT_VECTOR_INDEX length; Bool buildArray; }  ST_LENGTH;
}
STAT_DATA
    deriving (Eq, Bits);

typedef enum
{
    RECORDING, BUILD_ARRAY_LENGTH, FINISHING_LENGTH, DUMPING, FINISHING_DUMP, RESETING
}
STAT_STATE
    deriving (Eq, Bits);


//
// STAT_TYPE --
//    Statistics buckets are combined by the software in many ways...
typedef union tagged
{
    // Each statistic in the group is separate and there may be only one
    // instance of each ID in the entire system.
    void STATS_UNIQUE_INSTANCE;

    // A vector group of statistics all sharing the same ID, each having its own
    // index in the vector.
    void STATS_VECTOR_INSTANCE;

    // Allow multiple instances and build a vector.  Each instance in the group
    // is treated as an independent dictionary ID.
    STAT_VECTOR_INDEX STATS_BUILD_ARRAY;
}
STAT_TYPE
    deriving (Eq, Bits);


//
// mkStatCounterVec_Enabled --
//     Vector of individual statistics.  When singleID is true all entries share
//     the same ID.
//
module [Connected_Module] mkStatCounterVec_Enabled#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs,
                                                    STAT_TYPE statType)
    // interface:
    (STAT_VECTOR#(n_STATS))
    provisos
        (Add#(TLog#(n_STATS), k, STAT_VECTOR_INDEX_SZ));

    Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);

    Vector#(n_STATS, COUNTER#(`STATS_SIZE)) statPool <- replicateM(mkLCounter(0));

    Reg#(STAT_STATE) state <- mkReg(RECORDING);
    Reg#(Bool) enabled <- mkReg(True);

    Reg#(STAT_VECTOR_INDEX) curDumpIdx <- mkRegU();

    //
    // Compute the buckets vector index given the type of collector and the
    // buckets index in the group.
    //
    function STAT_VECTOR_INDEX statIdx(STAT_VECTOR_INDEX idx);
        case (statType) matches
            // Normal vector
            tagged STATS_VECTOR_INSTANCE:
                return idx;

            // Non-vector collectors
            tagged STATS_UNIQUE_INSTANCE:
                return 0;

            // Vector spead across multiple collectors.  The vector index is
            // the software-side index, not the local slot in the collection
            // of statistics buckets.
            tagged STATS_BUILD_ARRAY .v_idx:
                return v_idx;
        endcase
    endfunction


    //
    // dump --
    //     Done one entry in the statistics vector.
    //
    rule dump (state == DUMPING);
        chain.sendToNext(tagged ST_VAL { statID: myIDs[curDumpIdx],
                                         index: statIdx(curDumpIdx),
                                         value: statPool[curDumpIdx].value() });

        statPool[curDumpIdx].setC(0);

        if (curDumpIdx == fromInteger(valueOf(n_STATS) - 1))
            state <= FINISHING_DUMP;

        curDumpIdx <= curDumpIdx + 1;
    endrule


    //
    // finishDump --
    //     Done dumping all entries in the statistics vector.
    //
    rule finishDump (state == FINISHING_DUMP);
        chain.sendToNext(tagged ST_DUMP);
        state <= RECORDING;
    endrule


    //
    // finishLength --
    //     Done reporting the length of the vector.
    //
    rule finishGetLength (state == FINISHING_LENGTH);
        chain.sendToNext(tagged ST_GET_LENGTH);
        state <= RECORDING;
    endrule


    //
    // buildArrayLengths --
    //    This code is somewhat confusing because there are two vectors involved.
    //    The first is the vector here of unrelated statics collected together
    //    simply to save ring stops.  The second is the array the software
    //    side is expected to collect by combining unique indices from
    //    separate hardware counters into an array.
    //
    //    Here we step through each, individual, bucket and send the software-
    //    side array index for each statistic.  The software will determine
    //    appropriate array lengths.
    //
    Reg#(STAT_VECTOR_INDEX) buildArrayIdx <- mkReg(0);
    rule buildArrayLengths (state == BUILD_ARRAY_LENGTH);
        if (statType matches tagged STATS_BUILD_ARRAY .v_idx)
        begin
            chain.sendToNext(tagged ST_LENGTH { statID: myIDs[buildArrayIdx],
                                                length: v_idx + 1,
                                                buildArray: True });
        end

        if (buildArrayIdx == fromInteger(valueOf(n_STATS) - 1))
        begin
            buildArrayIdx <= 0;
            state <= FINISHING_LENGTH;
        end
        else
        begin
            buildArrayIdx <= buildArrayIdx + 1;
        end
    endrule


    //
    // receiveCmd --
    //     Receive a command on the statistics ring.
    //
    (* conservative_implicit_conditions *)
    rule receiveCmd (state == RECORDING);
        STAT_DATA st <- chain.recvFromPrev();

        case (st) matches 
            tagged ST_GET_LENGTH:
            begin
                //
                // Software assumes length 1 unless told otherwise.  Pass
                // a length message only for the various vector types.
                //
                case (statType) matches
                    // Non-vector collectors
                    tagged STATS_UNIQUE_INSTANCE:
                    begin
                        chain.sendToNext(st);
                    end

                    // Normal vector
                    tagged STATS_VECTOR_INSTANCE:
                    begin
                        chain.sendToNext(tagged ST_LENGTH { statID: myIDs[0],
                                                            length: fromInteger(valueOf(n_STATS)),
                                                            buildArray: False });
                        state <= FINISHING_LENGTH;
                    end

                    // Array spead across multiple collectors.
                    tagged STATS_BUILD_ARRAY .v_idx:
                    begin
                        state <= BUILD_ARRAY_LENGTH;
                    end
                endcase
            end

            tagged ST_DUMP:
            begin
                curDumpIdx <= 0;
                state <= DUMPING;
            end

            tagged ST_RESET: 
            begin
                chain.sendToNext(st);
                for (Integer s = 0; s < valueOf(n_STATS); s = s + 1)
                    statPool[s].setC(0);
            end

            tagged ST_TOGGLE: 
            begin
                chain.sendToNext(st);
                enabled <= !enabled;
            end

            default: chain.sendToNext(st);
        endcase
    endrule


    //
    // dumpPartial --
    //     Monitor counters and forward values to software before a counter
    //     overflows.
    //
    Reg#(STAT_VECTOR_INDEX) curDumpPartialIdx <- mkReg(0);

    (* descending_urgency = "receiveCmd, dumpPartial" *)
    rule dumpPartial (state == RECORDING);
        // Is the most significant bit set?
        if (msb(statPool[curDumpPartialIdx].value()) == 1)
        begin
            chain.sendToNext(tagged ST_VAL { statID: myIDs[curDumpPartialIdx],
                                             index: statIdx(curDumpPartialIdx),
                                             value: statPool[curDumpPartialIdx].value() });

            statPool[curDumpPartialIdx].setC(0);
        end

        if (curDumpPartialIdx == fromInteger(valueOf(n_STATS) - 1))
            curDumpPartialIdx <= 0;
        else
            curDumpPartialIdx <= curDumpPartialIdx + 1;
    endrule


    method Action incr(Bit#(TLog#(n_STATS)) idx);
        if (enabled)
        begin
            statPool[idx].up();
        end
    endmethod


    method Action incrBy(Bit#(TLog#(n_STATS)) idx, STAT_VALUE amount);
        if (enabled)
        begin
            statPool[idx].upBy(amount);
        end
    endmethod
endmodule


module [Connected_Module] mkStatCounterVec_Disabled#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs)
    // interface:
    (STAT_VECTOR#(n_STATS));

    method Action incr(Bit#(TLog#(n_STATS)) idx);
        noAction;
    endmethod

    method Action incrBy(Bit#(TLog#(n_STATS)) idx, STAT_VALUE amount);
        noAction;
    endmethod
endmodule
