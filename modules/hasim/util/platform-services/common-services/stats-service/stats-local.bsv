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

// Single statistic
interface STAT;
    method Action incr();

    // Be careful with this method.  Incrementing by values too close to
    // the `STATS_SIZE bit counter can cause data to be lost if the counter
    // rises faster than it can be dumped to the host.
    method Action incrBy(Bit#(`STATS_SIZE) amount);
endinterface: STAT

// Vector of multiple instances of the same statistics ID
interface STAT_VECTOR#(type ni);
    method Action incr(Bit#(TLog#(ni)) iid);

    // Be careful with this method.  Incrementing by values too close to
    // the `STATS_SIZE bit counter can cause data to be lost if the counter
    // rises faster than it can be dumped to the host.
    method Action incrBy(Bit#(TLog#(ni)) iid, Bit#(`STATS_SIZE) amount);
endinterface

//
// mkStatCounter --
//     Public module for the STAT single statistic interface.  Implement it
//     using the code for the vector interface.
//
module [Connected_Module] mkStatCounter#(STATS_DICT_TYPE statID)
    // interface:
    (STAT);

    STAT_VECTOR#(1) m <- mkStatCounter_MultiEntry(statID);
    
    method Action incr() = m.incr(0);
    method Action incrBy(Bit#(`STATS_SIZE) amount) = m.incrBy(0, amount);
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
    let m <- (`STATS_ENABLED) ? mkStatCounterVec_Enabled(statID_vec, True) :
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

    let m <- (`STATS_ENABLED) ? mkStatCounterVec_Enabled(myIDs, False) :
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
    struct {STATS_DICT_TYPE statID; STAT_VECTOR_INDEX length;}  ST_LENGTH;
}
STAT_DATA
    deriving (Eq, Bits);

typedef enum
{
    RECORDING, FINISHING_LENGTH, DUMPING, FINISHING_DUMP
}
STAT_STATE
    deriving (Eq, Bits);


//
// mkStatCounterVec_Enabled --
//     Vector of individual statistics.  When singleID is true all entries share
//     the same ID.
//
module [Connected_Module] mkStatCounterVec_Enabled#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs,
                                                    Bool singleID)
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
    // The statistic index sent to software should always be 1 unless this
    // instance is a multi-entry vector of multiple instances of the same ID.
    //
    function STAT_VECTOR_INDEX statIdx(STAT_VECTOR_INDEX idx) = singleID ? idx : 0;


    //
    // dump --
    //     Done one entry in the statistics vector.
    //
    rule dump (state == DUMPING);
        chain.send_to_next(tagged ST_VAL { statID: myIDs[curDumpIdx],
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
        chain.send_to_next(tagged ST_DUMP);
        state <= RECORDING;
    endrule


    //
    // finishLength --
    //     Done reporting the length of the vector.
    //
    rule finishGetLength (state == FINISHING_LENGTH);
        chain.send_to_next(tagged ST_GET_LENGTH);
        state <= RECORDING;
    endrule


    //
    // receiveCmd --
    //     Receive a command on the statistics ring.
    //
    (* conservative_implicit_conditions *)
    rule receiveCmd (state == RECORDING);
        STAT_DATA st <- chain.receive_from_prev();

        case (st) matches 
            tagged ST_GET_LENGTH:
            begin
                //
                // Software assumes length 1 unless told otherwise.  The only
                // case where the length may be greater than 1 is when there
                // is a single statistic ID in the vector.
                //
                if (singleID)
                begin
                    chain.send_to_next(tagged ST_LENGTH { statID: myIDs[0],
                                                          length: fromInteger(valueOf(n_STATS)) });
                    state <= FINISHING_LENGTH;
                end
                else
                begin
                    chain.send_to_next(st);
                end
            end

            tagged ST_DUMP:
            begin
                curDumpIdx <= 0;
                state <= DUMPING;
            end

            tagged ST_RESET: 
            begin
                chain.send_to_next(st);
                for (Integer s = 0; s < valueOf(n_STATS); s = s + 1)
                    statPool[s].setC(0);
            end

            tagged ST_TOGGLE: 
            begin
                chain.send_to_next(st);
                enabled <= !enabled;
            end

            default: chain.send_to_next(st);
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
            chain.send_to_next(tagged ST_VAL { statID: myIDs[curDumpPartialIdx],
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


    method Action incrBy(Bit#(TLog#(n_STATS)) idx, Bit#(`STATS_SIZE) amount);
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

    method Action incrBy(Bit#(TLog#(n_STATS)) idx, Bit#(`STATS_SIZE) amount);
        noAction;
    endmethod
endmodule
