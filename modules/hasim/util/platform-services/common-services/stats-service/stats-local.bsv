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
    method Action decr();
endinterface: STAT

// Vector of multiple instances of the same statistics ID
interface STAT_VECTOR#(type ni);
    method Action incr(Bit#(TLog#(ni)) iid);
    method Action decr(Bit#(TLog#(ni)) iid);
endinterface

//
// mkStatCounter --
//     Public module for the STAT single statistic interface.  Implement it
//     using the code for the vector interface.
//
module [Connected_Module] mkStatCounter#(STATS_DICT_TYPE statID)
    // interface:
    (STAT);

    Vector#(1, STATS_DICT_TYPE) statID_vec = replicate(statID);
    let m <- mkStatCounter_Vector(statID_vec);
    
    method Action incr() = m.incr(0);
    method Action decr() = m.decr(0);
endmodule


//
// mkStatCounter_Vector --
//     Public module for the STAT_VECTOR multiple instance IDs interface.
//
module [Connected_Module] mkStatCounter_Vector#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs)
    // interface:
    (STAT_VECTOR#(n_STATS))
    provisos
        (Add#(TLog#(n_STATS), k, 8));

    let m <- (`STATS_ENABLED) ? mkStatCounterVec_Enabled(myIDs) :
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
    struct {STATS_DICT_TYPE statID; STAT_VECTOR_INDEX index;} ST_OVERFLOW;
    
}
STAT_DATA
    deriving (Eq, Bits);

typedef enum
{
    RECORDING, REPORTING_LENGTH, FINISHING_LENGTH, DUMPING, FINISHING_DUMP
}
STAT_STATE
    deriving (Eq, Bits);


//
// Vector of individual statistics.
//
module [Connected_Module] mkStatCounterVec_Enabled#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs)
    // interface:
    (STAT_VECTOR#(n_STATS))
    provisos
        (Add#(TLog#(n_STATS), k, 8));

    Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);

    Vector#(n_STATS, COUNTER#(`STATS_SIZE)) statPool <- replicateM(mkLCounter(0));
    Vector#(n_STATS, Reg#(Bool)) seenPool <- replicateM(mkReg(False));

    Reg#(STAT_STATE) state <- mkReg(RECORDING);
    Reg#(Bool) enabled <- mkReg(True);
    Reg#(STAT_VECTOR_INDEX) curDumpIdx <- mkRegU();


    //
    // dump --
    //     Done one entry in the statistics vector.
    //
    rule dump (state == DUMPING);

        if (seenPool[curDumpIdx])
        begin

            chain.send_to_next(tagged ST_VAL { statID: myIDs[curDumpIdx],
                                               index: curDumpIdx,
                                               value: statPool[curDumpIdx].value() });

            statPool[curDumpIdx].setC(0);

        end

        if (curDumpIdx == fromInteger(valueOf(n_STATS) - 1))
            state <= FINISHING_DUMP;
        curDumpIdx <= curDumpIdx + 1;

    endrule

    //
    // reportLength --
    //     Send 1 for the length of the next stat in the vector.
    //
    rule reportLength (state == REPORTING_LENGTH);

        chain.send_to_next(tagged ST_LENGTH { statID: myIDs[curDumpIdx],
                                              length: fromInteger(valueof(n_STATS)) });

        if (curDumpIdx == fromInteger(valueOf(n_STATS) - 1))
            state <= FINISHING_LENGTH;
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
                // Send length=1 for each stat in the vector.
                chain.send_to_next(tagged ST_LENGTH { statID: myIDs[0], length: 1});
                curDumpIdx <= 1;
                if (valueOf(n_STATS) > 1)
                    state <= REPORTING_LENGTH;
                else
                    state <= FINISHING_LENGTH;
            end

            tagged ST_DUMP:
            begin
                //
                // Send the current value of the counter along the chain and reset
                // the counter.  If the run continues the software side will request
                // more stats dumps and compute the sum.
                //
                // Note we always send index zero whether or not it was seen.
                chain.send_to_next(tagged ST_VAL { statID: myIDs[0],
                                                       index: 0,
                                                       value: statPool[0].value() });
                statPool[0].setC(0);

                curDumpIdx <= 1;
                if (valueOf(n_STATS) > 1)
                    state <= DUMPING;
                else
                    state <= FINISHING_DUMP;
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


    method Action incr(Bit#(TLog#(n_STATS)) idx);
        if (enabled)
        begin
            statPool[idx].up();
            seenPool[idx] <= True;
            if (statPool[idx].value() == ~0)
            begin
                chain.send_to_next(tagged ST_OVERFLOW {statID: myIDs[idx], index: zeroExtend(idx)});
            end
        end
    endmethod

    method Action decr(Bit#(TLog#(n_STATS)) idx);
        if (enabled)
        begin
            statPool[idx].down();
            seenPool[idx] <= True;
        end
    endmethod
endmodule


module [Connected_Module] mkStatCounterVec_Disabled#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs)
    // interface:
    (STAT_VECTOR#(n_STATS));

    method Action incr(Bit#(TLog#(n_STATS)) idx);
        noAction;
    endmethod

    method Action decr(Bit#(TLog#(n_STATS)) idx);
        noAction;
    endmethod
endmodule
