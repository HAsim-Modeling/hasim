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

//AWB Parameters
//name:                  default:
//HASIM_STATS_ENABLED   True
//HASIM_STATS_SIZE      32
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
interface STAT_RECORDER_MULTIPLEXED#(type ni);
    method Action incr(INSTANCE_ID#(ni) iid);
    method Action decr(INSTANCE_ID#(ni) iid);
endinterface

// Vectors of multiple statistics IDs have an interface that is equivalent to
// multiplexed statistics with a single ID.  The difference is the argument
// to the module constructor.
typedef STAT_RECORDER_MULTIPLEXED#(n_STATS) STAT_VECTOR#(numeric type n_STATS);


typedef Bit#(`HASIM_STATS_SIZE) STAT_VALUE;


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
    (STAT_VECTOR#(n_STATS));

    let m <- (`HASIM_STATS_ENABLED) ? mkStatCounterVec_Enabled(myIDs) :
                                      mkStatCounterVec_Disabled(myIDs);
    return m;
endmodule


//
// mkStatCounter_Multiplexed --
//     Public module for the STAT_RECORDER_MULTIPLEXED multiple instances of
//     a single ID interface.
//
module [Connected_Module] mkStatCounter_Multiplexed#(STATS_DICT_TYPE myID)
    // interface:
    (STAT_RECORDER_MULTIPLEXED#(ni));

    Vector#(ni, STATS_DICT_TYPE) ids = replicate(myID);
    let m <- mkStatCounter_Vector(ids);
    return m;
endmodule


// ========================================================================
//
// Implementation -- internal modules.
//
// ========================================================================

typedef union tagged
{
    void ST_DUMP;
    void ST_ENABLE;
    void ST_DISABLE;
    struct {STATS_DICT_TYPE statID; STAT_VALUE value;}  ST_VAL;
    void ST_RESET;
}
STAT_DATA
    deriving (Eq, Bits);

typedef enum
{
    RECORDING, DUMPING, FINISHING_DUMP
}
STAT_STATE
    deriving (Eq, Bits);


//
// Vector of individual statistics.
//
module [Connected_Module] mkStatCounterVec_Enabled#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs)
    // interface:
    (STAT_VECTOR#(n_STATS));

    Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);

    Vector#(n_STATS, COUNTER#(`HASIM_STATS_SIZE)) statPool <- replicateM(mkLCounter(0));

    Reg#(STAT_STATE) state <- mkReg(RECORDING);
    Reg#(Bool) enabled <- mkReg(True);
    Reg#(Bit#(TLog#(n_STATS))) curDumpIdx <- mkRegU();


    //
    // dump --
    //     Done one entry in the statistics vector.
    //
    rule dump (state == DUMPING);
        chain.send_to_next(tagged ST_VAL { statID: myIDs[curDumpIdx],
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
    // receiveCmd --
    //     Receive a command on the statistics ring.
    //
    (* conservative_implicit_conditions *)
    rule receiveCmd (state == RECORDING);
        STAT_DATA st <- chain.receive_from_prev();

        case (st) matches 
            tagged ST_DUMP:
            begin
                //
                // Send the current value of the counter along the chain and reset
                // the counter.  If the run continues the software side will request
                // more stats dumps and compute the sum.
                //
                chain.send_to_next(tagged ST_VAL { statID: myIDs[0],
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

            tagged ST_DISABLE: 
            begin
                chain.send_to_next(st);
                enabled <= False;
            end

            tagged ST_ENABLE: 
            begin
                chain.send_to_next(st);
                enabled <= True;
            end

            default: chain.send_to_next(st);
        endcase
    endrule


    method Action incr(INSTANCE_ID#(n_STATS) idx);
        if (enabled)
        begin
            statPool[idx].up();
        end
    endmethod

    method Action decr(INSTANCE_ID#(n_STATS) idx);
        if (enabled)
            statPool[idx].down();
    endmethod
endmodule


module [Connected_Module] mkStatCounterVec_Disabled#(Vector#(n_STATS, STATS_DICT_TYPE) myIDs)
    // interface:
    (STAT_VECTOR#(n_STATS));

    method Action incr(INSTANCE_ID#(n_STATS) idx);
        noAction;
    endmethod

    method Action decr(INSTANCE_ID#(n_STATS) idx);
        noAction;
    endmethod
endmodule
