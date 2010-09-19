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

`include "asim/provides/hasim_common.bsh"
`include "asim/dict/RINGID.bsh"

//
// mkStatCounter_Multiplexed --
//     Public module for the STAT_RECORDER_MULTIPLEXED multiple instances of
//     a single ID interface.
//


//
// Same interface as a STAT_VECTOR except the index size is guaranteed not
// to be zero bits.  This is needed because the CPU ID is also guaranteed
// to be zero bits, even when the max. number of CPUs is 1.
//
interface STAT_RECORDER_MULTIPLEXED#(type ni);
    method Action incr(Bit#(TMax#(TLog#(ni), 1)) iid);
    method Action incrBy(Bit#(TMax#(TLog#(ni), 1)) iid, STAT_VALUE amount);
endinterface


module [Connected_Module] mkStatCounter_Multiplexed#(STATS_DICT_TYPE myID)
    // interface:
    (STAT_RECORDER_MULTIPLEXED#(n_STATS))
    provisos
        (Alias#(Bit#(TMax#(TLog#(n_STATS), 1)), t_STAT_IDX),
         Add#(TLog#(n_STATS), k, STAT_VECTOR_INDEX_SZ));

    Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);

    MULTIPLEXED_REG#(n_STATS, STAT_VALUE) statPool <- mkMultiplexedReg(0);

    Reg#(STAT_STATE) state <- mkReg(RECORDING);
    Reg#(Bool) enabled <- mkReg(True);

    Reg#(INSTANCE_ID#(n_STATS)) curIdx <- mkRegU();
    
    Wire#(Tuple2#(t_STAT_IDX, STAT_VALUE)) incrW <- mkWire();

    //
    // dump --
    //     Done one entry in the statistics vector.
    //
    rule dump (state == DUMPING);
    
        Reg#(STAT_VALUE) stat = statPool.getReg(curIdx);

        chain.sendToNext(tagged ST_VAL { statID: myID,
                                         index: zeroExtendNP(curIdx),
                                         value: stat });

        stat <= 0;

        if (curIdx == fromInteger(valueOf(n_STATS) - 1))
            state <= FINISHING_DUMP;

        curIdx <= curIdx + 1;
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
    // resetStats --
    //     Reset one entry in the statistics vector.
    //
    rule resetStats (state == RESETING);
    
        Reg#(STAT_VALUE) stat = statPool.getReg(curIdx);

        stat <= 0;

        if (curIdx == fromInteger(valueOf(n_STATS) - 1))
            state <= RECORDING;

        curIdx <= curIdx + 1;
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
    // updateStat
    //     Increment a stat. Placed in a rule to make the scheduler's life easier.
    
    (* fire_when_enabled *)
    rule updateStat (state == RECORDING && enabled);
        match {.idx, .amount} = incrW;
        // Zero extend here only for a corner case when the max. number of
        // CPUs is 1.  In that case, INSTANCE_ID is forced to 1 bit, even
        // though the true STAT_IDX size is 0.
        Reg#(STAT_VALUE) stat = statPool.getReg(zeroExtend(idx));
        stat <= stat + amount;
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
                chain.sendToNext(tagged ST_LENGTH { statID: myID,
                                                    length: fromInteger(valueOf(n_STATS)),
                                                    buildArray: False });
                state <= FINISHING_LENGTH;
            end

            tagged ST_DUMP:
            begin
                curIdx <= 0;
                state <= DUMPING;
            end

            tagged ST_RESET: 
            begin
                chain.sendToNext(st);
                curIdx <= 0;
                state <= RESETING;
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
    Reg#(INSTANCE_ID#(n_STATS)) curDumpPartialIdx <- mkReg(0);

    (* descending_urgency = "updateStat, receiveCmd, dumpPartial" *)
    rule dumpPartial (state == RECORDING);
    
        Reg#(STAT_VALUE) stat = statPool.getReg(curDumpPartialIdx);

        // Is the most significant bit set?
        if (msb(stat) == 1)
        begin
            chain.sendToNext(tagged ST_VAL { statID: myID,
                                             index: zeroExtendNP(curDumpPartialIdx),
                                             value: stat });

            stat <= 0;
        end

        if (curDumpPartialIdx == fromInteger(valueOf(n_STATS) - 1))
            curDumpPartialIdx <= 0;
        else
            curDumpPartialIdx <= curDumpPartialIdx + 1;
    endrule


    method Action incr(t_STAT_IDX idx);
        incrW <= tuple2(idx, 1);
    endmethod


    method Action incrBy(t_STAT_IDX idx, STAT_VALUE amount);
        incrW <= tuple2(idx, amount);
    endmethod

endmodule
