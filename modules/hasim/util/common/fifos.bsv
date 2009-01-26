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

import RWire::*;
import FIFO::*;
import FIFOF::*;


// ========================================================================
//
// BypassFIFO --
//
//   A FIFO with latency zero. The data is buffered if no one is listening.
//   Then future requests are answered from the buffer.
//
// ========================================================================

module mkBypassFIFO
    // Interface:
    (FIFO#(a))
    provisos (Bits#(a,sa));

    RWire#(a) enqw <- mkRWire;
    RWire#(a) result <- mkRWire;
    RWire#(PrimUnit) deqw <- mkRWire;
    FIFOF#(a) the_fifof <- mkUGFIFOF;

    rule doResult;
        if (the_fifof.notEmpty)
            result.wset(the_fifof.first());
        else
        begin
            case (enqw.wget()) matches
            tagged Just .r:
                result.wset(r);
            tagged Nothing:
                noAction;
            endcase
        end
    endrule

    rule doUpdate_enq;
        case (enqw.wget()) matches
        tagged Just .r: 
            if (the_fifof.notEmpty || !isJust(deqw.wget))
                the_fifof.enq(r); 
        tagged Nothing:
            noAction;
        endcase
    endrule

    rule doUpdate_deq;
        if (isJust(deqw.wget) && the_fifof.notEmpty)
            the_fifof.deq();
    endrule

    method Action clear();
        the_fifof.clear();
    endmethod: clear

    method Action enq(val) if (the_fifof.notFull);
        enqw.wset(val);
    endmethod: enq

    method Action deq() if ((the_fifof.notEmpty || isJust (enqw.wget())));
        deqw.wset(?); // I hate '?'.
    endmethod: deq

    method first() if (isJust(result.wget));
        return unJust(result.wget);
    endmethod: first

endmodule


module mkBypassSizedFIFO#(Integer x)
    // Interface:
    (FIFO#(a))
    provisos (Bits#(a,sa));

    RWire#(a) enqw <- mkRWire;
    RWire#(a) result <- mkRWire;
    RWire#(PrimUnit) deqw <- mkRWire;
    FIFOF#(a) the_fifof <- mkUGSizedFIFOF(x);

    rule doResult;
        if (the_fifof.notEmpty)
            result.wset(the_fifof.first());
        else
        begin
            case (enqw.wget()) matches
            tagged Just .r:
                result.wset(r);
            tagged Nothing:
                noAction;
            endcase
        end
    endrule

    rule doUpdate_enq;
        case (enqw.wget()) matches
        tagged Just .r: 
            if (the_fifof.notEmpty || !isJust(deqw.wget))
                the_fifof.enq(r); 
        tagged Nothing:
            noAction;
        endcase
    endrule

    rule doUpdate_deq;
        if (isJust(deqw.wget) && the_fifof.notEmpty)
            the_fifof.deq();
    endrule

    method Action clear();
        the_fifof.clear();
    endmethod: clear

    method Action enq(val) if (the_fifof.notFull);
        enqw.wset(val);
    endmethod: enq

    method Action deq() if ((the_fifof.notEmpty || isJust (enqw.wget())));
        deqw.wset(?); // I hate '?'.
    endmethod: deq

    method first() if (isJust(result.wget));
        return unJust(result.wget);
    endmethod: first

endmodule


// ========================================================================
//
// SCOREBOARD_FIFO --
//
//   A FIFO where objects flow out in the order they are allocated but
//   the data associated with a FIFO entry may arrive both late and out
//   of order.  Instead of taking data as an argument, the enq() method
//   returns a SCOREBOARD_FIFO_ENTRY_ID.  The value of the entry must be
//   set using the setValue() method before the entry may be accessed
//   as it exits the FIFO.
//
// ========================================================================

typedef Bit#(TLog#(t_NUM_ENTRIES)) SCOREBOARD_FIFO_ENTRY_ID#(numeric type t_NUM_ENTRIES);

interface SCOREBOARD_FIFO#(numeric type t_NUM_ENTRIES, type t_DATA);
    method ActionValue#(SCOREBOARD_FIFO_ENTRY_ID#(t_NUM_ENTRIES)) enq();
    method Action setValue(SCOREBOARD_FIFO_ENTRY_ID#(t_NUM_ENTRIES) id, t_DATA data);
    method t_DATA first();
    method Action deq();
    method Bool notFull();
    method Bool notEmpty();
    
    // For debug output:
    method SCOREBOARD_FIFO_ENTRY_ID#(t_NUM_ENTRIES) deqEntryId();
endinterface

module mkScoreboardFIFO
    // Interface:
    (SCOREBOARD_FIFO#(t_NUM_ENTRIES, t_DATA))
    provisos(
        Bits#(t_DATA, t_DATA_SZ),
        Alias#(SCOREBOARD_FIFO_ENTRY_ID#(t_NUM_ENTRIES), t_SCOREBOARD_FIFO_ENTRY_ID));
    
    COUNTER#(TLog#(TAdd#(t_NUM_ENTRIES, 1))) nEntries <- mkLCounter(0);
    LUTRAM#(t_SCOREBOARD_FIFO_ENTRY_ID, t_DATA) values <- mkLUTRAMU();

    // Pointers to next enq and deq slots in the ring buffer
    Reg#(t_SCOREBOARD_FIFO_ENTRY_ID) nextEnq <- mkReg(0);
    Reg#(t_SCOREBOARD_FIFO_ENTRY_ID) nextDeq <- mkReg(0);

    // reqVec and readyVec are used to determine whether an entry's data is
    // ready.  When ready, the bits corresponding to an entry match.  Using
    // separate vectors for enq() and deq() avoids write contention.
    Reg#(Vector#(t_NUM_ENTRIES, Bool)) reqVec <- mkReg(replicate(False));
    Reg#(Vector#(t_NUM_ENTRIES, Bool)) readyVec <- mkReg(replicate(False));


    function isNotFull() = (nEntries.value() != fromInteger(valueOf(t_NUM_ENTRIES)));
    function isNotEmpty() = (nEntries.value() != 0);

    function Bool oldestIsReady();
        //
        // To be ready there must be an entry in the queue and the reqVec bit
        // must match the readyVec bit for the oldest entry.
        //
        Bit#(t_NUM_ENTRIES) r = pack(reqVec) ^ pack(readyVec);
        return isNotEmpty() && (r[nextDeq] == 0);
    endfunction


    method ActionValue#(t_SCOREBOARD_FIFO_ENTRY_ID) enq() if (isNotFull());
        nEntries.up();

        // Mark FIFO slot as waiting for data
        let slot = nextEnq;
        reqVec[slot] <= ! reqVec[slot];
    
        // Update next slot pointer
        nextEnq <= slot + 1;

        return slot;
    endmethod

    method Action setValue(t_SCOREBOARD_FIFO_ENTRY_ID id, t_DATA data);
        // Write value to buffer
        values.upd(id, data);
        // Mark slot data ready
        readyVec[id] <= reqVec[id];
    endmethod

    method t_DATA first() if (oldestIsReady());
        return values.sub(nextDeq);
    endmethod

    method Action deq() if (oldestIsReady());
        // Pop oldest entry from FIFO
        nEntries.down();
        nextDeq <= nextDeq + 1;
    endmethod

    method Bool notFull();
        return isNotFull();
    endmethod

    method Bool notEmpty();
        return isNotEmpty();
    endmethod

    method SCOREBOARD_FIFO_ENTRY_ID#(t_NUM_ENTRIES) deqEntryId();
        return nextDeq;
    endmethod
endmodule
