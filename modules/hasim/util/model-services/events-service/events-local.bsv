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
// Tokens are the main way for HAsim to track data across simulator      
// partitions. The token type includes an index for token tables, epochs,
// and scratchpads which partitions can use as they see fit.             

// BSV library imports

import FIFO::*;
import SpecialFIFOs::*;

// Project imports

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/dict/RINGID.bsh"
`include "asim/dict/EVENTS.bsh"


//AWB Parameters
//name:                  default:
//HASIM_EVENTS_SIZE      32

//HASIM_EVENTS_ENABLED   comes from build environment (SCons script)

`ifndef HASIM_EVENTS_ENABLED
// This should only happen when looking for dependence in hasim-bsc-mkdepend.
// We should eventually fix it by teaching the build scrips to pass
// command line defines to the dependence processor.
`define HASIM_EVENTS_ENABLED False
`endif

typedef Bit#(`HASIM_EVENTS_SIZE) EVENT_PARAM;

interface EVENT_RECORDER;
  method Action recordEvent(Maybe#(EVENT_PARAM) mdata);
endinterface

module [CONNECTED_MODULE] mkEventRecorder#(EVENTS_DICT_TYPE eventID)
    //interface:
                (EVENT_RECORDER);

    let m <- (`HASIM_EVENTS_ENABLED) ? mkEventRecorder_Enabled(eventID) : mkEventRecorder_Disabled(eventID);
    return m;

endmodule



typedef union tagged
{
    EVENTS_DICT_TYPE EVT_NoEvent;
    struct {EVENTS_DICT_TYPE event_id; EVENT_PARAM event_data;} EVT_Event;

    void       EVT_Disable;
    void       EVT_Enable;
}
EVENT_DATA
    deriving (Eq, Bits);



module [CONNECTED_MODULE] mkEventRecorder_Enabled#(EVENTS_DICT_TYPE eventID)
    //interface:
    (EVENT_RECORDER);

    Connection_Chain#(EVENT_DATA) chain  <- mkConnection_Chain(`RINGID_EVENTS);
  
    Reg#(Bool) enabled <- mkReg(True);
    FIFO#(Maybe#(EVENT_PARAM)) newEventQ <- mkBypassFIFO();
    
    rule process (True);
        EVENT_DATA evt <- chain.recvFromPrev();
        case (evt) matches 
            tagged EVT_Disable:     enabled   <= False;
            tagged EVT_Enable:      enabled   <= True;
            default:                noAction;
        endcase

        chain.sendToNext(evt);
    endrule

    (* descending_urgency = "process, newEvent" *)
    rule newEvent (True);
        let evt = newEventQ.first();
        newEventQ.deq();

        case (evt) matches
            tagged Invalid:
            begin
                chain.sendToNext(tagged EVT_NoEvent eventID);
            end

            tagged Valid .data:
            begin
                chain.sendToNext(tagged EVT_Event {event_id: eventID, event_data: data});
            end
        endcase
    endrule

    method Action recordEvent(Maybe#(EVENT_PARAM) mdata);
        if (enabled)
        begin
            newEventQ.enq(mdata);
        end
    endmethod
endmodule


module [CONNECTED_MODULE] mkEventRecorder_Disabled#(EVENTS_DICT_TYPE eventID)
    //interface:
    (EVENT_RECORDER);

    Bit#(8) eventNum = zeroExtend(pack(eventID));

    Connection_Chain#(EVENT_DATA) chain <- mkConnection_Chain(`RINGID_EVENTS);
    Reg#(EVENTS_DICT_TYPE) stall <- mkReg(minBound);
 
    rule insert (stall == eventID);
        chain.sendToNext(tagged EVT_NoEvent eventID);
        stall <= unpack(pack(stall) + 1);
    endrule

    rule process (stall != eventID);
        EVENT_DATA evt <- chain.recvFromPrev();
        chain.sendToNext(evt);

        stall <= unpack(pack(stall) + 1);
    endrule

    method Action recordEvent(Maybe#(EVENT_PARAM) mdata);
        noAction;
    endmethod
endmodule


interface EVENT_RECORDER_MULTIPLEXED#(type ni);
    method Action recordEvent(INSTANCE_ID#(ni) iid, Maybe#(EVENT_PARAM) mdata);
endinterface


module [CONNECTED_MODULE] mkEventRecorder_Multiplexed#(EVENTS_DICT_TYPE eventID)
    //interface:
    (EVENT_RECORDER_MULTIPLEXED#(ni));

    Bit#(8) eventNum = zeroExtend(pack(eventID));

    Connection_Chain#(EVENT_DATA) chain <- mkConnection_Chain(`RINGID_EVENTS);
    Reg#(EVENTS_DICT_TYPE) stall <- mkReg(minBound);
 
    rule insert (stall == eventID);
        chain.sendToNext(tagged EVT_NoEvent eventID);
        stall <= unpack(pack(stall) + 1);
    endrule

    rule process (stall != eventID);
        EVENT_DATA evt <- chain.recvFromPrev();
        chain.sendToNext(evt);

        stall <= unpack(pack(stall) + 1);
    endrule

    method Action recordEvent(INSTANCE_ID#(ni) iid, Maybe#(EVENT_PARAM) mdata);
        noAction;
    endmethod
endmodule
