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


// Local counter for noting passing cycles without sending a message
typedef Bit#(8) EVENT_CYCLE_COUNTER;

// Internal instance ID bucket
typedef Bit#(8) EVENT_INSTANCE_ID;

typedef union tagged
{
    // Initialization step tells the software how many instances to expect.
    struct { EVENTS_DICT_TYPE eventId;
             EVENT_INSTANCE_ID max_iid;  } EVT_Init;
    struct { EVENTS_DICT_TYPE eventId;
             EVENT_INSTANCE_ID iid;
             EVENT_CYCLE_COUNTER cycles; } EVT_NoteCycles;
    struct { EVENTS_DICT_TYPE eventId;
             EVENT_INSTANCE_ID iid;
             EVENT_PARAM eventData;
             EVENT_CYCLE_COUNTER cycles; } EVT_Event;

    Bool                                   EVT_Enable;
}
EVENT_DATA
    deriving (Eq, Bits);



// ========================================================================
//
// Single context event recorders.
//
// ========================================================================


interface EVENT_RECORDER;
  method Action recordEvent(Maybe#(EVENT_PARAM) mdata);
endinterface

//
// mkEventRecorder --
//     Either allocate a real recorder if events are enabled or, if disabled,
//     allocated a dummy instance.
//
module [CONNECTED_MODULE] mkEventRecorder#(EVENTS_DICT_TYPE eventID)
    //interface:
    (EVENT_RECORDER);

    let m <- (`HASIM_EVENTS_ENABLED) ?
        mkEventRecorder_Enabled(eventID) :
        mkEventRecorder_Disabled(eventID);
    return m;
endmodule


module [CONNECTED_MODULE] mkEventRecorder_Enabled#(EVENTS_DICT_TYPE eventID)
    //interface:
    (EVENT_RECORDER);

    Connection_Chain#(EVENT_DATA) chain <- mkConnection_Chain(`RINGID_EVENTS);
  
    Reg#(EVENT_CYCLE_COUNTER) cycles <- mkReg(0);
    Reg#(Bool) enabled <- mkReg(False);
    FIFO#(Maybe#(EVENT_PARAM)) newEventQ <- mkBypassFIFO();
    
    Reg#(Bool) initialized <- mkReg(False);

    rule process (True);
        EVENT_DATA evt <- chain.recvFromPrev();
        case (evt) matches 
            tagged EVT_Enable .en:  enabled   <= en;
            default:                noAction;
        endcase

        chain.sendToNext(evt);
    endrule


    rule doInit (! initialized);
        chain.sendToNext(tagged EVT_Init { eventId: eventID, max_iid: 0 });
        initialized <= True;
    endrule


    (* descending_urgency = "process, doInit, newEvent" *)
    rule newEvent (initialized);
        let evt = newEventQ.first();
        newEventQ.deq();

        case (evt) matches
            tagged Invalid:
            begin
                // Update the cycle count.  If it overflows then send the
                // overflow to the host.
                //
                // The host always adds 1 to the cycle count it receives.
                // This avoids an adder in the hardware.
                if (cycles == maxBound)
                begin
                    chain.sendToNext(tagged EVT_NoteCycles { eventId: eventID,
                                                             iid: 0,
                                                             cycles: maxBound });
                    cycles <= 0;
                end
                else
                begin
                    cycles <= cycles + 1;
                end
            end

            tagged Valid .data:
            begin
                chain.sendToNext(tagged EVT_Event { eventId: eventID,
                                                    iid: 0,
                                                    eventData: data,
                                                    cycles: cycles });
                cycles <= 0;
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

    method Action recordEvent(Maybe#(EVENT_PARAM) mdata);
        noAction;
    endmethod
endmodule



// ========================================================================
//
// Multiplexed event recorders.
//
// ========================================================================

interface EVENT_RECORDER_MULTIPLEXED#(type ni);
    method Action recordEvent(INSTANCE_ID#(ni) iid, Maybe#(EVENT_PARAM) mdata);
endinterface

module [CONNECTED_MODULE] mkEventRecorder_Multiplexed#(EVENTS_DICT_TYPE eventID)
    //interface:
    (EVENT_RECORDER_MULTIPLEXED#(ni));

    let m <- (`HASIM_EVENTS_ENABLED) ?
        mkEventRecorder_Multiplexed_Enabled(eventID) :
        mkEventRecorder_Multiplexed_Disabled(eventID);
    return m;
endmodule


//
// mkEventRecorder_Multiplexed_Enabled --
//      Almost the same as a non-multiplexed event recorder.  The only major
//      difference is the cycles bucket is a pool, indexed by instance ID.
//
module [CONNECTED_MODULE] mkEventRecorder_Multiplexed_Enabled#(EVENTS_DICT_TYPE eventID)
    //interface:
    (EVENT_RECORDER_MULTIPLEXED#(ni));

    // Does the instance ID fit in the event data structure?  This test lets
    // us avoid a proviso and use resize() below, which would hide truncation.
    if (valueOf(SizeOf#(EVENT_INSTANCE_ID)) < valueOf(SizeOf#(INSTANCE_ID#(ni))))
    begin
        error("EVENT_INSTANCE_ID too small for " + integerToString(valueOf(ni)) + " instances");
    end

    Connection_Chain#(EVENT_DATA) chain <- mkConnection_Chain(`RINGID_EVENTS);
  
    MULTIPLEXED_REG#(ni, EVENT_CYCLE_COUNTER) cyclesPool <- mkMultiplexedReg(0);
    Reg#(Bool) enabled <- mkReg(False);
    FIFO#(Tuple2#(INSTANCE_ID#(ni), Maybe#(EVENT_PARAM))) newEventQ <- mkBypassFIFO();
    
    Reg#(Bool) initialized <- mkReg(False);

    rule process (True);
        EVENT_DATA evt <- chain.recvFromPrev();
        case (evt) matches 
            tagged EVT_Enable .en:  enabled   <= en;
            default:                noAction;
        endcase

        chain.sendToNext(evt);
    endrule


    rule doInit (! initialized);
        EVENT_INSTANCE_ID max_iid = 0;
        if (valueOf(ni) != 0)
        begin
            max_iid = fromInteger(valueOf(TSub#(ni, 1)));
        end

        chain.sendToNext(tagged EVT_Init { eventId: eventID, max_iid: max_iid });
        initialized <= True;
    endrule


    (* descending_urgency = "process, doInit, newEvent" *)
    rule newEvent (initialized);
        match {.iid, .evt} = newEventQ.first();
        newEventQ.deq();

        Reg#(EVENT_CYCLE_COUNTER) cycles = cyclesPool.getReg(iid);

        case (evt) matches
            tagged Invalid:
            begin
                // Update the cycle count.  If it overflows then send the
                // overflow to the host.
                //
                // The host always adds 1 to the cycle count it receives.
                // This avoids an adder in the hardware.
                if (cycles == maxBound)
                begin
                    chain.sendToNext(tagged EVT_NoteCycles { eventId: eventID,
                                                             iid: resize(iid),
                                                             cycles: maxBound });
                    cycles <= 0;
                end
                else
                begin
                    cycles <= cycles + 1;
                end
            end

            tagged Valid .data:
            begin
                chain.sendToNext(tagged EVT_Event { eventId: eventID,
                                                    iid: resize(iid),
                                                    eventData: data,
                                                    cycles: cycles });
                cycles <= 0;
            end
        endcase
    endrule

    method Action recordEvent(INSTANCE_ID#(ni) iid, Maybe#(EVENT_PARAM) mdata);
        if (enabled)
        begin
            newEventQ.enq(tuple2(iid, mdata));
        end
    endmethod
endmodule


module [CONNECTED_MODULE] mkEventRecorder_Multiplexed_Disabled#(EVENTS_DICT_TYPE eventID)
    //interface:
    (EVENT_RECORDER_MULTIPLEXED#(ni));

    method Action recordEvent(INSTANCE_ID#(ni) iid, Maybe#(EVENT_PARAM) mdata);
        noAction;
    endmethod
endmodule
