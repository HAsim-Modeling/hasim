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

typedef Bit#(`HASIM_EVENTS_SIZE) EventParam;

interface EventRecorder;
  method Action recordEvent(Maybe#(EventParam) mdata);
endinterface

module [CONNECTED_MODULE] mkEventRecorder#(EVENTS_DICT_TYPE eventID)
    //interface:
                (EventRecorder);

    let m <- (`HASIM_EVENTS_ENABLED) ? mkEventRecorder_Enabled(eventID) : mkEventRecorder_Disabled(eventID);
    return m;

endmodule



typedef union tagged
{
  EVENTS_DICT_TYPE EVT_NoEvent;
  struct {EVENTS_DICT_TYPE event_id; EventParam event_data;} EVT_Event;

  void       EVT_Disable;
  void       EVT_Enable;
}
  EventData deriving (Eq, Bits);



module [CONNECTED_MODULE] mkEventRecorder_Enabled#(EVENTS_DICT_TYPE eventID)
    //interface:
                (EventRecorder);

  Connection_Chain#(EventData)  chain  <- mkConnection_Chain(`RINGID_EVENTS);
  
  Reg#(Bool)       enabled <- mkReg(True);
    
  rule process (True);
  
    EventData evt <- chain.receive_from_prev();

    case (evt) matches 
      tagged EVT_Disable:     enabled   <= False;
      tagged EVT_Enable:      enabled   <= True;
      default:                noAction;
    endcase

    chain.send_to_next(evt);
  endrule
  
  method Action recordEvent(Maybe#(EventParam) mdata);
  
    if (enabled)
      case (mdata) matches
        tagged Invalid:
        begin
          chain.send_to_next(tagged EVT_NoEvent eventID);
        end
        tagged Valid .data:
        begin
          chain.send_to_next(tagged EVT_Event {event_id: eventID, event_data: data});
        end
      endcase
  
  endmethod

endmodule

module [CONNECTED_MODULE] mkEventRecorder_Disabled#(EVENTS_DICT_TYPE eventID)
    //interface:
                (EventRecorder);

  Bit#(8) eventNum = zeroExtend(pack(eventID));

  Connection_Chain#(EventData) chain <- mkConnection_Chain(`RINGID_EVENTS);
  Reg#(EVENTS_DICT_TYPE)         stall <- mkReg(minBound);
 
  rule insert (stall == eventID);
  
    chain.send_to_next(tagged EVT_NoEvent eventID);
    stall <= unpack(pack(stall) + 1);
  
  endrule
  
  rule process (stall != eventID);
  
    EventData evt <- chain.receive_from_prev();
    chain.send_to_next(evt);

    stall <= unpack(pack(stall) + 1);

  endrule

  method Action recordEvent(Maybe#(EventParam) mdata);
    noAction;
  endmethod

endmodule

interface EVENT_RECORDER_MULTIPLEXED#(type ni);
  method Action recordEvent(INSTANCE_ID#(ni) iid, Maybe#(EventParam) mdata);
endinterface

module [CONNECTED_MODULE] mkEventRecorder_Multiplexed#(EVENTS_DICT_TYPE eventID)
    //interface:
                (EVENT_RECORDER_MULTIPLEXED#(ni));

  Bit#(8) eventNum = zeroExtend(pack(eventID));

  Connection_Chain#(EventData) chain <- mkConnection_Chain(`RINGID_EVENTS);
  Reg#(EVENTS_DICT_TYPE)       stall <- mkReg(minBound);
 
  rule insert (stall == eventID);
  
    chain.send_to_next(tagged EVT_NoEvent eventID);
    stall <= unpack(pack(stall) + 1);
  
  endrule
  
  rule process (stall != eventID);
  
    EventData evt <- chain.receive_from_prev();
    chain.send_to_next(evt);

    stall <= unpack(pack(stall) + 1);

  endrule

  method Action recordEvent(INSTANCE_ID#(ni) iid, Maybe#(EventParam) mdata);
    noAction;
  endmethod

endmodule
