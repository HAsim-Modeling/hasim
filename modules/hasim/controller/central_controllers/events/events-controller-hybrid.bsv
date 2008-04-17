import FIFO::*;
import Counter::*;
import Vector::*;

import hasim_modellib::*;
import soft_connections::*;
`include "asim/dict/RINGID.bsh"
`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS_HEARTBEAT.bsh"
`include "asim/dict/EVENTS.bsh"

`include "streams.bsh"
`include "rrr.bsh"
`include "asim/rrr/service_ids.bsh"

// EVENTS_CONTROLLER

// Abstracts communication from the main controller to the Event trackers
// which are distributed throughout the hardware model.

// When Events are enabled the main controller can use the getNextEvent() method
// to get the next event for recording, until noMoreEvents() is asserted.

interface EVENTS_CONTROLLER;

  method Action doCommand(EVENTS_CONTROLLER_COMMAND com);

endinterface

// EVENT_CONTROLLER_COMMAND

// The datatype of commands the EVENTS_CONTROLLER accepts

typedef enum
{
  EVENTS_Enable,
  EVENTS_Disable
}
  EVENTS_CONTROLLER_COMMAND
                deriving (Eq, Bits);

// EVC_STATE

// The internal state of the EventsController

typedef enum
{
  EVC_Initialize,
  EVC_Enabled,
  EVC_Enabling,
  EVC_Disabling,
  EVC_Idle
}
  EVC_STATE
            deriving 
                     (Eq, Bits);

// mkEventsController

// A module which uses RRR to communicate events to software.
// Also emits a "heartbeat" every 1000 model cycles.

module [Connected_Module] mkEventsController#(Connection_Send#(STREAMS_REQUEST) link_streams)
    //interface:
                (EVENTS_CONTROLLER);


  //***** State Elements *****  
  
  // Communication link to the rest of the Events
  Connection_Chain#(EventData) chain <- mkConnection_Chain(`RINGID_EVENTS);
    
  // Communication link to the Events RRR service
  Connection_Send#(RRR_Request) link_events <- mkConnection_Send("rrr_client_events");

  // The current Event ID we are expecting
  Reg#(Bit#(8))       cur <- mkReg(0);
  
  // The current FPGA cycle
  Reg#(Bit#(64))      fpga_cc <- mkReg(0);
  
  // Track our internal state
  Reg#(EVC_STATE)   state <- mkReg(EVC_Enabled);
  
  // Emit a heartbeat when this equals 1000
  Reg#(Bit#(10)) beat_ctr <- mkReg(0);
  Bool need_beat = beat_ctr == 10'd1000;
  
  // Internal tick counts
  Vector#(TExp#(`EVENTS_DICT_BITS), Counter#(32)) ticks <- replicateM(mkCounter(0));

  // ***** Rules *****
  
  // tick
  
  // Count the current FPGA cycle
  
  rule tick (True);
  
    fpga_cc <= fpga_cc + 1;
  
  endrule
  
  // processResp
  
  // Process the next response from an individual Event.
  // Most of these will just get placed into the output FIFO.
  
  rule processResp (state != EVC_Initialize && !need_beat);
  
    let et <- chain.receive_from_prev();
    
    case (et) matches
      tagged EVT_Event .evt:  //Event Data to pass along
      begin
        link_events.send(RRR_Request { serviceID   : `EVENTS_SERVICE_ID,
                                       param0      : 0, //unused for now.
                                       param1      : zeroExtend(pack(evt.event_id)),
                                       param2      : zeroExtend(pack(evt.event_data)),
                                       param3      : ticks[pack(evt.event_id)].value(),
                                       needResponse: False });

        ticks[pack(evt.event_id)].up();
        if ((pack(evt.event_id) == 0))
        begin
          beat_ctr <= beat_ctr + 1;
        end
      end
      tagged EVT_NoEvent .event_id:  //No event, just tick.
      begin
        ticks[pack(event_id)].up();
        if ((pack(event_id) == 0))
        begin
          beat_ctr <= beat_ctr + 1;
        end
      end
      default: noAction;
    endcase
     
  endrule
  
  // heartBeat

  // An occaisional heartbeat just to let the outside world know the hardware is alive.
  // Currently happens every 1000 model cycles.

  rule heartBeat (need_beat);

    beat_ctr <= 0;

    link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_HEARTBEAT,
                                        stringID: `STREAMS_HEARTBEAT_MSG,
                                        payload0: truncate(fpga_cc),
                                        payload1: truncate(ticks[0].value()) });

  endrule


  // ***** Methods *****
  
  // doCommand
  
  // The primary way that the outside world tells us what to do.
  
  method Action doCommand(EVENTS_CONTROLLER_COMMAND com) if (!(state == EVC_Enabling) || (state == EVC_Disabling));
    
    case (com)
      EVENTS_Enable:  chain.send_to_next(EVT_Enable);  //XXX More must be done to get all event recorders onto the same model CC.
      EVENTS_Disable: chain.send_to_next(EVT_Disable);
    endcase
    
  endmethod

endmodule
