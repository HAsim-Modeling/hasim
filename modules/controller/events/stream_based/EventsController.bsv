import FIFO::*;
import Counter::*;
import Vector::*;

import hasim_modellib::*;
import soft_connections::*;
`include "asim/dict/RINGID.bsh"
`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS.bsh"

`include "streams.bsh"

// EventsController

// Abstracts the communication between the main hardware controller and the 
// Event recorders spread throughout the hardware model.

// EVC_State

// The internal state of the EventsController

typedef enum
{
  EVC_Initialize,
  EVC_Enabled,
  EVC_Enabling,
  EVC_Disabling,
  EVC_Idle
}
  EVC_State
            deriving 
                     (Eq, Bits);

// mkEventsController

// A module which serially passes Events back to the main hardware controller.

module [Connected_Module] mkEventsController#(Connection_Send#(STREAMS_REQUEST) link_streams)
    //interface:
                (EventsController);


  //***** State Elements *****  
  
  // Communication link to the rest of the Events
  Connection_Chain#(EventData) chain <- mkConnection_Chain(`RINGID_EVENTS);
  
  // Output FIFO of events to pass along
  FIFO#(EventInfo) eventQ <- mkFIFO();
  
  // The current Event ID we are expecting
  Reg#(Bit#(8))       cur <- mkReg(0);
  
  // The current FPGA cycle
  Reg#(Bit#(64))      fpga_cc <- mkReg(0);
  
  // Track our internal state
  Reg#(EVC_State)   state <- mkReg(EVC_Enabled);
  
  // Emit a heartbeat when this equals 1000
  Reg#(Bit#(10)) beat_ctr <- mkReg(0);
  Bool need_beat = beat_ctr == 10'd1000;
  
  // Internal tick counts
  Vector#(TExp#(`STREAMS_DICT_BITS), Counter#(32)) ticks <- replicateM(mkCounter(0));

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
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_EVENT,
                                            stringID: evt.event_id,
                                            payload0: ticks[pack(evt.event_id)].value(),
                                            payload1: evt.event_data });

        ticks[pack(evt.event_id)].up();
        if ((pack(evt.event_id) == 2))
        begin
          beat_ctr <= beat_ctr + 1;
        end
      end
      tagged EVT_NoEvent .event_id:  //No event, just tick.
      begin
        ticks[pack(event_id)].up();
        if ((pack(event_id) == 2))
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
                                        payload1: truncate(ticks[2].value()) });

  endrule


  // ***** Methods *****
  
  // doCommand
  
  // The primary way that the outside world tells us what to do.
  
  method Action doCommand(EventsCommand com) if (!(state == EVC_Enabling) || (state == EVC_Disabling));
    
    case (com)
      Events_Enable:  chain.send_to_next(EVT_Enable);  //XXX More must be done to get all event recorders onto the same model CC.
      Events_Disable: chain.send_to_next(EVT_Disable);
    endcase
    
  endmethod

endmodule
