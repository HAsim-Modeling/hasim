import FIFO::*;

import hasim_modellib::*;
import soft_connections::*;

`include "streams.bsh"
`include "asim/dict/STREAMS.bsh"
`include "asim/dict/STREAMID.bsh"

// EventsController

// Abstracts the communication between the main hardware controller and the 
// Event recorders spread throughout the hardware model.

`define CHAIN_IDX_EVENTS 0

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
  
  
  //Communication link to the rest of the Events
  Connection_Chain#(EventData) chain <- mkConnection_Chain(`CHAIN_IDX_EVENTS);
  
  //Output FIFO of events to pass along
  FIFO#(EventInfo) eventQ <- mkFIFO();
  
  //The current Event ID we are expecting
  Reg#(Bit#(8))       cur <- mkReg(0);
  
  //Track the model cycle boundary
  Reg#(Bool) isBoundary <- mkReg(False);
  
  //Track our internal state
  Reg#(EVC_State)   state <- mkReg(EVC_Enabled);
  
  // The current FPGA clock cycle
  Reg#(Bit#(64)) curTick <- mkReg(minBound);

  // The current model clock cycle, according to the Event Controller
  Reg#(Bit#(64)) modelTick <- mkReg(0);
  
  // Every so often we emit a heartbeat just to let the outside world
  // know that the hardware is still alive.
  Reg#(Bit#(64)) beat <- mkReg(0);

  // ***** Rules *****
  
  // tick
  
  // Count the current FPGA cycle
  
  rule tick (True);
  
    curTick <= curTick + 1;
  
  endrule

  // sendReq
  
  // Send the next request to the Events.
  // Only do this if we are in a state which requires communication.
  
  rule sendReq (!((state == EVC_Idle) || (state == EVC_Initialize)));
  
    // The next command to send.
  
    let nextCmd = case (state) matches
                    tagged EVC_Disabling:   tagged EVT_Disable;
                    tagged EVC_Enabling:    tagged EVT_Enable;
                    tagged EVC_Enabled:     tagged EVT_Boundary 0;
                    default: ?;
                  endcase;

    // Our next state.

    let nextState = case (state) matches
                    tagged EVC_Disabling:   EVC_Idle;
                    tagged EVC_Enabling:    EVC_Enabled;
                    tagged EVC_Enabled:     EVC_Enabled;
                    default: ?;
                  endcase;

     state <= nextState;
     chain.send_to_next(nextCmd);

  endrule
  
  // processResp
  
  // Process the next response from an individual Event.
  // Most of these will just get placed into the output FIFO.
  
  rule processResp (state != EVC_Initialize);
  
    let evt <- chain.receive_from_prev();
    
    case (evt) matches
      tagged EVT_Event .d:  //Event Data to pass along
      begin
        eventQ.enq(EventInfo {eventBoundary: pack(isBoundary), eventStringID: cur, eventData: pack(d)});
        isBoundary <= False;
        cur <= cur + 1;
      end
      tagged EVT_NoEvent:  //This Event didn't occur this model cycle. For now this information is not passed along.
      begin
        cur <= cur + 1;
        isBoundary <= False;
      end
      tagged EVT_Boundary .t: //When our boundary gets back to us the model cycle is done.
      begin
        cur <= 0;
        isBoundary <= True;
      end
      default: noAction; //This should never happen
    endcase
     
  endrule
  
  // heartBeat

  // An occaisional heartbeat just to let the outside world know the hardware is alive.
  // Currently happens every 1000 model cycles.

  rule heartBeat (beat == 1000);

    link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_HEARTBEAT,
                                        stringID: `STREAMS_HEARTBEAT_MSG,
                                        payload0: truncate(curTick),
                                        payload1: truncate(modelTick()) });
    beat <= 0;

  endrule

  // printEvent
  
  // print the next event via Streams.
  
  rule printEvent(beat != 1000);
  
    let evt = eventQ.first();
    eventQ.deq();

    // TEMPORARY: manually translate stringID. We have to do this because
    // of the incremental way in which raw stringIDs are generated
    STREAMS_DICT_TYPE stringID = case (evt.eventStringID)
                                     0: `STREAMS_EVENT_FETCH;
                                     1: `STREAMS_EVENT_DECODE;
                                     2: `STREAMS_EVENT_EXECUTE;
                                     3: `STREAMS_EVENT_MEMORY;
                                     4: `STREAMS_EVENT_WRITEBACK;
                                  endcase;

    link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_EVENT,
                                        stringID: stringID,
                                        payload0: truncate(modelTick),
                                        payload1: pack(evt.eventData) });

    if (evt.eventBoundary == 1)
    begin
        modelTick <= modelTick + 1;
        beat <= beat + 1;
    end

  endrule

  // ***** Methods *****
  
  // doCommand
  
  // The primary way that the outside world tells us what to do.
  
  method Action doCommand(EventsCommand com) if (!(state == EVC_Enabling) || (state == EVC_Disabling));
    
    case (com)
      Events_Enable:  state <= EVC_Enabling;  //XXX More must be done to get all event recorders onto the same model CC.
      Events_Disable: state <= EVC_Disabling;
    endcase
    
  endmethod

  // noMoreEvents
  
  // Let the main controller know that no more events are coming.
  // XXX this is also too liberal. We should get all event recorders on the same model CC.
  
  method Bool noMoreEvents();

    return isBoundary;
  
  endmethod

endmodule
