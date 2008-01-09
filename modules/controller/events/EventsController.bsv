import FIFO::*;

import hasim_modellib::*;
import soft_connections::*;

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

module [Connected_Module] mkEventsController
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
  
  // ***** Rules *****
  
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
  
  // ***** Methods *****
  
  // doCommand
  
  // The primary way that the outside world tells us what to do.
  
  method Action doCommand(EventsCommand com) if (!(state == EVC_Enabling) || (state == EVC_Disabling));
    
    case (com)
      Events_Enable:  state <= EVC_Enabling;  //XXX More must be done to get all event recorders onto the same model CC.
      Events_Disable: state <= EVC_Disabling;
    endcase
    
  endmethod

  // getNextEvent
  
  // Return the next event, which will presumably get passed on to the event dumper.
  
  method ActionValue#(EventInfo) getNextEvent();
  
    let evt = eventQ.first();
    eventQ.deq();
    return evt;
    
  endmethod
  
  // noMoreEvents
  
  // Let the main controller know that no more events are coming.
  // XXX this is also too liberal. We should get all event recorders on the same model CC.
  
  method Bool noMoreEvents();

    return isBoundary;
  
  endmethod
  
endmodule
