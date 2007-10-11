//BSV library imports
import FIFO::*;
import ModuleCollect::*;
import soft_connections::*;

//AWB Parameters
//name:                  default:
//HASIM_EVENTS_ENABLED   True
//HASIM_EVENTS_SIZE      32
`define CHAIN_IDX_EVENTS 0

typedef Bit#(`HASIM_EVENTS_SIZE) EventParam;

interface EventRecorder;
  method Action recordEvent(Maybe#(EventParam) mdata);
endinterface

module [Connected_Module] mkEventRecorder#(String eventname)
    //interface:
                (EventRecorder);

    let m <- (`HASIM_EVENTS_ENABLED) ? mkEventRecorder_Enabled(eventname) : mkEventRecorder_Disabled(eventname);
    return m;

endmodule



typedef union tagged
{
  Bit#(32)   EVT_Boundary;
  void       EVT_NoEvent;
  EventParam EVT_Event;
  void       EVT_Disable;
  void       EVT_Enable;
}
  EventData deriving (Eq, Bits);



module [Connected_Module] mkEventRecorder_Enabled#(String eventname)
    //interface:
                (EventRecorder);

  Connection_Chain#(EventData)  chain  <- mkConnection_Chain(`CHAIN_IDX_EVENTS);
  
  FIFO#(EventData)  localQ <- mkFIFO();
  Reg#(Bool)         stall <- mkReg(False);
  Reg#(Bool)       enabled <- mkReg(True);
  
  rule insert (stall);
  
    chain.send_to_next(localQ.first());
    
    localQ.deq();
    stall <= False;
  
  endrule
  
  rule process (!stall);
  
    EventData evt <- chain.receive_from_prev();

    case (evt) matches 
      tagged EVT_Boundary .t: stall     <= True;
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
          localQ.enq(tagged EVT_NoEvent);
          //$display("EVENT %s: No Event", eventname);
        end
        tagged Valid .data:
        begin
          localQ.enq(tagged EVT_Event data);
          //$display("EVENT %s: 0x%h", eventname, data);
        end
      endcase
  
  endmethod

endmodule


module [Connected_Module] mkEventRecorder_Disabled#(String eventname)
    //interface:
                (EventRecorder);

  Connection_Chain#(EventData) chain <- mkConnection_Chain(`CHAIN_IDX_EVENTS); 
  FIFO#(EventData)  localQ <- mkFIFO();
  Reg#(Bool)         stall <- mkReg(False);
 
  rule insert (stall);
  
    chain.send_to_next(tagged EVT_NoEvent);
    stall <= False;
  
  endrule
  
  rule process (!stall);
  
    EventData evt <- chain.receive_from_prev();
    chain.send_to_next(evt);

    case (evt) matches 
      tagged EVT_Boundary .t: stall <= True;
      default: noAction;
    endcase
                  
  endrule

  method Action recordEvent(Maybe#(EventParam) mdata);
    noAction;
  endmethod

endmodule

interface EventController;

  method Action                  enableEvents();
  method Action                  disableEvents();
  method ActionValue#(EventInfo) getNextEvent();
  method Bool                    noMoreEvents();

endinterface

typedef struct
{
        Bit#(1) eventBoundary; //Is it a model cycle boundary?
        Bit#(8)  eventStringID;
        Bit#(`HASIM_EVENTS_SIZE) eventData;
}
  EventInfo deriving (Eq, Bits);



typedef enum
{
  EVC_Initialize,
  EVC_Enabled,
  EVC_Enabling,
  EVC_Disabling,
  EVC_Idle
}
  EVC_State
    deriving (Eq, Bits);

module [Connected_Module] mkEventController
    //interface:
                (EventController);

  Connection_Chain#(EventData) chain <- mkConnection_Chain(`CHAIN_IDX_EVENTS);
  FIFO#(EventInfo) eventQ <- mkFIFO();
  Reg#(Bit#(8))       cur <- mkReg(0);
  Reg#(EVC_State)   state <- mkReg(EVC_Enabled);
  Reg#(Bool) isBoundary <- mkReg(False);
  
  
  rule processResp (state != EVC_Initialize);
  
    let evt <- chain.receive_from_prev();
    
    case (evt) matches
      tagged EVT_Boundary .t:
      begin
        cur <= 0;
        isBoundary <= True;
      end
      tagged EVT_NoEvent:
      begin
        cur <= cur + 1;
        isBoundary <= False;
      end
      tagged EVT_Event .d:
      begin
        eventQ.enq(EventInfo {eventBoundary: pack(isBoundary), eventStringID: cur, eventData: pack(d)});
        isBoundary <= False;
        cur <= cur + 1;
      end
      default: noAction;
    endcase
     
  endrule
  
  rule sendReq (!((state == EVC_Idle) || (state == EVC_Initialize)));
  
    let nextCmd = case (state) matches
                    tagged EVC_Disabling:   tagged EVT_Disable;
                    tagged EVC_Enabling:    tagged EVT_Enable;
                    tagged EVC_Enabled:     tagged EVT_Boundary 0;
                    default: ?;
                  endcase;

    let nextState = case (state) matches
                    tagged EVC_Disabling:   EVC_Idle;
                    tagged EVC_Enabling:    EVC_Enabled;
                    tagged EVC_Enabled:     EVC_Enabled;
                    default: ?;
                  endcase;

     state <= nextState;
     chain.send_to_next(nextCmd);

  endrule
  
  method Action enableEvents() if (state == EVC_Idle);
    //XXX More must be done to get all event recorders onto the same model CC.
    state <= EVC_Enabling;
    
  endmethod

  method Action disableEvents() if (state == EVC_Enabled);
  
    state <= EVC_Disabling;
    
  endmethod
  
  method ActionValue#(EventInfo) getNextEvent();
  
    eventQ.deq();
    return eventQ.first();
    
  endmethod
  
  method Bool noMoreEvents();
    //XXX this is also too liberal. We should get all event recorders on the same model CC.
    return isBoundary;
  
  endmethod
  
endmodule
