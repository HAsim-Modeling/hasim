//BSV library imports
import FIFO::*;
import ModuleCollect::*;
import soft_connections::*;

//AWB Parameters
//name:                  default:
//HASIM_EVENTS_ENABLED   True
//HASIM_EVENTS_LOGFILE   events_log.out
//HASIM_BMARK_TIMEOUT    100000

interface EventRecorder;
  method Action recordEvent(Maybe#(Bit#(32)) mdata);
endinterface

module [Connected_Module] mkEventRecorder#(String eventname)
    //interface:
                (EventRecorder);

    let m <- (`HASIM_EVENTS_ENABLED) ? mkEventRecorder_Enabled(eventname) : mkEventRecorder_Disabled(eventname);
    return m;

endmodule

`define CHAIN_IDX_EVENTS 0


typedef union tagged
{
  Bit#(32) EVT_Boundary;
  void EVT_NoEvent;
  Bit#(32) EVT_Event;
  File EVT_SetLog; //Simulation only
  void EVT_Disable;
  void EVT_Enable;
}
  EventData deriving (Eq, Bits);



module [Connected_Module] mkEventRecorder_Enabled#(String eventname)
    //interface:
                (EventRecorder);

  Connection_Chain#(EventData)  chain  <- mkConnection_Chain(`CHAIN_IDX_EVENTS);
  
  FIFO#(EventData)  localQ <- mkFIFO();
  Reg#(Bool)         stall <- mkReg(False);
  Reg#(Bool)       enabled <- mkReg(True);
  let event_log <- mkReg(InvalidFile);
  
  rule insert (stall);
  
    chain.send_to_next(localQ.first());
    
    localQ.deq();
    stall <= False;
  
  endrule
  
  rule process (!stall);
  
    EventData evt <- chain.receive_from_prev();

    case (evt) matches 
      tagged EVT_Boundary .t:
      begin
        stall <= True;
        case (localQ.first()) matches
          tagged EVT_NoEvent:    $fdisplay(event_log, "[%d]: %s:", t, eventname);
          tagged EVT_Event .et: $fdisplay(event_log, "[%d]: %s: %0d", t, eventname, et);
          default: noAction;
        endcase
      end
      tagged EVT_Disable: enabled <= False;
      tagged EVT_Enable:  enabled <= True;
      tagged EVT_SetLog .l:  event_log <= l;
      default: noAction;
    endcase

    chain.send_to_next(evt);
  endrule
  
  method Action recordEvent(Maybe#(Bit#(32)) mdata);
  
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

  method Action recordEvent(Maybe#(Bit#(32)) mdata);
    noAction;
  endmethod

endmodule

interface EventController;

  method Action enableEvents();
  method Action disableEvents();
  method ActionValue#(EventInfo) getNextEvent();

endinterface

typedef struct
{
        Bit#(1) eventBoundary; //Is it a model cycle boundary?
        Bit#(8)  eventStringID;
        Bit#(32) eventData;
}
  EventInfo deriving (Eq, Bits);



typedef enum
{
  EVC_Initialize,  //Simulation only
  EVC_PassLogFile, //Simulation only
  EVC_Enabled,
  EVC_Enabling,
  EVC_Disabling,
  EVC_Idle
}
  EVC_State
    deriving (Eq, Bits);

module [Connected_Module] mkEventController_Simulation
    //interface:
                (EventController);

  Connection_Chain#(EventData) chain <- mkConnection_Chain(`CHAIN_IDX_EVENTS);
  
  FIFO#(EventInfo)  eventQ <- mkFIFO();
  Reg#(Bit#(32))   fpga_cc <- mkReg(0);
  Reg#(Bit#(32))  nextBeat <- mkReg(1000);
  Reg#(Bit#(32))  model_cc <- mkReg(0);
  Reg#(Bit#(8))        cur <- mkReg(0);
  Reg#(EVC_State)    state <- mkReg(EVC_Initialize);
  let            event_log <- mkReg(InvalidFile);
  Reg#(Bool)    isBoundary <- mkReg(False);
  
  rule tick (True);
  
    fpga_cc <= fpga_cc + 1;
  
  endrule
  
  rule timeout (model_cc == `HASIM_BMARK_TIMEOUT);
  
    $display("[%0d] Event Controller: ERROR: Benchmark timed out after %0d model cycles.", fpga_cc, `HASIM_BMARK_TIMEOUT);
    $finish(1);
  
  endrule
  
  rule heartbeat ((model_cc == nextBeat) && (model_cc != 0));
  
    $display("[%0d] Event Controller: %0d Model cycles simulated.", fpga_cc, model_cc);
    $fflush();
    nextBeat <= nextBeat + 1000;
  
  endrule
  
  rule initialize (state == EVC_Initialize);
  
    let fd <- $fopen(`HASIM_EVENTS_LOGFILE, "w");
    
    if (fd == InvalidFile)
    begin
      $display("Event Controller: ERROR: Could not create file %s", `HASIM_EVENTS_LOGFILE);
      $finish(1);
    end
    
    event_log <= fd;
    state <= EVC_PassLogFile;
  
  endrule
  
  
  rule processResp (state != EVC_Initialize);
  
    let evt <- chain.receive_from_prev();

    case (evt) matches
      tagged EVT_Boundary .t:
      begin
        model_cc <= model_cc + 1;
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
        cur <= cur + 1;
        //eventQ.enq(EventInfo {eventBoundary: pack(isBoundary), eventStringID: cur, eventData: d});
        isBoundary <= False;
      end
      default: noAction;
    endcase
     
  endrule
  
  rule sendReq (!((state == EVC_Idle) || (state == EVC_Initialize)));
  
    let nextCmd = case (state) matches
                    tagged EVC_Disabling:   tagged EVT_Disable;
                    tagged EVC_Enabling:    tagged EVT_Enable;
                    tagged EVC_Enabled:     tagged EVT_Boundary model_cc;
                    tagged EVC_PassLogFile: tagged EVT_SetLog event_log;
                    default: ?;
                  endcase;

    let nextState = case (state) matches
                    tagged EVC_Disabling:   EVC_Idle;
                    tagged EVC_Enabling:    EVC_Enabled;
                    tagged EVC_Enabled:     EVC_Enabled;
                    tagged EVC_PassLogFile: EVC_Enabled;
                    default: ?;
                  endcase;
    
    chain.send_to_next(nextCmd);
    
    state <= nextState;

  endrule
  
  method Action enableEvents();
    //XXX More must be done to get all event recorders onto the same model CC.
    state <= EVC_Enabling;
    
  endmethod

  method Action disableEvents();
  
    state <= EVC_Disabling;
    
  endmethod
  
  method ActionValue#(EventInfo) getNextEvent();
  
    eventQ.deq();
    return eventQ.first();
    
  endmethod
  
endmodule

module [Connected_Module] mkEventController_Hybrid
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
        eventQ.enq(EventInfo {eventBoundary: pack(isBoundary), eventStringID: cur, eventData: d});
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
  
  method Action enableEvents();
    //XXX More must be done to get all event recorders onto the same model CC.
    state <= EVC_Enabling;
    
  endmethod

  method Action disableEvents();
  
    state <= EVC_Disabling;
    
  endmethod
  
  method ActionValue#(EventInfo) getNextEvent();
  
    eventQ.deq();
    return eventQ.first();
    
  endmethod
  
endmodule
