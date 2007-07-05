//BSV library imports
import FIFO::*;
import ModuleCollect::*;

//AWB Parameters
//name:                  default:
//HASIM_EVENTS_ENABLED   True
//HASIM_EVENTS_LOGFILE   events_log.out
//HASIM_BMARK_TIMEOUT    100000

module [Connected_Module] mkEventRecorder#(String eventname)
    //interface:
                (EventRecorder);

    let m <- (`HASIM_EVENTS_ENABLED) ? mkEventRecorder_Enabled(eventname) : mkEventRecorder_Disabled(eventname);
    return m;

endmodule



typedef union tagged
{
  Bit#(32) EVT_Boundary;
  void EVT_NoEvent;
  Bit#(32) EVT_Event;
  File EVT_SetLog;
  void EVT_Disable;
  void EVT_Enable;
}
  EventData deriving (Eq, Bits);



module [Connected_Module] mkEventRecorder_Enabled#(String eventname)
    //interface:
                (EventRecorder);

  FIFO#(EventData)  localQ <- mkFIFO();
  FIFO#(EventData)  chainQ <- mkFIFO();
  Reg#(Bool)         stall <- mkReg(False);
  Reg#(Bool)       enabled <- mkReg(True);
  let event_log <- mkReg(InvalidFile);
  
  rule insert (stall);
  
    chainQ.enq(localQ.first());
    
    localQ.deq();
    stall <= False;
  
  endrule
      
  let chn = (interface FIFO;
  
	        method CON_Data first() = marshall(chainQ.first());
		method Action deq() = chainQ.deq();
		method Action clear() = noAction;
	        method Action enq(CON_Data x) if (!stall);
		
		  EventData evt = unmarshall(x);
		  
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
		  		  
		  chainQ.enq(evt);
		  
		endmethod

	     endinterface);
	     
  //Add our interface to the ModuleCollect collection
  addToCollection(tagged LChain tuple2(0, chn));

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

  FIFO#(EventData)  localQ <- mkFIFO();
  FIFO#(EventData)  chainQ <- mkFIFO();
  Reg#(Bool)         stall <- mkReg(False);
 
  rule insert (stall);
  
    chainQ.enq(tagged EVT_NoEvent);
    stall <= False;
  
  endrule
      
  let chn = (interface FIFO;
  
	        method CON_Data first() = marshall(chainQ.first());
		method Action deq() = chainQ.deq();
		method Action clear() = noAction;
	        method Action enq(CON_Data x) if (!stall);
		
		  EventData evt = unmarshall(x);
		  chainQ.enq(evt);
		  
		  case (evt) matches 
		    tagged EVT_Boundary .t: stall <= True;
		    default: noAction;
		  endcase
		  
		endmethod

	     endinterface);
	     
  //Add our interface to the ModuleCollect collection
  addToCollection(tagged LChain tuple2(0, chn));

  method Action recordEvent(Maybe#(Bit#(32)) mdata);
    noAction;
  endmethod

endmodule

interface EventController;

  method Action enableEvents();
  method Action disableEvents();

endinterface

typedef enum
{
  EVC_Initialize,
  EVC_PassLogFile,
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

  FIFO#(EventData) chainQ <- mkFIFO();
  Reg#(Bit#(32))       cc <- mkReg(0);
  Reg#(Bit#(32))      cur <- mkReg(0);
  Reg#(EVC_State)   state <- mkReg(EVC_Initialize);
  let           event_log <- mkReg(InvalidFile);
  
  rule timeout (cc == `HASIM_BMARK_TIMEOUT);
  
    $display("Event Controller: ERROR: Benchmark timed out after %s model cycles.", `HASIM_BMARK_TIMEOUT);
    $finish(1);
  
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
  
  
  rule process (state != EVC_Initialize);
  
    chainQ.deq();
    
    case (chainQ.first()) matches
      tagged EVT_Boundary .t:
      begin
	cur <= 0;
      end
      tagged EVT_NoEvent:
      begin
        //Record information here
	//$fdisplay(event_log, "EVENT %0d: ****", cur);
	cur <= cur + 1;
      end
      tagged EVT_Event .d:
      begin
        //Record information here
        //$fdisplay(event_log, "EVENT %0d: 0x%h", cur, d);
	cur <= cur + 1;
      end
      default: noAction;
    endcase
     
  endrule
  
  let canStart = !((state == EVC_Idle) || (state == EVC_Initialize));
  let nextCmd = case (state) matches
                  tagged EVC_Disabling:   tagged EVT_Disable;
		  tagged EVC_Enabling:    tagged EVT_Enable;
		  tagged EVC_Enabled:     tagged EVT_Boundary cc;
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
    
  let chn = (interface FIFO;
  
	        method CON_Data first() if (canStart) = marshall(nextCmd);
		method Action deq() if (canStart);
		  if (state == EVC_Enabled) 
		  begin
		    cc <= cc + 1;
		  end
		  state <= nextState;
		endmethod
	        method Action enq(CON_Data x) = chainQ.enq(unmarshall(x));
		method Action clear() = noAction;

	     endinterface);

  //Add our interface to the ModuleCollect collection
  addToCollection(tagged LChain tuple2(0, chn));

  method Action enableEvents();
    //XXX More must be done to get all event recorders onto the same model CC.
    state <= EVC_Enabling;
    
  endmethod

  method Action disableEvents();
  
    state <= EVC_Disabling;
    
  endmethod
  
endmodule
