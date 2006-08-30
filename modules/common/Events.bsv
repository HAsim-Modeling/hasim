//BSV library imports
import FIFO::*;
import ModuleCollect::*;

//HASim library imports
import HASim::*;

/*

module [Connected_Module] mkEventRecorder#(String eventname)
    //interface:
                (EventRecorder);

    let m <- (`EVENTS_ENABLED) ? mkEventRecorder_Enabled(eventname) : mkEventRecorder_Disabled(eventname);
    return m;

endmodule

*/

typedef union tagged
{
  Bit#(32) EVT_Boundary;
  void EVT_NoEvent;
  Bit#(32) EVT_Event;
  void EVT_Toggle;
}
  EventData deriving (Eq, Bits);



module [Connected_Module] mkEventRecorder#(String eventname)
    //interface:
                (EventRecorder);

  FIFO#(EventData)  localQ <- mkFIFO();
  FIFO#(EventData)  chainQ <- mkFIFO();
  Reg#(Bool)         stall <- mkReg(False);
  Reg#(Bool)       enabled <- mkReg(True);
 
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
		  chainQ.enq(evt);
		  
		  case (evt) matches 
		    tagged EVT_Boundary .t: stall <= True;
		    tagged EVT_Toggle: enabled <= !enabled;
		    default: noAction;
		  endcase
		  
		endmethod

	     endinterface);
	     
  //Add our interface to the ModuleCollect collection
  addToCollection(LChain tuple2(0, chn));

  method Action recordEvent(Maybe#(Bit#(32)) mdata);
  
    if (enabled)
      case (mdata) matches
	tagged Invalid:
	begin
          localQ.enq(EVT_NoEvent);
          //$display("EVENT %s: No Event", eventname);
	end
	tagged Valid .data:
	begin
          localQ.enq(EVT_Event data);
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
  
    chainQ.enq(EVT_NoEvent);
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
  addToCollection(LChain tuple2(0, chn));

  method Action recordEvent(Maybe#(Bit#(32)) mdata);
    noAction;
  endmethod

endmodule

interface EventController;

  method Action toggle();

endinterface

module [Connected_Module] mkEventController_Software
    //interface:
                (EventController);

  FIFO#(EventData) chainQ <- mkFIFO();
  Reg#(Bit#(32))       cc <- mkReg(0);
  Reg#(Bit#(32))      cur <- mkReg(0);
  Reg#(Bool)      toggling <- mkReg(False);
  Reg#(Bool)      enabled <- mkReg(True);
    
  rule process (True);
  
    chainQ.deq();
    
    case (chainQ.first()) matches
      tagged EVT_Boundary .t:
      begin
	cur <= 0;
	$display("EVENT BEGIN CC [%d]", t);
      end
      tagged EVT_NoEvent:
      begin
	$display("EVENT %0d: ****", cur);
	cur <= cur + 1;
      end
      tagged EVT_Event .d:
      begin
        $display("EVENT %0d: 0x%h", cur, d);
	cur <= cur + 1;
      end
      tagged EVT_Toggle:
      begin
	$display("EVENT TOGGLE");
	cur <= 0;
	enabled <= !enabled;
      end
    endcase
     
  endrule
  
  let chn = (interface FIFO;
  
	        method CON_Data first() if (enabled || toggling) = marshall(toggling? EVT_Toggle : EVT_Boundary cc);
		method Action deq() if (enabled || toggling);
		  if (!toggling) cc <= cc + 1;
		  toggling <= False;
		endmethod
	        method Action enq(CON_Data x) = chainQ.enq(unmarshall(x));
		method Action clear() = noAction;

	     endinterface);

  //Add our interface to the ModuleCollect collection
  addToCollection(LChain tuple2(0, chn));

  method Action toggle();
  
    toggling <= True;
    
  endmethod

endmodule
