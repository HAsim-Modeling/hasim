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
