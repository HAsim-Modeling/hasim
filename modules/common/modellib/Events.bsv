//BSV library imports
import FIFO::*;
import ModuleCollect::*;
import soft_connections::*;
`include "asim/dict/RINGID.bsh"
`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS.bsh"

//AWB Parameters
//name:                  default:
//HASIM_EVENTS_ENABLED   True
//HASIM_EVENTS_SIZE      32

typedef Bit#(`HASIM_EVENTS_SIZE) EventParam;

interface EventRecorder;
  method Action recordEvent(Maybe#(EventParam) mdata);
endinterface

module [Connected_Module] mkEventRecorder#(STREAMS_DICT_TYPE eventID)
    //interface:
                (EventRecorder);

    let m <- (`HASIM_EVENTS_ENABLED) ? mkEventRecorder_Enabled(eventID) : mkEventRecorder_Disabled(eventID);
    return m;

endmodule



typedef union tagged
{
  STREAMS_DICT_TYPE EVT_NoEvent;
  struct {STREAMS_DICT_TYPE event_id; EventParam event_data;} EVT_Event;

  void       EVT_Disable;
  void       EVT_Enable;
}
  EventData deriving (Eq, Bits);



module [Connected_Module] mkEventRecorder_Enabled#(STREAMS_DICT_TYPE eventID)
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

module [Connected_Module] mkEventRecorder_Disabled#(STREAMS_DICT_TYPE eventID)
    //interface:
                (EventRecorder);

  Bit#(8) eventNum = zeroExtend(pack(eventID));

  Connection_Chain#(EventData) chain <- mkConnection_Chain(`RINGID_EVENTS);
  Reg#(STREAMS_DICT_TYPE)         stall <- mkReg(minBound);
 
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
