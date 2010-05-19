import FIFO::*;
import Counter::*;
import Vector::*;

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/librl_bsv_base.bsh"

`include "asim/provides/low_level_platform_interface.bsh"
`include "asim/provides/rrr.bsh"

`include "asim/rrr/remote_client_stub_EVENTS.bsh"
`include "asim/rrr/remote_server_stub_EVENTS.bsh"

`include "asim/dict/RINGID.bsh"
`include "asim/dict/EVENTS.bsh"

// EVENTS_SERVICE

// Abstracts communication from the RRR service to the Event trackers
// which are distributed throughout the model.

// mkEventsService

// Use a ring to receive Events. Use RRR to communicate events to software.

module [CONNECTED_MODULE] mkEventsService
    // interface:
        ();

    // ***** State Elements *****  
    
    // Communication link to the rest of the Events
    Connection_Chain#(EVENT_DATA) chain <- mkConnection_Chain(`RINGID_EVENTS);
    
    // instantiate stubs
    ClientStub_EVENTS clientStub <- mkClientStub_EVENTS();
    ServerStub_EVENTS serverStub <- mkServerStub_EVENTS();

    // Track our internal state
    Reg#(Bool)   enabled <- mkReg(False);
  
    // Internal tick counts
    Vector#(TExp#(`EVENTS_DICT_BITS), COUNTER#(32)) ticks <- replicateM(mkLCounter(0));

    // ***** Rules *****
    
    // processResp
    
    // Process the next response from an individual Event.
    // Most of these will just get placed into the output FIFO.
    
    rule processResp (True);
        
        let et <- chain.recvFromPrev();
    
        case (et) matches
            tagged EVT_Event .evt:  //Event Data to pass along
            begin
                clientStub.makeRequest_LogEvent(zeroExtend(pack(evt.event_id)),
                                                zeroExtend(pack(evt.event_data)),
                                                ticks[pack(evt.event_id)].value());
                
                ticks[pack(evt.event_id)].up();
            end
            tagged EVT_NoEvent .event_id:  //No event, just tick.
            begin
                ticks[pack(event_id)].up();
            end
            default: noAction;
        endcase
        
    endrule
    
    rule toggleEvents (True);
    
        let req <- serverStub.acceptRequest_ToggleEvents();
        
        if (!enabled)
        begin
            chain.sendToNext(EVT_Enable);  //XXX More must be done to get all event recorders onto the same model CC.
            enabled <= True;
        end
        else
        begin
            chain.sendToNext(EVT_Disable);
            enabled <= False;
        end
        
    
    endrule
    
endmodule
