import FIFO::*;
import Counter::*;
import Vector::*;

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/librl_bsv_base.bsh"

`include "asim/provides/low_level_platform_interface.bsh"
`include "asim/provides/rrr.bsh"

`include "asim/rrr/remote_client_stub_EVENTS.bsh"
`include "asim/rrr/remote_server_stub_EVENTS.bsh"

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
    CONNECTION_CHAIN#(EVENT_DATA) chain <- mkConnectionChain("EVENTS");
    
    // instantiate stubs
    ClientStub_EVENTS clientStub <- mkClientStub_EVENTS();
    ServerStub_EVENTS serverStub <- mkServerStub_EVENTS();

    // ***** Rules *****
    
    // processResp
    
    // Process the next response from an individual Event.
    // Most of these will just get placed into the output FIFO.
    
    rule processResp (True);
        
        let et <- chain.recvFromPrev();
    
        case (et) matches
            tagged EVT_Init .evt:
            begin
                clientStub.makeRequest_LogInit(zeroExtend(pack(evt.eventId)),
                                               evt.max_iid);
            end

            tagged EVT_Event .evt:  //Event Data to pass along
            begin
                clientStub.makeRequest_LogEvent(zeroExtend(pack(evt.eventId)),
                                                evt.iid,
                                                zeroExtend(pack(evt.eventData)),
                                                evt.cycles);
            end

            tagged EVT_NoteCycles .evt:
            begin
                clientStub.makeRequest_LogCycles(zeroExtend(pack(evt.eventId)),
                                                 evt.iid,
                                                 evt.cycles);
            end

            default: noAction;
        endcase
        
    endrule
    
    rule enableEvents (True);
    
        let req <- serverStub.acceptRequest_EnableEvents();
        
        Bool want_enabled = (req[0] == 1);

        //XXX More must be done to get all event recorders onto the same model CC.
        chain.sendToNext(tagged EVT_Enable want_enabled);
    endrule
    
endmodule
