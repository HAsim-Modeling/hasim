import hasim_common::*;
import rrr::*;
import soft_connections::*;

`define HWSTATE_IDLE        0
`define HWSTATE_RUNNING     1
`define HWSTATE_STOPPED     2

`define REQ_STATE_SYNC      0
`define REQ_PRINT_MSG       1
`define REQ_PRINT_EVENT     2
`define REQ_PRINT_STAT      3

`define EVENT_STRING_DEC_NEWLINE    5

`define POLL_INTERVAL       100000

//********* stub for Software Controller **********
interface SoftwareController;
    method Bit#(32) getSimState();
    method Action   printMessage1P(Bit#(32) msgclass, Bit#(32) payload);
    method Action   printMessage2P(Bit#(32) msgclass, Bit#(32) payload0, Bit#(32) payload1);
    method Action   printEvent(Bit#(8) stringID, Bit#(64) modelcycle, Bit#(32) payload);
    method Action   printStat(Bit#(8) stringID, Bit#(32) value);
endinterface

module [HASim_Module] mkSoftwareController(SoftwareController);

    /* ----------- state ----------- */
    Reg#(Bit#(32))  pollCounter     <- mkReg(0);
    Reg#(Bit#(32))  simState        <- mkReg(`HWSTATE_IDLE);
    Reg#(Bit#(8))   reqState        <- mkReg(0);
    Reg#(Bit#(32))  pendingReqType  <- mkReg(0);
    Reg#(Bit#(32))  pendingMsgClass <- mkReg(0);
    Reg#(Bit#(32))  pendingPayload  <- mkReg(0);
   
    Connection_Client#(RRR_Request, RRR_Response)   link_rrr_diov
        <- mkConnection_Client("rrr_controller_diov");

    Connection_Send#(RRR_Request)                   link_rrr_void
        <- mkConnection_Send("rrr_controller_void");

    /* ----------- rules ------------ */

    // sync state of simulator with software
    rule send_state_update_req (reqState == 0 && pollCounter == 0);
        RRR_Request req;
        req.serviceID  = `SID_SWCON_SERVICE;
        req.param0     = `REQ_STATE_SYNC;
        req.param1     = 0;
        req.param2     = 0;
        req.needResponse = True;

        link_rrr_diov.makeReq(req);
        reqState <= 1;
    endrule

    // wait for response for sync state request
    rule wait_for_state_resp (reqState == 1);
        RRR_Response resp = link_rrr_diov.getResp();
        link_rrr_diov.deq();

        simState <= resp;
        reqState <= 0;
    endrule

    // send pending message/event/stat, if any
    rule send_pending_req (reqState == 2);
        
        RRR_Request req;
        req.serviceID       = `SID_SWCON_SERVICE;
        req.param0          = pendingReqType;
        req.param1          = pendingMsgClass;
        req.param2          = pendingPayload;
        req.needResponse    = False;

        link_rrr_void.send(req);

        reqState <= 0;

    endrule

    // cycle poll counter
    rule cycle_Counter (True);
        if (pollCounter == `POLL_INTERVAL)
            pollCounter <= 0;
        else
            pollCounter <= pollCounter + 1;
    endrule

    /* ------------ methods ------------ */

    // return cached simulation state
    method Bit#(32) getSimState();
        return simState;
    endmethod

    // print message with 1 payload
    method Action printMessage1P(Bit#(32) msgclass, Bit#(32) payload) if (reqState == 0);
        RRR_Request req;
        req.serviceID       = `SID_SWCON_SERVICE;
        req.param0          = `REQ_PRINT_MSG;
        req.param1          = msgclass;
        req.param2          = payload;
        req.needResponse    = False;

        link_rrr_void.send(req);

        reqState <= 0;
    endmethod

    // print message with 2 payloads
    method Action printMessage2P(Bit#(32) msgclass, Bit#(32) payload0, Bit#(32) payload1) if (reqState == 0);
        RRR_Request req;
        req.serviceID       = `SID_SWCON_SERVICE;
        req.param0          = `REQ_PRINT_MSG;
        req.param1          = msgclass;
        req.param2          = payload0;
        req.needResponse    = False;

        link_rrr_void.send(req);

        // buffer second payload, we will send it next cycle
        pendingReqType  <= `REQ_PRINT_MSG;
        pendingMsgClass <= msgclass + 1;
        pendingPayload  <= payload1;
        reqState <= 2;
    endmethod

    // print event in 2 stages
    method Action   printEvent(Bit#(8) stringID, Bit#(64) modelcycle, Bit#(32) payload) if (reqState == 0);
        RRR_Request req;
        req.serviceID       = `SID_SWCON_SERVICE;
        req.param0          = `REQ_PRINT_EVENT;
        req.param1          = zeroExtend(stringID);
        req.param2          = truncate(modelcycle);
        req.needResponse    = False;

        link_rrr_void.send(req);

        // buffer second payload, we will send it next cycle
        pendingReqType  <= `REQ_PRINT_EVENT;
        pendingMsgClass <= `EVENT_STRING_DEC_NEWLINE;
        pendingPayload  <= payload;
        reqState <= 2;
    endmethod

    // print stat
    method Action printStat(Bit#(8) stringID, Bit#(32) value) if (reqState == 0);
        RRR_Request req;
        req.serviceID       = `SID_SWCON_SERVICE;
        req.param0          = `REQ_PRINT_STAT;
        req.param1          = zeroExtend(stringID);
        req.param2          = value;
        req.needResponse    = False;

        link_rrr_void.send(req);

        reqState <= 0;
    endmethod

endmodule

