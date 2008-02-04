import hasim_common::*;
import rrr::*;
import soft_connections::*;

`include "asim/rrr/rrr_service_ids.bsh"
`define SERVICE_ID  `STREAMS_SERVICE_ID

`define REQ_PRINT_MSG       0
`define REQ_PRINT_EVENT     1
`define REQ_PRINT_STAT      2
`define REQ_PRINT_ASSERT    3

`define EVENT_STRING_DEC_NEWLINE    5

interface Streams;
    method Action   printMessage1P(Bit#(32) msgclass, Bit#(32) payload);
    method Action   printMessage2P(Bit#(32) msgclass, Bit#(32) payload0, Bit#(32) payload1);
    method Action   printEvent(Bit#(8) stringID, Bit#(64) modelcycle, Bit#(32) payload);
    method Action   printStat(Bit#(8) stringID, Bit#(32) value);
    method Action   printAssertion(Bit#(8) stringID, Bit#(8) severity);
endinterface

module [HASim_Module] mkStreams(Streams);

    // ----------- state -----------
    Reg#(Bool)      pendingReq      <- mkReg(False);
    Reg#(Bit#(32))  pendingReqType  <- mkReg(0);
    Reg#(Bit#(32))  pendingMsgClass <- mkReg(0);
    Reg#(Bit#(32))  pendingPayload  <- mkReg(0);
   
    Connection_Send#(RRR_Request) link_rrr <- mkConnection_Send("rrr_terminal");

    // ----------- rules ------------

    // send pending message/event/stat, if any
    rule send_pending_req (pendingReq == True);
        RRR_Request req;
        req.serviceID       = `SERVICE_ID;
        req.param0          = pendingReqType;
        req.param1          = pendingMsgClass;
        req.param2          = pendingPayload;
        req.needResponse    = False;

        link_rrr.send(req);

        pendingReq <= False;
    endrule

    // ------------ methods ------------

    // print message with 1 payload
    method Action printMessage1P(Bit#(32) msgclass, Bit#(32) payload) if (pendingReq == False);
        RRR_Request req;
        req.serviceID       = `SERVICE_ID;
        req.param0          = `REQ_PRINT_MSG;
        req.param1          = msgclass;
        req.param2          = payload;
        req.needResponse    = False;

        link_rrr.send(req);
    endmethod

    // print message with 2 payloads
    method Action printMessage2P(Bit#(32) msgclass, Bit#(32) payload0, Bit#(32) payload1) if (pendingReq == False);
        RRR_Request req;
        req.serviceID       = `SERVICE_ID;
        req.param0          = `REQ_PRINT_MSG;
        req.param1          = msgclass;
        req.param2          = payload0;
        req.needResponse    = False;

        link_rrr.send(req);

        // buffer second payload, we will send it next cycle
        pendingReqType  <= `REQ_PRINT_MSG;
        pendingMsgClass <= msgclass + 1;
        pendingPayload  <= payload1;
        pendingReq      <= True;
    endmethod

    // print event in 2 stages
    method Action   printEvent(Bit#(8) stringID, Bit#(64) modelcycle, Bit#(32) payload) if (pendingReq == False);
        RRR_Request req;
        req.serviceID       = `SERVICE_ID;
        req.param0          = `REQ_PRINT_EVENT;
        req.param1          = zeroExtend(stringID);
        req.param2          = truncate(modelcycle);
        req.needResponse    = False;

        link_rrr.send(req);

        // buffer second payload, we will send it next cycle
        pendingReqType  <= `REQ_PRINT_EVENT;
        pendingMsgClass <= `EVENT_STRING_DEC_NEWLINE;
        pendingPayload  <= payload;
        pendingReq      <= True;
    endmethod

    // print stat
    method Action printStat(Bit#(8) stringID, Bit#(32) value) if (pendingReq == False);
        RRR_Request req;
        req.serviceID       = `SERVICE_ID;
        req.param0          = `REQ_PRINT_STAT;
        req.param1          = zeroExtend(stringID);
        req.param2          = value;
        req.needResponse    = False;

        link_rrr.send(req);
    endmethod

    // print assert
    method Action printAssertion(Bit#(8) stringID, Bit#(8) severity) if (pendingReq == False);

        RRR_Request req;
        req.serviceID       = `SERVICE_ID;
        req.param0          = `REQ_PRINT_ASSERT;
        req.param1          = zeroExtend(stringID);
        req.param2          = zeroExtend(severity);
        req.needResponse    = False;

        link_rrr.send(req);
    endmethod

endmodule

