import hasim_common::*;
import soft_connections::*;

// TODO: replicate the exact same messages as in the hybrid version

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
   
    // ----------- rules ------------

    // print pending message/event/stat, if any
    rule print_pending_req (pendingReq == True);
        $display("%d %d %d", pendingReqType, pendingMsgClass, pendingPayload);
        pendingReq <= False;
    endrule

    // ------------ methods ------------

    // print message with 1 payload
    method Action printMessage1P(Bit#(32) msgclass, Bit#(32) payload) if (pendingReq == False);
        $display("%d %d %d", `REQ_PRINT_MSG, msgclass, payload);
    endmethod

    // print message with 2 payloads
    method Action printMessage2P(Bit#(32) msgclass, Bit#(32) payload0, Bit#(32) payload1) if (pendingReq == False);
        $write("%d %d %d ", `REQ_PRINT_MSG, msgclass, payload0);

        // buffer second payload, we will send it next cycle
        pendingReqType  <= `REQ_PRINT_MSG;
        pendingMsgClass <= msgclass + 1;
        pendingPayload  <= payload1;
        pendingReq      <= True;
    endmethod

    // print event in 2 stages
    method Action   printEvent(Bit#(8) stringID, Bit#(64) modelcycle, Bit#(32) payload) if (pendingReq == False);
        $write("%d %d %d ", `REQ_PRINT_EVENT, stringID, modelcycle);

        // buffer second payload, we will send it next cycle
        pendingReqType  <= `REQ_PRINT_EVENT;
        pendingMsgClass <= `EVENT_STRING_DEC_NEWLINE;
        pendingPayload  <= payload;
        pendingReq      <= True;
    endmethod

    // print stat
    method Action printStat(Bit#(8) stringID, Bit#(32) value) if (pendingReq == False);
        $display("%d %d %d", `REQ_PRINT_STAT, stringID, value);
    endmethod

    // print assert
    method Action printAssertion(Bit#(8) stringID, Bit#(8) severity) if (pendingReq == False);
        $display("%d %d %d", `REQ_PRINT_ASSERT, stringID, severity);
    endmethod

endmodule

