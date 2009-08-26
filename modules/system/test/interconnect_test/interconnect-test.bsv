//
// Copyright (C) 2008 Massachusetts Institute of Technology
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//


import FIFO::*;
import Counter::*;
import FIFOF::*;

// General environmental includes

`include "asim/provides/fpga_components.bsh"
`include "asim/provides/soft_connections.bsh"

// Virtual services

`include "asim/provides/platform_services.bsh"
`include "asim/provides/mem_services.bsh"
`include "asim/provides/common_services.bsh"

// Submodule includes

`include "asim/provides/physical_interconnect.bsh"

`include "asim/dict/STREAMID.bsh"
`include "asim/dict/STREAMS_ICTEST.bsh"
`include "asim/dict/STREAMS_MESSAGE.bsh"


module [CONNECTED_MODULE] mkSystem ();

    match {.ic_ctrl, .ic_ctrl_info} <- mkICTestController();
    match {.ic_1, .ic_1_info} <- mkICTestEntity1();
    match {.ic_2, .ic_2_info} <- mkICTestEntity2();
    match {.ic_3, .ic_3_info} <- mkICTestEntity3();
    
    let children12 = cons(ic_1,
                     cons(ic_2,
                     nil));
    
    STATION_INFO info12 <- initRoutingTable(cons(ic_1_info, cons(ic_2_info, nil)));
    PHYSICAL_STATION node12 <- mkPhysicalStation(children12, info12.routingTable);
    // printStationInfo(info12);

    STATION_INFO info3 <- initRoutingTable(cons(info12, cons(ic_3_info, nil)));
    PHYSICAL_STATION node3 <- mkPhysicalStation(cons(node12, cons(ic_3, nil)), info3.routingTable);
    // printStationInfo(info3);
    
    STATION_INFO infoR <- initRoutingTable(cons(ic_ctrl_info, cons(info3, nil)));
    PHYSICAL_STATION nodeR <- mkPhysicalStation(cons(ic_ctrl, cons(node3, nil)), infoR.routingTable);
    // printStationInfo(infoR);

    let rootWrap <- mkEmptyRoot(nodeR);

endmodule


typedef enum
{
    CTRL_ready,
    CTRL_seq_test,
    CTRL_multi_test,
    CTRL_traffic_test,
    CTRL_seq_multi_test,
    CTRL_multi_traffic_test,
    CTRL_seq_traffic_test,
    CTRL_seq_multi_traffic_test
}
    CONTROLLER_STATE
        deriving (Eq, Bits);


module [CONNECTED_MODULE] mkICTestController (Tuple2#(PHYSICAL_STATION, STATION_INFO));

    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");
    
    Reg#(CONTROLLER_STATE) state <- mkReg(CTRL_ready);
    
    Counter#(7) testFreq <- mkCounter(127);
    Counter#(7) curFreq <- mkCounter(127);
    Counter#(4) testIter <- mkCounter(15);
    
    FIFO#(Bit#(7)) seqQ <- mkSizedFIFO(16);
    FIFO#(Bit#(7)) multi1Q <- mkSizedFIFO(16);
    FIFO#(Bit#(7)) multi2Q <- mkSizedFIFO(16);
    FIFO#(Bit#(7)) multi3Q <- mkSizedFIFO(16); 
    FIFO#(Bit#(7)) traffic1Q <- mkSizedFIFO(16);
    FIFO#(Bit#(7)) traffic2Q <- mkSizedFIFO(16);
    FIFO#(Bit#(7)) traffic3Q <- mkSizedFIFO(16); 
    
    Reg#(Bool) waitForFinish <- mkReg(False);
    Reg#(Bool) seqDone <- mkReg(False);
    Reg#(Bool) multiDone <- mkReg(False);
    Reg#(Bool) trafficDone <- mkReg(False);
    Reg#(Bool) multi1Passed <- mkReg(False);
    Reg#(Bool) multi2Passed <- mkReg(False);
    Reg#(Bool) multi3Passed <- mkReg(False);

    // Note: "new" connections, not old connections.
    // For now we must marshall up all the info ourselves.
    // Later ModuleCollect/Context will do it for us.

    match {.linkTo1, .send1station, .send1info} <- mkTestConnectionSend("Cto1");
    match {.linkTo2, .send2station, .send2info} <- mkTestConnectionSend("Cto2");
    match {.linkTo3, .send3station, .send3info} <- mkTestConnectionSend("Cto3");
    
    match {.linkFrom1, .recv1station, .recv1info} <- mkTestConnectionRecv("1toC");
    match {.linkFrom2, .recv2station, .recv2info} <- mkTestConnectionRecv("2toC");
    match {.linkFrom3, .recv3station, .recv3info} <- mkTestConnectionRecv("3toC");

    let sends = cons(send1info,
                cons(send2info,
                cons(send3info,
                nil)));
                
    let recvs = cons(recv1info,
                cons(recv2info,
                cons(recv3info,
                nil)));

    let children = cons(recv1station,
                   cons(recv2station,
                   cons(recv3station,
                   cons(send1station,
                   cons(send2station,
                   cons(send3station,
                   nil))))));

    let station_info <- initRoutingTableLeaf(recvs, sends);
    let station <- mkPhysicalStation(children, station_info.routingTable);

    function Bool canProceed();
    
        case (state)
            CTRL_ready: return True;
            CTRL_seq_test: return seqDone;
            CTRL_multi_test: return multiDone;
            CTRL_traffic_test: return trafficDone;
            CTRL_seq_multi_test: return seqDone && multiDone;
            CTRL_multi_traffic_test: return multiDone && trafficDone;
            CTRL_seq_traffic_test: return seqDone && trafficDone;
            CTRL_seq_multi_traffic_test: return seqDone && multiDone && trafficDone;
        endcase
    
    endfunction
    

    rule count (curFreq.value() != testFreq.value() && !waitForFinish);
    
        curFreq.up();
    
    endrule

    rule initiate (curFreq.value() == testFreq.value() && !waitForFinish);
    
        if (state == CTRL_seq_test || 
            state == CTRL_seq_multi_test || 
            state == CTRL_seq_traffic_test ||
            state == CTRL_seq_multi_traffic_test)
        begin
            linkTo1.send(tagged TEST_seq curFreq.value());
            seqQ.enq(curFreq.value());
        end
        
        if (state == CTRL_multi_test ||
            state == CTRL_seq_multi_test ||
            state == CTRL_multi_traffic_test ||
            state == CTRL_seq_multi_traffic_test)
        begin
            linkTo2.send(tagged TEST_multi curFreq.value());
            multi3Q.enq(curFreq.value());
            multi2Q.enq(curFreq.value());
            multi1Q.enq(curFreq.value());
            multi1Passed <= False;
            multi2Passed <= False;
            multi3Passed <= False;
        end
        
        if (state == CTRL_traffic_test ||
            state == CTRL_seq_traffic_test ||
            state == CTRL_multi_traffic_test ||
            state == CTRL_seq_multi_traffic_test)
        begin
            linkTo3.send(tagged TEST_traffic curFreq.value());
            traffic3Q.enq(curFreq.value());
        end
        
        if (testFreq.value() == 1)
        begin
            waitForFinish <= True;
        end
        else
        begin
            testFreq.down();
            curFreq.setC(0);
        end
    
    endrule
    
    rule finishTest (waitForFinish && canProceed());

        waitForFinish <= False;
        seqDone <= False;
        multiDone <= False;
        trafficDone <= False;

        let newstate = unpack(pack(state) + 1);
        let str_id = case (newstate)
            CTRL_ready:                   `STREAMS_ICTEST_DONE;
            CTRL_seq_test:                `STREAMS_ICTEST_SEQ_BEGIN;
            CTRL_multi_test:              `STREAMS_ICTEST_MULTI_BEGIN;
            CTRL_traffic_test:            `STREAMS_ICTEST_TRAFFIC_BEGIN;
            CTRL_seq_multi_test:          `STREAMS_ICTEST_SEQ_MULTI_BEGIN;
            CTRL_multi_traffic_test:      `STREAMS_ICTEST_MULTI_TRAFFIC_BEGIN;
            CTRL_seq_traffic_test:        `STREAMS_ICTEST_SEQ_TRAFFIC_BEGIN;
            CTRL_seq_multi_traffic_test:  `STREAMS_ICTEST_SEQ_MULTI_TRAFFIC_BEGIN;
        endcase;
        
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                            stringID: str_id,
                                            payload0: 0,
                                            payload1: 0 });
        state <= newstate;
        testFreq.setC(127);
        curFreq.setC(0);
    
    endrule
    
    rule finishSeq1 (linkFrom1.receive() matches tagged TEST_seq .n);
    
        if (n != seqQ.first())
        begin
        
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                stringID: `STREAMS_ICTEST_SEQ_FAILED,
                                                payload0: 0,
                                                payload1: 0 });
        end
        else
        begin
            if (n == 1)
            begin
            
                link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                    stringID: `STREAMS_ICTEST_SEQ_PASSED,
                                                    payload0: 0,
                                                    payload1: 0 });
                seqDone <= True;
            end
        end

        seqQ.deq();
        linkFrom1.deq();
    
    endrule
    
    rule finishSeq2 (linkFrom2.receive() matches tagged TEST_seq .n);
    
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                            stringID: `STREAMS_ICTEST_SEQ_FAILED,
                                            payload0: 0,
                                            payload1: 0 });
        
        linkFrom2.deq();

    endrule

    rule finishSeq3 (linkFrom3.receive() matches tagged TEST_seq .n);
    
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                            stringID: `STREAMS_ICTEST_SEQ_FAILED,
                                            payload0: 0,
                                            payload1: 0 });

        linkFrom3.deq();

    endrule

    rule finishMulti1 (linkFrom1.receive() matches tagged TEST_multi .n);
    
        
        if (n != multi1Q.first())
        begin
        
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                stringID: `STREAMS_ICTEST_MULTI_FAILED,
                                                payload0: 0,
                                                payload1: 0 });
        end
        else
        begin
            if (n == 1)
            begin
                if (multi2Passed && multi3Passed)
                begin
                    link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                        stringID: `STREAMS_ICTEST_MULTI_PASSED,
                                                        payload0: 0,
                                                        payload1: 0 });
                    multiDone <= True;
                end
                else
                begin
                    multi1Passed <= True;
                end
            end
        end

        multi1Q.deq();
        linkFrom1.deq();
    
    endrule

    rule finishMulti2 (linkFrom2.receive() matches tagged TEST_multi .n);
    
        
        if (n != multi2Q.first())
        begin
        
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                stringID: `STREAMS_ICTEST_MULTI_FAILED,
                                                payload0: 0,
                                                payload1: 0 });
        end
        else
        begin
            if (n == 1)
            begin
                if (multi1Passed && multi3Passed)
                begin
                    link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                        stringID: `STREAMS_ICTEST_MULTI_PASSED,
                                                        payload0: 0,
                                                        payload1: 0 });
                    multiDone <= True;
                end
                else
                begin
                    multi2Passed <= True;
                end
            end
        end

        multi2Q.deq();
        linkFrom2.deq();
    
    endrule

    rule finishMulti3 (linkFrom3.receive() matches tagged TEST_multi .n);
    
        
        if (n != multi3Q.first())
        begin
        
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                stringID: `STREAMS_ICTEST_MULTI_FAILED,
                                                payload0: 0,
                                                payload1: 0 });
        end
        else
        begin
            if (n == 1)
            begin
                if (multi1Passed && multi2Passed)
                begin
                    link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                        stringID: `STREAMS_ICTEST_MULTI_PASSED,
                                                        payload0: 0,
                                                        payload1: 0 });
                    multiDone <= True;
                end
                else
                begin
                    multi3Passed <= True;
                end
            end
        end

        multi3Q.deq();
        linkFrom3.deq();
    
    endrule

    rule finishTraffic1 (linkFrom1.receive() matches tagged TEST_traffic .n);

        if (n != traffic1Q.first())
        begin
        
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                stringID: `STREAMS_ICTEST_TRAFFIC_FAILED,
                                                payload0: 0,
                                                payload1: 0 });

        end
        else
        begin
            if (n == 1)
            begin
                link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                    stringID: `STREAMS_ICTEST_TRAFFIC_PASSED,
                                                    payload0: 0,
                                                    payload1: 0 });
                trafficDone <= True;
            end
        end

        traffic1Q.deq();
        linkFrom1.deq();
    
    endrule

    rule finishTraffic2 (linkFrom2.receive() matches tagged TEST_traffic .n);

        linkTo1.send(tagged TEST_traffic n);
        traffic1Q.enq(n);
        
        if (n != traffic2Q.first())
        begin
        
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                stringID: `STREAMS_ICTEST_TRAFFIC_FAILED,
                                                payload0: 0,
                                                payload1: 0 });

        end
        
        traffic2Q.deq();
        linkFrom2.deq();
        
    endrule
    
    
    rule finishTraffic3 (linkFrom3.receive() matches tagged TEST_traffic .n);

        linkTo2.send(tagged TEST_traffic n);
        traffic2Q.enq(n);
        
        if (n != traffic3Q.first())
        begin
        
            link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                                stringID: `STREAMS_ICTEST_TRAFFIC_FAILED,
                                                payload0: 0,
                                                payload1: 0 });

        end
        
        traffic3Q.deq();
        linkFrom3.deq();
        
    endrule

    rule error1 (linkFrom1.receive() matches tagged TEST_error .cd);
    
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                            stringID: `STREAMS_ICTEST_ERROR1,
                                            payload0: zeroExtend(cd),
                                            payload1: 0 });
 
        linkFrom1.deq();
    
    endrule
    

    rule error2 (linkFrom2.receive() matches tagged TEST_error .cd);
    
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                            stringID: `STREAMS_ICTEST_ERROR2,
                                            payload0: zeroExtend(cd),
                                            payload1: 0 });
 
        linkFrom2.deq();
    
    endrule
    
   (* descending_urgency="error3, error2, error1, finishTest, initiate, finishSeq3, finishSeq2, finishSeq1, finishMulti1, finishMulti2, finishMulti3, finishTraffic3, finishTraffic2, finishTraffic1" *)
   rule error3 (linkFrom3.receive() matches tagged TEST_error .cd);
    
        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_ICTEST,
                                            stringID: `STREAMS_ICTEST_ERROR3,
                                            payload0: zeroExtend(cd),
                                            payload1: 0 });
 
        linkFrom3.deq();
    
    endrule

    return tuple2(station, station_info);

endmodule


typedef union tagged
{
    Bit#(7) TEST_seq;
    Bit#(7) TEST_multi;
    Bit#(7) TEST_traffic;
    Bit#(7) TEST_error;
}
    TEST_MSG
        deriving (Eq, Bits);

module [CONNECTED_MODULE] mkICTestEntity1 (Tuple2#(PHYSICAL_STATION, STATION_INFO));

    
    FIFOF#(Bool) multi2Passed <- mkSizedFIFOF(16);
    FIFOF#(Bool) multi3Passed <- mkSizedFIFOF(16);
    
    match {.linkToC, .sendCstation, .sendCinfo} <- mkTestConnectionSend("1toC");
    match {.linkTo2, .send2station, .send2info} <- mkTestConnectionSend("1to2");
    match {.linkTo3, .send3station, .send3info} <- mkTestConnectionSend("1to3");
    match {.linkTo1B, .send1Bstation, .send1Binfo} <- mkTestConnectionBroadcast("1B");
    
    match {.linkFromC, .recvCstation, .recvCinfo} <- mkTestConnectionRecv("Cto1");
    match {.linkFrom2, .recv2station, .recv2info} <- mkTestConnectionRecv("2to1");
    match {.linkFrom3, .recv3station, .recv3info} <- mkTestConnectionRecv("3to1");
    match {.linkFrom2B, .recv2Bstation, .recv2Binfo} <- mkTestConnectionRecv("2B");
    match {.linkFrom3B, .recv3Bstation, .recv3Binfo} <- mkTestConnectionRecv("3B");

    let recvs = cons(recvCinfo,
                cons(recv2info,
                cons(recv3info,
                cons(recv2Binfo,
                cons(recv3Binfo,
                nil)))));
                
    let sends = cons(sendCinfo,
                cons(send2info,
                cons(send3info,
                cons(send1Binfo,
                nil))));

    let children = cons(recvCstation,
                   cons(recv2station,
                   cons(recv3station,
                   cons(recv2Bstation,
                   cons(recv3Bstation,
                   cons(sendCstation,
                   cons(send2station,
                   cons(send3station,
                   cons(send1Bstation,
                   nil)))))))));

    let station_info <- initRoutingTableLeaf(recvs, sends);
    let station <- mkPhysicalStation(children, station_info.routingTable);
    
    rule recvFromC;

        linkFromC.deq();

        case (linkFromC.receive()) matches 
            tagged TEST_seq .n:
            begin
                linkTo2.send(tagged TEST_seq n);
            end
            tagged TEST_traffic .n:
            begin
                linkToC.send(tagged TEST_traffic n);
            end
            default:
            begin
                linkToC.send(tagged TEST_error 0);
            end
        endcase
    
    endrule

    rule recvFrom3;
    
        case (linkFrom3.receive()) matches 
            tagged TEST_seq .n:
            begin
                linkTo3.send(tagged TEST_seq n);
            end
            default:
            begin
                linkToC.send(tagged TEST_error 3);
            end
        endcase
    
        linkFrom3.deq();
    
    endrule

    rule recvFrom2;
    
        case (linkFrom2.receive()) matches 
            tagged TEST_seq .n:
            begin
                linkToC.send(tagged TEST_seq n);
            end
            default:
            begin
                linkToC.send(tagged TEST_error 2);
            end
        endcase
    
        linkFrom2.deq();
    
    endrule

    rule recvFrom2B;
    
        case (linkFrom2B.receive()) matches
            tagged TEST_multi .n:
            begin
            
                if (multi3Passed.notEmpty())
                begin
                    linkToC.send(tagged TEST_multi n);
                    multi3Passed.deq();
                end
                else
                begin
                    linkTo1B.send(tagged TEST_multi n);
                    multi2Passed.enq(True);
                end

            end
            default:
            begin
                linkToC.send(tagged TEST_error 5);
            end
        endcase

        linkFrom2B.deq();
    
    endrule

    rule recvFrom3B;
    
        case (linkFrom3B.receive()) matches
            tagged TEST_multi .n:
            begin
            
                if (multi2Passed.notEmpty())
                begin
                    linkToC.send(tagged TEST_multi n);
                    multi2Passed.deq();
                end
                else
                begin
                    linkTo1B.send(tagged TEST_multi n);
                    multi3Passed.enq(True);
                end

            end
            default:
            begin
                linkToC.send(tagged TEST_error 6);
            end
        endcase

        linkFrom3B.deq();
    
    endrule

    return tuple2(station, station_info);

endmodule

module [CONNECTED_MODULE] mkICTestEntity2 (Tuple2#(PHYSICAL_STATION, STATION_INFO));

    
    FIFOF#(Bool) multi1Passed <- mkSizedFIFOF(16);
    FIFOF#(Bool) multi3Passed <- mkSizedFIFOF(16);
    
    match {.linkToC, .sendCstation, .sendCinfo} <- mkTestConnectionSend("2toC");
    match {.linkTo1, .send1station, .send1info} <- mkTestConnectionSend("2to1");
    match {.linkTo3, .send3station, .send3info} <- mkTestConnectionSend("2to3");
    match {.linkTo2B, .send2Bstation, .send2Binfo} <- mkTestConnectionBroadcast("2B");
    
    match {.linkFromC, .recvCstation, .recvCinfo} <- mkTestConnectionRecv("Cto2");
    match {.linkFrom1, .recv1station, .recv1info} <- mkTestConnectionRecv("1to2");
    match {.linkFrom3, .recv3station, .recv3info} <- mkTestConnectionRecv("3to2");
    match {.linkFrom1B, .recv1Bstation, .recv1Binfo} <- mkTestConnectionRecv("1B");
    match {.linkFrom3B, .recv3Bstation, .recv3Binfo} <- mkTestConnectionRecv("3B");

    let recvs = cons(recvCinfo,
                cons(recv1info,
                cons(recv3info,
                cons(recv1Binfo,
                cons(recv3Binfo,
                nil)))));
                
    let sends = cons(sendCinfo,
                cons(send1info,
                cons(send3info,
                cons(send2Binfo,
                nil))));

    let children = cons(recvCstation,
                   cons(recv1station,
                   cons(recv3station,
                   cons(recv1Bstation,
                   cons(recv3Bstation,
                   cons(sendCstation,
                   cons(send1station,
                   cons(send3station,
                   cons(send2Bstation,
                   nil)))))))));

    let station_info <- initRoutingTableLeaf(recvs, sends);
    let station <- mkPhysicalStation(children, station_info.routingTable);
    rule recvFromC;

        linkFromC.deq();

        case (linkFromC.receive()) matches 
            tagged TEST_multi .n:
            begin
                linkTo2B.send(tagged TEST_multi n);
            end
            tagged TEST_traffic .n:
            begin
                linkToC.send(tagged TEST_traffic n);
            end
            default:
            begin
                linkToC.send(tagged TEST_error 0);
            end
        endcase
    
    endrule

    rule recvFrom3;
    
        case (linkFrom3.receive()) matches 
            tagged TEST_seq .n:
            begin
                linkTo1.send(tagged TEST_seq n);
            end
            default:
            begin
                linkToC.send(tagged TEST_error 3);
            end
        endcase
    
        linkFrom3.deq();
    
    endrule

    rule recvFrom1;
    
        case (linkFrom1.receive()) matches 
            tagged TEST_seq .n:
            begin
                linkTo3.send(tagged TEST_seq n);
            end
            default:
            begin
                linkToC.send(tagged TEST_error 1);
            end
        endcase
    
        linkFrom1.deq();
    
    endrule

    rule recvFrom1B;
    
        case (linkFrom1B.receive()) matches
            tagged TEST_multi .n:
            begin
            
                if (multi3Passed.notEmpty())
                begin
                    linkToC.send(tagged TEST_multi n);
                    multi3Passed.deq();
                end
                else
                begin
                    multi1Passed.enq(True);
                end

            end
            default:
            begin
                linkToC.send(tagged TEST_error 4);
            end
        endcase

        linkFrom1B.deq();
    
    endrule

    rule recvFrom3B;
    
        case (linkFrom3B.receive()) matches
            tagged TEST_multi .n:
            begin
            
                if (multi1Passed.notEmpty())
                begin
                    linkToC.send(tagged TEST_multi n);
                    multi1Passed.deq();
                end
                else
                begin
                    multi3Passed.enq(True);
                end

            end
            default:
            begin
                linkToC.send(tagged TEST_error 6);
            end
        endcase

        linkFrom3B.deq();
    
    endrule

    return tuple2(station, station_info);

endmodule


module [CONNECTED_MODULE] mkICTestEntity3 (Tuple2#(PHYSICAL_STATION, STATION_INFO));

    
    FIFOF#(Bool) multi1Passed <- mkSizedFIFOF(16);
    FIFOF#(Bool) multi2Passed <- mkSizedFIFOF(16);
    
    match {.linkToC, .sendCstation, .sendCinfo} <- mkTestConnectionSend("3toC");
    match {.linkTo1, .send1station, .send1info} <- mkTestConnectionSend("3to1");
    match {.linkTo2, .send2station, .send2info} <- mkTestConnectionSend("3to2");
    match {.linkTo3B, .send3Bstation, .send3Binfo} <- mkTestConnectionBroadcast("3B");
    
    match {.linkFromC, .recvCstation, .recvCinfo} <- mkTestConnectionRecv("Cto3");
    match {.linkFrom1, .recv1station, .recv1info} <- mkTestConnectionRecv("1to3");
    match {.linkFrom2, .recv2station, .recv2info} <- mkTestConnectionRecv("2to3");
    match {.linkFrom1B, .recv1Bstation, .recv1Binfo} <- mkTestConnectionRecv("1B");
    match {.linkFrom2B, .recv2Bstation, .recv2Binfo} <- mkTestConnectionRecv("2B");

    let recvs = cons(recvCinfo,
                cons(recv1info,
                cons(recv2info,
                cons(recv1Binfo,
                cons(recv2Binfo,
                nil)))));
                
    let sends = cons(sendCinfo,
                cons(send1info,
                cons(send2info,
                cons(send3Binfo,
                nil))));

    let children = cons(recvCstation,
                   cons(recv1station,
                   cons(recv2station,
                   cons(recv1Bstation,
                   cons(recv2Bstation,
                   cons(sendCstation,
                   cons(send1station,
                   cons(send2station,
                   cons(send3Bstation,
                   nil)))))))));

    let station_info <- initRoutingTableLeaf(recvs, sends);
    let station <- mkPhysicalStation(children, station_info.routingTable);
    
    rule recvFromC;

        linkFromC.deq();

        case (linkFromC.receive()) matches 
            tagged TEST_traffic .n:
            begin
                linkToC.send(tagged TEST_traffic n);
            end
            default:
            begin
                linkToC.send(tagged TEST_error 0);
            end
        endcase
    
    endrule

    rule recvFrom2;
    
        case (linkFrom2.receive()) matches 
            tagged TEST_seq .n:
            begin
                linkTo1.send(tagged TEST_seq n);
            end
            default:
            begin
                linkToC.send(tagged TEST_error 2);
            end
        endcase
    
        linkFrom2.deq();
    
    endrule

    rule recvFrom1;
    
        case (linkFrom1.receive()) matches 
            tagged TEST_seq .n:
            begin
                linkTo2.send(tagged TEST_seq n);
            end
            default:
            begin
                linkToC.send(tagged TEST_error 1);
            end
        endcase
    
        linkFrom1.deq();
    
    endrule

    rule recvFrom1B;
    
        case (linkFrom1B.receive()) matches
            tagged TEST_multi .n:
            begin
            
                if (multi2Passed.notEmpty())
                begin
                    linkToC.send(tagged TEST_multi n);
                    multi2Passed.deq();
                end
                else
                begin
                    linkTo3B.send(tagged TEST_multi n);
                    multi1Passed.enq(True);
                end

            end
            default:
            begin
                linkToC.send(tagged TEST_error 4);
            end
        endcase

        linkFrom1B.deq();
    
    endrule

    rule recvFrom2B;
    
        case (linkFrom2B.receive()) matches
            tagged TEST_multi .n:
            begin
            
                if (multi1Passed.notEmpty())
                begin
                    linkToC.send(tagged TEST_multi n);
                    multi1Passed.deq();
                end
                else
                begin
                    linkTo3B.send(tagged TEST_multi n);
                    multi2Passed.enq(True);
                end

            end
            default:
            begin
                linkToC.send(tagged TEST_error 5);
            end
        endcase

        linkFrom2B.deq();
    
    endrule

    return tuple2(station, station_info);

endmodule

module mkTestConnectionSend#(String nm) (Tuple3#(CONNECTION_SEND#(TEST_MSG), PHYSICAL_STATION, LOGICAL_SEND_INFO));

    FIFOF#(PHYSICAL_PAYLOAD) q <- mkFIFOF();
    
    let outg = (interface PHYSICAL_CONNECTION_OUT
                    method first = q.first();
                    method notEmpty = q.notEmpty();
                    method deq = q.deq();
                endinterface);

    a foo = ?;
    let my_type = typeOf(foo);
    let info = LOGICAL_SEND_INFO
        {
            logicalName: nm,
            logicalType: printType(my_type),
            oneToMany: False
        };

    let inc = (interface CONNECTION_SEND
                   method send(x) = q.enq(zeroExtend(pack(x)));
               endinterface);
    
    let station <- mkPhysicalStationSendWrapper(outg);
    
    return tuple3(inc, station, info);

endmodule

module mkTestConnectionRecv#(String nm) (Tuple3#(CONNECTION_RECV#(TEST_MSG), PHYSICAL_STATION, LOGICAL_RECV_INFO));

    RWire#(TEST_MSG) dataW <- mkRWire();
    PulseWire ackW <- mkPulseWire();
    
    let outg = (interface CONNECTION_RECV
                    method receive if (dataW.wget() matches tagged Valid .v) = v;
                    method deq if (isValid(dataW.wget())) = ackW.send();
                endinterface);

    a foo = ?;
    let my_type = typeOf(foo);
    let info = LOGICAL_RECV_INFO
        {
            logicalName: nm,
            logicalType: printType(my_type),
            manyToOne: False
        };

    let inc = (interface PHYSICAL_CONNECTION_IN
                   method try(x) = dataW.wset(unpack(truncate(x)));
                   method success() = ackW;
               endinterface);
    
    let station <- mkPhysicalStationRecvWrapper(inc);
    
    return tuple3(outg, station, info);

endmodule


module mkTestConnectionBroadcast#(String nm) (Tuple3#(CONNECTION_SEND#(TEST_MSG), PHYSICAL_STATION, LOGICAL_SEND_INFO));

    FIFOF#(PHYSICAL_PAYLOAD) q <- mkFIFOF();
    
    let outg = (interface PHYSICAL_CONNECTION_OUT
                    method first = q.first();
                    method notEmpty = q.notEmpty();
                    method deq = q.deq();
                endinterface);

    a foo = ?;
    let my_type = typeOf(foo);
    let info = LOGICAL_SEND_INFO
        {
            logicalName: nm,
            logicalType: printType(my_type),
            oneToMany: True
        };

    let inc = (interface CONNECTION_SEND
                   method send(x) = q.enq(zeroExtend(pack(x)));
               endinterface);
    
    let station <- mkPhysicalStationSendWrapper(outg);
    
    return tuple3(inc, station, info);

endmodule

interface CONNECTION_SEND#(parameter type t_MSG);
    method Action send(t_MSG x);
endinterface

interface CONNECTION_RECV#(parameter type t_MSG);

    method t_MSG receive();
    method Action deq();
endinterface


module mkPhysicalStationSendWrapper#(PHYSICAL_SEND physical_send)
    // interface:
        (PHYSICAL_STATION);

    
    interface PHYSICAL_CONNECTION_IN incoming;

        method Action try(MESSAGE_DOWN msg);
            noAction;
        endmethod
        
        method Bool success() = True;

    endinterface

    interface PHYSICAL_CONNECTION_OUT outgoing;

       method MESSAGE_UP first();

            let msg = 
                MESSAGE_UP
                {
                    origin: 0,
                    payload: physical_send.first()
                };
                
            return msg;
             
       endmethod

       method Bool notEmpty() = physical_send.notEmpty();
       method Action deq() = physical_send.deq();

    endinterface

endmodule

module mkPhysicalStationRecvWrapper#(PHYSICAL_RECV physical_recv)
    // interface:
        (PHYSICAL_STATION);

    
    interface PHYSICAL_CONNECTION_IN incoming;

        method Action try(MESSAGE_DOWN msg);
            physical_recv.try(msg.payload);
        endmethod
        
        method Bool success() = physical_recv.success();

    endinterface

    interface PHYSICAL_CONNECTION_OUT outgoing;

       method MESSAGE_UP first() if (False) = ?;
       method Bool notEmpty() = False;
       method Action deq() = noAction;

    endinterface

endmodule
