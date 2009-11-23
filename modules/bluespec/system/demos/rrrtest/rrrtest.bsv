//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
//

//
// @file rrrtest.bsv
// @brief RRR Test System
//
// @author Angshuman Parashar
//

`include "asim/provides/virtual_platform.bsh"
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/physical_platform.bsh"
`include "asim/provides/low_level_platform_interface.bsh"

`include "asim/rrr/service_ids.bsh"
`include "asim/rrr/server_stub_RRRTEST.bsh"
`include "asim/rrr/client_stub_RRRTEST.bsh"

// types

typedef enum 
{
    STATE_idle, 
    STATE_f2hOneWay1,
    STATE_f2hOneWay8,
    STATE_f2hOneWay16,
    STATE_f2hTwoWayReq,
    STATE_f2hTwoWayResp,
    STATE_f2hTwoWayPipe
} 
STATE deriving(Bits,Eq);

typedef Bit#(64) PAYLOAD;

// mkApplication

module mkApplication#(VIRTUAL_PLATFORM vp)();

    LowLevelPlatformInterface llpi = vp.llpint;
    
    // instantiate stubs
    ServerStub_RRRTEST serverStub <- mkServerStub_RRRTEST(llpi.rrrServer);
    ClientStub_RRRTEST clientStub <- mkClientStub_RRRTEST(llpi.rrrClient);
    
    // counters
    Reg#(Bit#(64)) curTick              <- mkReg(0);
    Reg#(Bit#(64)) timer                <- mkReg(0);
    Reg#(Bit#(64)) testLength           <- mkReg(0);
    Reg#(Bit#(64)) outstandingResponses <- mkReg(0);
    
    // test payload
    PAYLOAD payload = 'h12345678abcdef2b;
    
    // state
    Reg#(STATE) state <- mkReg(STATE_idle);
    
    // count FPGA cycles
    rule tick (True);
        
        if (curTick == '1)
        begin
            curTick <= 0;
        end
        else
        begin
            curTick <= curTick + 1;
        end
        
    endrule
    
    //
    // FPGA -> Host one-way test
    //
    rule start_f2h_oneway_test (state == STATE_idle);
        
        // accept request from host
        let test <- serverStub.acceptRequest_F2HOneWayTest();
        
        // start the clock and let it rip
        timer      <= curTick;
        testLength <= test.length;
        if (test.which == 0)
            state <= STATE_f2hOneWay1;
        else if (test.which == 1)
            state <= STATE_f2hOneWay8;
        else
            state <= STATE_f2hOneWay16;
        
    endrule
    
    rule do_f2h_oneway_test1 (state == STATE_f2hOneWay1 && testLength != 0);
        
        clientStub.makeRequest_F2HOneWayMsg1(payload);
        testLength <= testLength - 1;
        
    endrule
    
    rule do_f2h_oneway_test8 (state == STATE_f2hOneWay8 && testLength != 0);
        
        clientStub.makeRequest_F2HOneWayMsg8(1, 2, 3, 4, 5, 6, 7, 8);
        testLength <= testLength - 1;
        
    endrule
    
    rule do_f2h_oneway_test16 (state == STATE_f2hOneWay16 && testLength != 0);
        
        clientStub.makeRequest_F2HOneWayMsg16(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
        testLength <= testLength - 1;
        
    endrule
    
    rule finish_f2h_oneway_test (((state == STATE_f2hOneWay1) ||
                                  (state == STATE_f2hOneWay8) ||
                                  (state == STATE_f2hOneWay16)) &&
                                 testLength == 0);
        
        // stop the clock and measure the time
        Bit#(64) cycles = curTick - timer;
        
        // send response to start request
        serverStub.sendResponse_F2HOneWayTest(cycles);
        
        state <= STATE_idle;
        
    endrule
    
    //
    // FPGA -> Host two-way test (unpipelined)
    //
    rule start_f2h_twoway_test (state == STATE_idle);
        
        // accept request from host
        let test_length <- serverStub.acceptRequest_F2HTwoWayTest();
        
        // start the clock and let it rip
        timer      <= curTick;
        testLength <= test_length;
        state      <= STATE_f2hTwoWayReq;
        
    endrule
    
    rule do_f2h_twoway_test_req (state == STATE_f2hTwoWayReq && testLength != 0);
        
        clientStub.makeRequest_F2HTwoWayMsg(payload);
        state <= STATE_f2hTwoWayResp;

    endrule

    rule do_f2h_twoway_test_resp (state == STATE_f2hTwoWayResp);
        
        PAYLOAD dummy <- clientStub.getResponse_F2HTwoWayMsg();
        state <= STATE_f2hTwoWayReq;
        testLength <= testLength - 1;

    endrule
    
    rule finish_f2h_twoway_test (state == STATE_f2hTwoWayReq && testLength == 0);
        
        // stop the clock and measure the time
        Bit#(64) cycles = curTick - timer;
        
        // send response to start request
        serverStub.sendResponse_F2HTwoWayTest(cycles);
        
        state <= STATE_idle;
        
    endrule
    
    //
    // FPGA -> Host two-way test (pipelined)
    //
    rule start_f2h_twoway_pipe_test (state == STATE_idle);
        
        // accept request from host
        let test_length <- serverStub.acceptRequest_F2HTwoWayPipeTest();
        
        // start the clock and let it rip
        timer                 <= curTick;
        testLength            <= test_length;
        outstandingResponses  <= test_length;
        state                 <= STATE_f2hTwoWayPipe;
        
    endrule
    
    rule do_f2h_twoway_pipe_test_req (state == STATE_f2hTwoWayPipe && testLength != 0);
        
        clientStub.makeRequest_F2HTwoWayMsg(payload);
        testLength <= testLength - 1;

    endrule

    rule do_f2h_twoway_pipe_test_resp (state == STATE_f2hTwoWayPipe && outstandingResponses != 0);
        
        PAYLOAD dummy <- clientStub.getResponse_F2HTwoWayMsg();
        outstandingResponses <= outstandingResponses - 1;

    endrule
    
    rule finish_f2h_twoway_pipe_test (state == STATE_f2hTwoWayPipe && outstandingResponses == 0);
        
        // stop the clock and measure the time
        Bit#(64) cycles = curTick - timer;
        
        // send response to start request
        serverStub.sendResponse_F2HTwoWayPipeTest(cycles);
        
        state <= STATE_idle;
        
    endrule
    
endmodule
