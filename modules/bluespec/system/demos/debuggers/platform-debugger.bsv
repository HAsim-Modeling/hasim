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
// @file platform-debugger.cpp
// @brief Platform Debugger Application
//
// @author Angshuman Parashar
//

import FIFO::*;
import Vector::*;

`include "asim/provides/virtual_platform.bsh"
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/physical_platform.bsh"
`include "asim/provides/ddr2_device.bsh"
`include "asim/provides/low_level_platform_interface.bsh"

`include "asim/rrr/server_stub_PLATFORM_DEBUGGER.bsh"

// types

typedef enum
{
    STATE_idle,
    STATE_running,
    STATE_calibrating
}
STATE
    deriving (Bits, Eq);

// mkApplication

module mkApplication#(VIRTUAL_PLATFORM vp)();
    
    LowLevelPlatformInterface llpi    = vp.llpint;
    PHYSICAL_DRIVERS          drivers = llpi.physicalDrivers;
    let sram = drivers.ddr2Driver;
    
    Reg#(STATE) state <- mkReg(STATE_idle);
    
    // instantiate stubs
    ServerStub_PLATFORM_DEBUGGER serverStub <- mkServerStub_PLATFORM_DEBUGGER(llpi.rrrServer);
    
    Reg#(Bit#(64)) curCycle <- mkReg(0);
    (* no_implicit_conditions *)
    (* fire_when_enabled *)
    rule updateCycle (True);
        curCycle <= curCycle + 1;
    endrule


    // receive the start request from software
    rule start_debug (state == STATE_idle);
        
        let param <- serverStub.acceptRequest_StartDebug();
        serverStub.sendResponse_StartDebug(0);
        state <= STATE_running;
        
    endrule
    
    //
    // Platform-specific debug code goes here.
    //
    rule accept_load_req0 (state == STATE_running);
        
        let addr <- serverStub.acceptRequest_ReadReq0();        
        serverStub.sendResponse_ReadReq0(0);

        sram[0].readReq(truncate(addr));

    endrule
    
    rule accept_load_rsp0 (state == STATE_running);
        
        let dummy <- serverStub.acceptRequest_ReadRsp0();
        let data  <- sram[0].readRsp();
        serverStub.sendResponse_ReadRsp0(truncate(data));
        
    endrule
    
    rule accept_load_req1 (state == STATE_running);
        
        let addr <- serverStub.acceptRequest_ReadReq1();        
        serverStub.sendResponse_ReadReq1(0);

        sram[valueOf(TSub#(FPGA_DDR_BANKS, 1))].readReq(truncate(addr));

    endrule
    
    rule accept_load_rsp1 (state == STATE_running);
        
        let dummy <- serverStub.acceptRequest_ReadRsp1();
        let data  <- sram[valueOf(TSub#(FPGA_DDR_BANKS, 1))].readRsp();
        serverStub.sendResponse_ReadRsp1(truncate(data));
        
    endrule
    
    rule accept_write_req (state == STATE_running);
        
        let addr <- serverStub.acceptRequest_WriteReq();        
        serverStub.sendResponse_WriteReq(0);

        sram[0].writeReq(truncate(addr));
        if (valueOf(FPGA_DDR_BANKS) > 1)
            sram[1].writeReq(truncate(addr));
        
    endrule
    
    rule accept_write_data (state == STATE_running);
        
        let resp <- serverStub.acceptRequest_WriteData();
        sram[0].writeData(zeroExtend(resp.data), truncate(resp.mask));
        if (valueOf(FPGA_DDR_BANKS) > 1)
            sram[1].writeData(~zeroExtend(resp.data), truncate(resp.mask));

        serverStub.sendResponse_WriteData(0);
        
    endrule
    
    rule accept_status_check (True);
        
        let bank <- serverStub.acceptRequest_StatusCheck();
        serverStub.sendResponse_StatusCheck(sram[bank[0]].statusCheck());
        
    endrule
    

    //
    // read_latency rules are useful for calibrating the optimal size of
    // the controller's read response buffer size.  The buffer must be large
    // enough to hold responses from all pending read requests in the RAM's
    // read pipeline.
    //

    Reg#(FPGA_DDR_ADDRESS) calAddr <- mkRegU();
    Reg#(Bit#(64)) calStartCycle <- mkRegU();
    Reg#(Bit#(16)) calReads <- mkRegU();
    Reg#(Maybe#(Bit#(64))) calFirstRespCycle <- mkRegU();
    Reg#(Bit#(16)) calReqCnt <- mkRegU();
    Reg#(Bit#(16)) calRespCnt <- mkRegU();

    rule accept_read_latency (state == STATE_running);
        let cal <- serverStub.acceptRequest_ReadLatency();
        sram[0].setMaxReads(truncate(cal.maxOutstanding));

        state <= STATE_calibrating;
        calAddr <= 0;
        calStartCycle <= curCycle;
        calReads <= cal.nReads;
        calFirstRespCycle <= tagged Invalid;
        calReqCnt <= 0;
        calRespCnt <= 0;
    endrule

    rule read_latency_req ((state == STATE_calibrating) &&
                           (calReqCnt < calReads));
        sram[0].readReq(calAddr);
        calAddr <= calAddr + fromInteger(valueOf(TMul#(FPGA_DDR_BURST_LENGTH, TDiv#(FPGA_DDR_DUALEDGE_DATA_SZ, FPGA_DDR_WORD_SZ))));
        calReqCnt <= calReqCnt + 1;
    endrule

    (* descending_urgency = "accept_status_check, read_latency_resp" *)
    rule read_latency_resp (state == STATE_calibrating);
        let data  <- sram[0].readRsp();
        if (! isValid(calFirstRespCycle))
        begin
            calFirstRespCycle <= tagged Valid curCycle;
        end

        if (calRespCnt + 1 == (calReads * fromInteger(valueOf(FPGA_DDR_BURST_LENGTH))))
        begin
            let first_read_latency = validValue(calFirstRespCycle) - calStartCycle;
            let total_latency = curCycle - calStartCycle;
            serverStub.sendResponse_ReadLatency(truncate(first_read_latency),
                                                truncate(total_latency));
            
            state <= STATE_running;
        end

        calRespCnt <= calRespCnt + 1;
    endrule

endmodule
