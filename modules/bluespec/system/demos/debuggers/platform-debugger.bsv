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
    STATE_running
}
STATE
    deriving (Bits, Eq);

// mkApplication

module mkApplication#(VIRTUAL_PLATFORM vp)();
    
    LowLevelPlatformInterface llpi    = vp.llpint;
    PHYSICAL_DRIVERS          drivers = llpi.physicalDrivers;
    DDR2_DRIVER               sram    = drivers.ddr2Driver;
    
    Reg#(STATE) state <- mkReg(STATE_idle);
    
    // instantiate stubs
    ServerStub_PLATFORM_DEBUGGER serverStub <- mkServerStub_PLATFORM_DEBUGGER(llpi.rrrServer);
    
    // receive the start request from software
    rule start_debug (state == STATE_idle);
        
        let param <- serverStub.acceptRequest_StartDebug();
        serverStub.sendResponse_StartDebug(0);
        state <= STATE_running;
        
    endrule
    
    //
    // Platform-specific debug code goes here.
    //
    
    rule accept_load_req (state == STATE_running);
        
        let addr <- serverStub.acceptRequest_ReadReq();        
        serverStub.sendResponse_ReadReq(0);

        sram.readReq(truncate(addr));
        
    endrule
    
    rule accept_load_rsp (state == STATE_running);
        
        let dummy <- serverStub.acceptRequest_ReadRsp();
        let data  <- sram.readRsp();
        serverStub.sendResponse_ReadRsp(truncate(data));
        
    endrule
    
    rule accept_write_req (state == STATE_running);
        
        let addr <- serverStub.acceptRequest_WriteReq();        
        serverStub.sendResponse_WriteReq(0);

        sram.writeReq(truncate(addr));
        
    endrule
    
    rule accept_write_data (state == STATE_running);
        
        let resp <- serverStub.acceptRequest_WriteData();
        sram.writeData(zeroExtend(resp.data), truncate(resp.mask));

        serverStub.sendResponse_WriteData(0);
        
    endrule
    
endmodule
