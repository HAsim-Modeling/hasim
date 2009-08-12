//
// Copyright (C) 2009 Intel Corporation
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

`include "asim/provides/virtual_platform.bsh"
`include "asim/provides/virtual_devices.bsh"
`include "asim/provides/physical_platform.bsh"
`include "asim/provides/low_level_platform_interface.bsh"
`include "asim/provides/shared_memory.bsh"

`include "asim/rrr/service_ids.bsh"
`include "asim/rrr/server_stub_SHMEM_TEST.bsh"

// types

typedef enum 
{
    STATE_idle, 
    STATE_OneWay
} 
STATE deriving(Bits,Eq);

typedef Bit#(64) PAYLOAD;

// mkSystem

module mkApplication#(VIRTUAL_PLATFORM vp)();
    
    // instantiate the virtual devices I need
    SHARED_MEMORY sharedMemory = vp.virtualDevices.sharedMemory;

    // instantiate stubs
    ServerStub_SHMEM_TEST serverStub <- mkServerStub_SHMEM_TEST(vp.llpint.rrrServer);
    
    // counters
    Reg#(SHARED_MEMORY_DATA) curTick     <- mkReg(0);
    Reg#(SHARED_MEMORY_DATA) timer       <- mkReg(0);
    Reg#(SHARED_MEMORY_BURST_LENGTH) burstLength <- mkReg(0);

    SHARED_MEMORY_DATA cycles = curTick - timer;

    // test payload
    PAYLOAD payload = '1;
    
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
    
    rule start_oneway_test (state == STATE_idle);
        
        let burst_length <- serverStub.acceptRequest_OneWayTest();
        
        // start the clock (only for the first request) and let it rip
        if (timer == 0)
        begin
            timer <= curTick;
        end
        
        burstLength <= unpack(burst_length);
        state       <= STATE_OneWay;
        
        sharedMemory.writeBurstReq(0, burst_length);
        
    endrule
    
    rule cont_oneway_test (state == STATE_OneWay && burstLength != 0);
    
        sharedMemory.writeBurstData(cycles);
        burstLength <= burstLength - 1;

    endrule
    
    rule end_oneway_test (state == STATE_OneWay && burstLength == 0);
        
        serverStub.sendResponse_OneWayTest(pack(cycles));
        state <= STATE_idle;
        
    endrule
    
endmodule
