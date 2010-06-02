//
// Copyright (C) 2010 Intel Corporation
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

//
// @file chan-test.bsv
// @brief Channel integrity test
//
// @author Michael Adler
//

import FIFO::*;
import LFSR::*;

`include "asim/provides/librl_bsv.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/virtual_platform.bsh"
`include "asim/provides/low_level_platform_interface.bsh"

`include "asim/rrr/service_ids.bsh"
`include "asim/rrr/server_stub_CHANTEST.bsh"
`include "asim/rrr/client_stub_CHANTEST.bsh"

// types

typedef enum 
{
    STATE_IDLE,
    STATE_F2H8
} 
STATE deriving(Bits,Eq);

typedef Bit#(64) PAYLOAD;

// mkApplication

module mkApplication#(VIRTUAL_PLATFORM vp)();

    LowLevelPlatformInterface llpi = vp.llpint;
    
    // Instantiate stubs
    ServerStub_CHANTEST serverStub <- mkServerStub_CHANTEST(llpi.rrrServer);
    ClientStub_CHANTEST clientStub <- mkClientStub_CHANTEST(llpi.rrrClient);
    
    // Counters
    Reg#(Bit#(64)) cycle <- mkReg(0);
    Reg#(Bit#(64)) f2hIter <- mkReg(0);
    Reg#(Bit#(64)) h2fPackets <- mkReg(0);
    Reg#(Bit#(64)) h2fErrors <- mkReg(0);
    Reg#(Bit#(64)) h2fBitsFlipped <- mkReg(0);
    
    Reg#(STATE) state <- mkReg(STATE_IDLE);
    
    // Count FPGA cycles
    rule tick (True);
        cycle <= cycle + 1;
    endrule
    
    //
    // FPGA -> Host one-way test
    //
    rule startF2HOneWayTest (state == STATE_IDLE);
        // accept request from host
        let count <- serverStub.acceptRequest_F2HStartOneWayTest();
        f2hIter <= count;

        state <= STATE_F2H8;
    endrule
    

    //
    // sendF2HMessage --
    //     Send the requested number of messages to the host.  Each message
    //     has 4 random 64 bit values and their complements.
    //

    LFSR#(Bit#(32)) random[8];
    for (Integer n = 0; n < 8; n = n + 1)
    begin
        random[n] <- mkFeedLFSR(lfsr32FeedPolynomials(n));
    end

    rule sendF2HMessage (state == STATE_F2H8);
        UINT64 p0 = { random[0].value(), random[1].value() };
        UINT64 p1 = { random[2].value(), random[3].value() };
        UINT64 p2 = { random[4].value(), random[5].value() };
        UINT64 p3 = { random[6].value(), random[7].value() };
        for (Integer n = 0; n < 8; n = n + 1)
        begin
            random[n].next();
        end

        clientStub.makeRequest_F2HOneWayMsg8(p0, p1, p2, p3,
                                             ~p0, ~p1, ~p2, ~p3);
        
        // Update test iteration counter
        let i = f2hIter - 1;
        f2hIter <= i;

        if (i == 0)
        begin
            // Done
            state <= STATE_IDLE;
        end
    endrule


    //
    // Host -> FPGA test
    //
    FIFO#(Tuple3#(UInt#(16), Bit#(16), IN_TYPE_H2FOneWayMsg8)) h2fErrorQ <- mkFIFO();

    Reg#(Bit#(64)) lastMsgCycle <- mkReg(0);
    Reg#(Bit#(16)) chunkIdx <- mkReg(0);

    (* conservative_implicit_conditions *)
    rule checkH2FMessage (True);
        IN_TYPE_H2FOneWayMsg8 msg <- serverStub.acceptRequest_H2FOneWayMsg8();

        //
        // This code is more relevant to the ACP than many other channels.
        // On ACP, messages are grouped into chunks.  This code tries to
        // figure out the index of this message in the chunk by looking
        // for a time gap between chunks.
        //
        Bit#(16) chunk_idx;
        if (cycle > (lastMsgCycle + 20))
        begin
            // New chunk
            chunk_idx = 0;
        end
        else
        begin
            chunk_idx = chunkIdx + 1;
        end

        lastMsgCycle <= cycle;
        chunkIdx <= chunk_idx;

        //
        // 4-7 are complements of 0-3.
        //
        let mask0 = ~(msg.payload0 ^ msg.payload4);
        let mask1 = ~(msg.payload1 ^ msg.payload5);
        let mask2 = ~(msg.payload2 ^ msg.payload6);
        let mask3 = ~(msg.payload3 ^ msg.payload7);

        if ((mask0 != 0) ||
            (mask1 != 0) ||
            (mask2 != 0) ||
            (mask3 != 0))
        begin
            h2fErrors <= h2fErrors + 1;

            // Count the number of flipped bits
            UInt#(16) bit_errors =
                zeroExtend(countOnes(mask0)) +
                zeroExtend(countOnes(mask1)) +
                zeroExtend(countOnes(mask2)) +
                zeroExtend(countOnes(mask3));

            h2fErrorQ.enq(tuple3(bit_errors, chunk_idx, msg));
        end

        h2fPackets <= h2fPackets + 1;
    endrule


    (* descending_urgency = "h2fError, sendF2HMessage" *)
    rule h2fError (True);
        match {.bits, .chunk_idx, .msg} = h2fErrorQ.first();
        h2fErrorQ.deq();
        
        h2fBitsFlipped <= h2fBitsFlipped + zeroExtend(pack(bits));
        clientStub.makeRequest_H2FNoteError(pack(bits),
                                            chunk_idx,
                                            msg.payload0,
                                            msg.payload1,
                                            msg.payload2,
                                            msg.payload3,
                                            msg.payload4,
                                            msg.payload5,
                                            msg.payload6,
                                            msg.payload7);
    endrule

    rule getH2FErrorCount (state == STATE_IDLE);
        let dummy <- serverStub.acceptRequest_H2FGetStats();
        serverStub.sendResponse_H2FGetStats(h2fPackets, h2fErrors, h2fBitsFlipped);
    endrule

endmodule
