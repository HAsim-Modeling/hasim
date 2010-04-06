//
// Copyright (C) 2008 Intel Corporation
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

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"

`include "asim/rrr/remote_client_stub_DEBUG_SCAN.bsh"
`include "asim/dict/RINGID.bsh"
`include "asim/dict/DEBUG_SCAN.bsh"

//
// Debug scan is a ring of nodes that scan out debug state to the host when
// commanded.
//
// Scanning begins when the dump command is received.  The controller signals
// completion of the scan by enabling scanDone once.
//
interface DEBUG_SCAN_CONTROLLER;

  method Action scanStart();
  method Bool scanIsDone();

endinterface


//
// mkDebugScanController --
//
// Manage debug scan ring.
//
module [Connected_Module] mkDebugScanController
    // interface:
    (DEBUG_SCAN_CONTROLLER);

    // ****** State Elements ******

    // Communication link to the Stats themselves
    Connection_Chain#(DEBUG_SCAN_DATA) chain <- mkConnection_Chain(`RINGID_DEBUG_SCAN);

    // Communication to our RRR server
    ClientStub_DEBUG_SCAN client_stub <- mkClientStub_DEBUG_SCAN();

    // Track if we are done dumping
    Reg#(Bool) dump_finished  <- mkReg(False);
  
    // Our internal state
    Reg#(DEBUG_SCAN_STATE) state <- mkReg(DS_IDLE);
    

    // ****** Rules ******
  
    //
    // processResp --
    //
    // Process a response from an individual scan node.
    //  
    rule processResp (state == DS_DUMPING);
        let ds <- chain.recvFromPrev();

        case (ds) matches
            // A value to dump
            tagged DS_VAL .info:
            begin
                client_stub.makeRequest_Send(zeroExtend(info.id), info.value);
            end

            // Command came all the way around the loop.  Done dumping.
            tagged DS_DUMP:
            begin
                client_stub.makeRequest_Done(?);
            end
        endcase
    endrule


    //
    // waitForDoneAck
    //
    // Forward response that a full scan has reached the host.
    //
    rule waitForDoneAck (state == DS_DUMPING);
        let a <- client_stub.getResponse_Done();
        state <= DS_IDLE;
        dump_finished <= True;
    endrule
    

    // ====================================================================
    //
    // Methods
    //
    // ====================================================================

    //
    // scanStart --
    //    
    // Begin a debug scan out.
    //
    method Action scanStart() if (state == DS_IDLE);
        chain.sendToNext(tagged DS_DUMP);

        state <= DS_DUMPING;
        dump_finished <= False;
    endmethod
  
    method Bool scanIsDone();
        return dump_finished;
    endmethod
endmodule
