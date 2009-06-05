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

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/rrr.bsh"
`include "asim/provides/soft_connections.bsh"

`include "asim/rrr/remote_client_stub_STARTER.bsh"
`include "asim/rrr/remote_server_stub_STARTER.bsh"

// Starter
interface Starter;

    // service methods: requests
    method Action acceptRequest_Run();
    method Action acceptRequest_Pause();
    method Action acceptRequest_Sync();
    method Action acceptRequest_DumpStats();
    method Action acceptRequest_DebugScan();

    method ActionValue#(CONTEXT_ID) acceptRequest_EnableContext();
    method ActionValue#(CONTEXT_ID) acceptRequest_DisableContext();

    // service methods: responses
    method Action sendResponse_DumpStats();
    method Action sendResponse_DebugScan();

    // client methods
    method Action makeRequest_EndSim(Bool success);
    
    //
    // Heartbeat --
    //   Message comes from the central controller.
    //
    //   Arguments:
    //     fpga_cycles   -- Unlike other arguments, cycles since beginning of time.
    //                      This is to keep fpga_cycle counting easy.
    //     model_cycles  -- Model cycles since the last heartbeat.
    //     instr_commits -- Committed instructions since the last heartbeat.
    //
    method Action makeRequest_Heartbeat(CONTEXT_ID ctxId, Bit#(64) fpga_cycles, Bit#(32) model_cycles, Bit#(32) instr_commits);

    //
    // FPGA Heartbeat --
    //   Central controller message based in FPGA clock, not model cycles.
    //   Useful for detecting deadlocks.
    //
    method Action makeRequest_FPGAHeartbeat(Bit#(64) fpga_cycles);

endinterface

// mkStarter
module [HASIM_MODULE] mkStarter(Starter);

    // ----------- stubs -----------
    ClientStub_STARTER client_stub <- mkClientStub_STARTER();
    ServerStub_STARTER server_stub <- mkServerStub_STARTER();
    
    // ----------- service methods: request ------------

    // Run
    method Action acceptRequest_Run ();
        let r <- server_stub.acceptRequest_Run();
    endmethod

    // Pause
    method Action acceptRequest_Pause ();
        let r <- server_stub.acceptRequest_Pause();
    endmethod

    // Sync
    method Action acceptRequest_Sync ();
        let r <- server_stub.acceptRequest_Sync();
    endmethod

    // DumpStats
    method Action acceptRequest_DumpStats ();
        let r <- server_stub.acceptRequest_DumpStats();
    endmethod

    // send response to DumpStats
    method Action sendResponse_DumpStats();
        server_stub.sendResponse_DumpStats(0);
    endmethod

    // DebugScan
    method Action acceptRequest_DebugScan ();
        let r <- server_stub.acceptRequest_DebugScan();
    endmethod

    // send response to DebugScan
    method Action sendResponse_DebugScan();
        server_stub.sendResponse_DebugScan(0);
    endmethod

    // SW side says enable a context
    method ActionValue#(CONTEXT_ID) acceptRequest_EnableContext();
        let r <- server_stub.acceptRequest_EnableContext();
        return truncate(r);
    endmethod

    // SW side says disable a context
    method ActionValue#(CONTEXT_ID) acceptRequest_DisableContext();
        let r <- server_stub.acceptRequest_DisableContext();
        return truncate(r);
    endmethod

    // ------------ client methods ------------

    // signal end of simulation
    method Action makeRequest_EndSim(Bool success);
        client_stub.makeRequest_EndSim(zeroExtend(pack(success)));
    endmethod

    // Heartbeat
    method Action makeRequest_Heartbeat(CONTEXT_ID ctxId, Bit#(64) fpga_cycles, Bit#(32) model_cycles, Bit#(32) instr_commits);
        client_stub.makeRequest_Heartbeat(0, contextIdToRRR(ctxId), fpga_cycles, model_cycles, instr_commits);
    endmethod

    method Action makeRequest_FPGAHeartbeat(Bit#(64) fpga_cycles);
        client_stub.makeRequest_Heartbeat(1, 0, fpga_cycles, 0, 0);
    endmethod

endmodule
