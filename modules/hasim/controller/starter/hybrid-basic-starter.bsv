import hasim_common::*;
import rrr::*;
import soft_connections::*;

`include "asim/rrr/remote_client_stub_STARTER.bsh"
`include "asim/rrr/remote_server_stub_STARTER.bsh"

// Starter
interface Starter;

    // service methods: requests
    method Action acceptRequest_Run();
    method Action acceptRequest_Pause();
    method Action acceptRequest_Sync();
    method Action acceptRequest_DumpStats();

    // service methods: responses
    method Action sendResponse_DumpStats();

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
    method Action makeRequest_Heartbeat(Bit#(64) fpga_cycles, Bit#(32) model_cycles, Bit#(32) instr_commits);

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

    // ------------ client methods ------------

    // signal end of simulation
    method Action makeRequest_EndSim(Bool success);
        client_stub.makeRequest_EndSim(success);
    endmethod

    // Heartbeat
    method Action makeRequest_Heartbeat(Bit#(64) fpga_cycles, Bit#(32) model_cycles, Bit#(32) instr_commits);
        Bit#(128) info;
        info[31:0] = instr_commits;
        info[63:32] = model_cycles;
        info[127:64] = fpga_cycles;
        client_stub.makeRequest_Heartbeat(info);
    endmethod

endmodule

