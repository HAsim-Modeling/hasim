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
    method Action makeRequest_Heartbeat(Bit#(64) fpga_cycles, Bit#(64) model_cycles);

endinterface

// mkStarter
module [HASim_Module] mkStarter(Starter);

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
    method Action makeRequest_Heartbeat(Bit#(64) fpga_cycles, Bit#(64) model_cycles);
        Bit#(128) cycles;
        cycles[63:0] = model_cycles;
        cycles[127:64] = fpga_cycles;
        client_stub.makeRequest_Heartbeat(cycles);
    endmethod

endmodule

