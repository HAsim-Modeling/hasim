import hasim_common::*;
import rrr::*;
import soft_connections::*;

`include "asim/rrr/service_ids.bsh"
`define SERVICE_ID  `STARTER_SERVICE_ID

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

endinterface

// mkStarter
module [HASim_Module] mkStarter(Starter);

    // ----------- links -----------
    Connection_Receive#(UINT32)        link_Run       <- mkConnection_Receive("rrr_server_STARTER_Run");
    Connection_Receive#(UINT32)        link_Pause     <- mkConnection_Receive("rrr_server_STARTER_Pause");
    Connection_Receive#(UINT32)        link_Sync      <- mkConnection_Receive("rrr_server_STARTER_Sync");
    Connection_Server#(UINT32, UINT32) link_DumpStats <- mkConnection_Server("rrr_server_STARTER_DumpStats");
    Connection_Send#(Bool)             link_EndSim    <- mkConnection_Send("rrr_client_STARTER_EndSim");

    // ----------- service methods: request ------------

    // Run
    method Action acceptRequest_Run ();
        link_Run.deq();
    endmethod

    // Pause
    method Action acceptRequest_Pause ();
        link_Pause.deq();
    endmethod

    // Sync
    method Action acceptRequest_Sync ();
        link_Sync.deq();
    endmethod

    // DumpStats
    method Action acceptRequest_DumpStats ();
        link_DumpStats.deq();
    endmethod

    // send response to DumpStats
    method Action sendResponse_DumpStats();
        link_DumpStats.makeResp(0);
    endmethod

    // ------------ client methods ------------

    // signal end of simulation
    method Action makeRequest_EndSim(Bool success);
        link_EndSim.send(success);
    endmethod

endmodule

