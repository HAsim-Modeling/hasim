`include "hasim_common.bsh"
`include "soft_connections.bsh"

`include "streams.bsh"
`include "hasim_local_controller.bsh"
`include "hasim_stats_controller.bsh"
`include "hasim_events_controller.bsh"
`include "hasim_assertions_controller.bsh"
`include "hasim_module_controller.bsh"
`include "starter.bsh"

// control state
typedef enum
{
    CONTROL_STATE_idle,    // simulation halted, modules are sync'ed
    CONTROL_STATE_running, // simulation running
    CONTROL_STATE_paused,  // simulation halted, modules may not be sync'ed
    CONTROL_STATE_dumping  // simulation halted, modules sync'ed, dumping stats
}
CONTROL_STATE
    deriving (Bits, Eq);

// ================ Standard Controller ===============

module [HASim_Module] mkController ();

    // instantiate shared links to the outside world
    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    // instantiate sub-controllers
    ModuleController     moduleController  <- mkModuleController(link_streams);
    EventsController     eventsController  <- mkEventsController(link_streams);
    StatsController      statsController   <- mkStatsController(link_streams);
    AssertionsController assertsController <- mkAssertionsController(link_streams);

    // instantiate starter
    Starter starter <- mkStarter();

    // state
    Reg#(CONTROL_STATE) state <- mkReg(CONTROL_STATE_idle);

    // === rules ===

    // accept Run request from starter
    rule accept_request_Run (state == CONTROL_STATE_idle || state == CONTROL_STATE_paused);
        starter.acceptRequest_Run();
        moduleController.run();
        state <= CONTROL_STATE_running;
    endrule

    // accept Pause request from starter
    rule accept_request_Pause (state == CONTROL_STATE_running);
        starter.acceptRequest_Pause();
        moduleController.pause();
        state <= CONTROL_STATE_paused;
    endrule

    // accept Sync request from starter
    rule accept_request_Sync (state == CONTROL_STATE_paused);
        starter.acceptRequest_Sync();
        moduleController.sync();
        state <= CONTROL_STATE_idle;
    endrule

    // accept DumpStats request from starter
    rule accept_request_DumpStats (state == CONTROL_STATE_idle);
        starter.acceptRequest_DumpStats();
        statsController.doCommand(Stats_Dump);
        state <= CONTROL_STATE_dumping;
    endrule

    // monitor module controller
    rule monitor_module_controller (state == CONTROL_STATE_running);
        let success = moduleController.queryResult();
        starter.makeRequest_EndSim(success ? 1 : 0);
        state <= CONTROL_STATE_paused;
    endrule

    // monitor stats controller
    rule sync_model (state == CONTROL_STATE_dumping && statsController.noMoreStats());
        starter.sendResponse_DumpStats();
        state <= CONTROL_STATE_idle;
    endrule

endmodule
