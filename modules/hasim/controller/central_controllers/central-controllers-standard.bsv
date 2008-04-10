`include "hasim_common.bsh"
`include "soft_connections.bsh"

`include "streams.bsh"
`include "stats_controller.bsh"
`include "events_controller.bsh"
`include "assertions_controller.bsh"
`include "module_controller.bsh"

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

interface CENTRAL_CONTROLLERS;

    interface MODULE_CONTROLLER moduleController;
    interface EVENTS_CONTROLLER eventsController;
    interface STATS_CONTROLLER statsController;
    interface ASSERTIONS_CONTROLLER assertsController;

endinterface

// ================ Standard Controller ===============

module [HASim_Module] mkCentralControllers
    // interface:
        (CENTRAL_CONTROLLERS);

    // instantiate shared links to the outside world
    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    // instantiate sub-controllers
    MODULE_CONTROLLER     moduleCtrl  <- mkModuleController(link_streams);
    EVENTS_CONTROLLER     eventsCtrl  <- mkEventsController(link_streams);
    STATS_CONTROLLER      statsCtrl   <- mkStatsController();
    ASSERTIONS_CONTROLLER assertsCtrl <- mkAssertionsController();

    interface moduleController  = moduleCtrl;
    interface eventsController  = eventsCtrl;
    interface statsController   = statsCtrl;
    interface assertsController = assertsCtrl;

endmodule
