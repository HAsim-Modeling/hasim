`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"

`include "asim/provides/streams.bsh"
`include "asim/provides/stats_controller.bsh"
`include "asim/provides/debug_scan_controller.bsh"
`include "asim/provides/events_controller.bsh"
`include "asim/provides/assertions_controller.bsh"
`include "asim/provides/params_controller.bsh"
`include "asim/provides/module_controller.bsh"

interface CENTRAL_CONTROLLERS;

    interface MODULE_CONTROLLER moduleController;
    interface EVENTS_CONTROLLER eventsController;
    interface STATS_CONTROLLER statsController;
    interface DEBUG_SCAN_CONTROLLER debugScanController;
    interface ASSERTIONS_CONTROLLER assertsController;
    interface PARAMS_CONTROLLER paramsController;

endinterface

// ================ Standard Controller ===============

module [HASIM_MODULE] mkCentralControllers
    // interface:
        (CENTRAL_CONTROLLERS);

    // instantiate shared links to the outside world
    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    // instantiate sub-controllers
    MODULE_CONTROLLER     moduleCtrl    <- mkModuleController(link_streams);
    EVENTS_CONTROLLER     eventsCtrl    <- mkEventsController(link_streams);
    STATS_CONTROLLER      statsCtrl     <- mkStatsController();
    DEBUG_SCAN_CONTROLLER debugScanCtrl <- mkDebugScanController();
    ASSERTIONS_CONTROLLER assertsCtrl   <- mkAssertionsController();
    PARAMS_CONTROLLER     paramsCtrl    <- mkParamsController();

    interface moduleController    = moduleCtrl;
    interface eventsController    = eventsCtrl;
    interface statsController     = statsCtrl;
    interface debugScanController = debugScanCtrl;
    interface assertsController   = assertsCtrl;
    interface paramsController    = paramsCtrl;

endmodule
