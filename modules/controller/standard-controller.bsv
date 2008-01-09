`include "hasim_common.bsh"
`include "soft_connections.bsh"

`include "streams.bsh"
`include "hasim_local_controller.bsh"
`include "hasim_stats_controller.bsh"
`include "hasim_events_controller.bsh"
`include "hasim_assertions_controller.bsh"
`include "hasim_command_controller.bsh"

// ================ Standard Controller ===============

module [HASim_Module] mkController ();

  // instantiate interfaces to the outside world (i.e., connections to virtual devices)
  // that all sub-controllers need to share.
  Streams streams <- mkStreams();

  // instantiate sub-controllers
  EventsController     events_controller  <- mkEventsController();
  StatsController      stats_controller   <- mkStatsController();
  AssertionsController asserts_controller <- mkAssertionsController();
  CommandController    command_controller <- mkCommandController(streams,
                                                                 events_controller,   // temporary
                                                                 stats_controller,    // temporary
                                                                 asserts_controller); // temporary

endmodule
