`include "hasim_common.bsh"
`include "soft_connections.bsh"

`include "central_controllers.bsh"
`include "module_controller.bsh"
`include "events_controller.bsh"
`include "stats_controller.bsh"
`include "assertions_controller.bsh"
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

    // instantiate all the sub-controllers
    CENTRAL_CONTROLLERS centralControllers <- mkCentralControllers();

    // instantiate starter
    Starter starter <- mkStarter();

    // The timing model must tell us the current model cycle.  By convention,
    // it is the token request stage at the head of the pipeline.
    Connection_Receive#(Bool) link_model_cycle <- mkConnection_Receive("model_cycle");

    // state
    Reg#(CONTROL_STATE) state <- mkReg(CONTROL_STATE_idle);

    // The current FPGA clock cycle
    Reg#(Bit#(64)) fpga_cycle <- mkReg(minBound);
  
    // The current model cycle
    Reg#(Bit#(64)) curModelCycle <- mkReg(minBound);

    // Heartbeat trigger bit
    Reg#(Bit#(1)) heartbeatTrigger <- mkReg(0);

    // === rules ===

    // Count the current FPGA cycle
    rule tick (True);
        fpga_cycle <= fpga_cycle + 1;
    endrule
  
    // accept Run request from starter
    rule accept_request_Run (state == CONTROL_STATE_idle || state == CONTROL_STATE_paused);
        starter.acceptRequest_Run();
        centralControllers.moduleController.run();
        state <= CONTROL_STATE_running;
    endrule

    // accept Pause request from starter
    rule accept_request_Pause (state == CONTROL_STATE_running);
        starter.acceptRequest_Pause();
        centralControllers.moduleController.pause();
        state <= CONTROL_STATE_paused;
    endrule

    // accept Sync request from starter
    rule accept_request_Sync (state == CONTROL_STATE_paused);
        starter.acceptRequest_Sync();
        centralControllers.moduleController.sync();
        state <= CONTROL_STATE_idle;
    endrule

    // accept DumpStats request from starter
    rule accept_request_DumpStats (state == CONTROL_STATE_idle);
        starter.acceptRequest_DumpStats();
        centralControllers.statsController.doCommand(STATS_Dump);
        state <= CONTROL_STATE_dumping;
    endrule

    // monitor module controller
    rule monitor_module_controller (state == CONTROL_STATE_running);
        let success = centralControllers.moduleController.queryResult();
        starter.makeRequest_EndSim(success);
        state <= CONTROL_STATE_paused;
    endrule

    // monitor stats controller
    rule sync_model (state == CONTROL_STATE_dumping && centralControllers.statsController.noMoreStats());
        starter.sendResponse_DumpStats();
        state <= CONTROL_STATE_idle;
    endrule

    // Count the model cycle and send heartbeat updates
    rule model_tick (True);
        link_model_cycle.deq();

        curModelCycle <= curModelCycle + 1;
        let trigger = curModelCycle[`HEARTBEAT_TRIGGER_BIT];
        if (trigger != heartbeatTrigger)
        begin
            heartbeatTrigger <= trigger;
            starter.makeRequest_Heartbeat(fpga_cycle, curModelCycle);
        end
    endrule

endmodule
