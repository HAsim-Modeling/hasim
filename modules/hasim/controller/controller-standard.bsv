`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"

`include "asim/provides/central_controllers.bsh"
`include "asim/provides/module_controller.bsh"
`include "asim/provides/events_controller.bsh"
`include "asim/provides/stats_controller.bsh"
`include "asim/provides/params_controller.bsh"
`include "asim/provides/assertions_controller.bsh"
`include "asim/provides/starter.bsh"

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


// Instructions committed this cycle.  The width here must be large enough for
// the commit bandwidth of the largest model.
typedef Bit#(4) MODEL_NUM_COMMITS;


typedef Bit#(TAdd#(`HEARTBEAT_TRIGGER_BIT, 1)) HEARTBEAT_MODEL_CYCLES;


// ================ Standard Controller ===============

module [HASIM_MODULE] mkController ();

    // instantiate all the sub-controllers
    CENTRAL_CONTROLLERS centralControllers <- mkCentralControllers();

    // instantiate starter
    Starter starter <- mkStarter();

    // The timing model must tell us the current model cycle.  By convention,
    // it is the token request stage at the head of the pipeline.
    Connection_Receive#(Bool) link_model_cycle <- mkConnection_Receive("model_cycle");

    Connection_Receive#(MODEL_NUM_COMMITS) link_model_commit <- mkConnection_Receive("model_commits");

    // state
    Reg#(CONTROL_STATE) state <- mkReg(CONTROL_STATE_idle);

    // The current FPGA clock cycle
    Reg#(Bit#(64)) fpga_cycle <- mkReg(minBound);
  
    // Model cycles since last heartbeat message sent to software
    Reg#(HEARTBEAT_MODEL_CYCLES) curModelCycle <- mkReg(0);

    // Committed instructions since last heartbeat message sent to software.
    // If Bit#(32) isn't big enough the heartbeat isn't being sent often enough.
    Reg#(Bit#(32)) instrCommits <- mkReg(0);

    // In the middle of dumping statistics?
    Reg#(Bool) dumpingStats <- mkReg(False);

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

    // monitor module controller
    rule monitor_module_controller (state == CONTROL_STATE_running);
        let success = centralControllers.moduleController.queryResult();
        starter.makeRequest_EndSim(success);
        state <= CONTROL_STATE_paused;
    endrule

    // accept DumpStats request from starter
    rule accept_request_DumpStats (! dumpingStats);
        starter.acceptRequest_DumpStats();
        centralControllers.statsController.doCommand(STATS_Dump);
        dumpingStats <= True;
    endrule

    // monitor stats controller
    rule sync_model (dumpingStats && centralControllers.statsController.noMoreStats());
        starter.sendResponse_DumpStats();
        dumpingStats <= False;
    endrule

    (* descending_urgency = "model_commits, model_tick" *)

    // Count the model cycle and send heartbeat updates
    rule model_tick (True);
        link_model_cycle.deq();

        let trigger = curModelCycle[`HEARTBEAT_TRIGGER_BIT];
        if (trigger == 1)
        begin
            starter.makeRequest_Heartbeat(fpga_cycle, zeroExtend(curModelCycle), instrCommits);
            curModelCycle <= 1;
            instrCommits <= 0;
        end
        else
        begin
            curModelCycle <= curModelCycle + 1;
        end
    endrule

    rule model_commits (True);
        Bit#(32) commits = zeroExtend(link_model_commit.receive());
        link_model_commit.deq();

        instrCommits <= instrCommits + commits;
    endrule

endmodule
