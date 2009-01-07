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

import Vector::*;

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"

`include "asim/provides/central_controllers.bsh"
`include "asim/provides/module_controller.bsh"
`include "asim/provides/events_controller.bsh"
`include "asim/provides/stats_controller.bsh"
`include "asim/provides/params_controller.bsh"
`include "asim/provides/assertions_controller.bsh"
`include "asim/provides/starter.bsh"

typedef CONTEXT_ID                             CONTROL_MODEL_CYCLE_MSG;
typedef Tuple2#(CONTEXT_ID, MODEL_NUM_COMMITS) CONTROL_MODEL_COMMIT_MSG;

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

    TIMEP_DEBUG_FILE debugLog <- mkTIMEPDebugFile("controller.out");

    // instantiate all the sub-controllers
    CENTRAL_CONTROLLERS centralControllers <- mkCentralControllers();

    // instantiate starter
    Starter starter <- mkStarter();

    // The timing model must tell us the current model cycle.  By convention,
    // it is the token request stage at the head of the pipeline.
    Connection_Receive#(CONTROL_MODEL_CYCLE_MSG) link_model_cycle <- mkConnection_Receive("model_cycle");
    Connection_Receive#(CONTROL_MODEL_COMMIT_MSG) link_model_commit <- mkConnection_Receive("model_commits");

    // state
    Reg#(CONTROL_STATE) state <- mkReg(CONTROL_STATE_idle);

    // The current FPGA clock cycle
    Reg#(Bit#(64)) fpgaCycle <- mkReg(minBound);
  
    // Model cycles since last heartbeat message sent to software
    Vector#(NUM_CONTEXTS, Reg#(HEARTBEAT_MODEL_CYCLES)) curModelCycle = newVector();

    // Committed instructions since last heartbeat message sent to software.
    // If Bit#(32) isn't big enough the heartbeat isn't being sent often enough.
    Vector#(NUM_CONTEXTS, Reg#(Bit#(32))) instrCommits = newVector();

    for (Integer c = 0; c < valueOf(NUM_CONTEXTS); c = c + 1)
    begin
        curModelCycle[c] <- mkReg(0);
        instrCommits[c] <- mkReg(0);
    end

    // In the middle of dumping statistics?
    Reg#(Bool) dumpingStats <- mkReg(False);

    // === rules ===

    // Count the current FPGA cycle
    rule tick (True);
        fpgaCycle <= fpgaCycle + 1;
    endrule
  
    // accept Run request from starter
    rule accept_request_Run (state == CONTROL_STATE_idle || state == CONTROL_STATE_paused);
        starter.acceptRequest_Run();
        centralControllers.moduleController.run();
        state <= CONTROL_STATE_running;
        debugLog.record($format("RUN"));
    endrule

    // accept Pause request from starter
    rule accept_request_Pause (state == CONTROL_STATE_running);
        starter.acceptRequest_Pause();
        centralControllers.moduleController.pause();
        state <= CONTROL_STATE_paused;
        debugLog.record($format("PAUSE"));
    endrule

    // accept Sync request from starter
    rule accept_request_Sync (state == CONTROL_STATE_paused);
        starter.acceptRequest_Sync();
        centralControllers.moduleController.sync();
        state <= CONTROL_STATE_idle;
        debugLog.record($format("IDLE"));
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
        debugLog.record($format("STATS_DUMP Start"));
    endrule

    // monitor stats controller
    rule sync_model (dumpingStats && centralControllers.statsController.noMoreStats());
        starter.sendResponse_DumpStats();
        dumpingStats <= False;
    endrule

    // monitor requests to enable contexts
    rule accept_request_EnableContext (True);
        let ctx_id <- starter.acceptRequest_EnableContext();
        centralControllers.moduleController.enableContext(ctx_id);

        debugLog.record($format("ENABLE Context %0d", ctx_id));
    endrule

    // monitor requests to disable contexts
    rule accept_request_DisableContext (True);
        let ctx_id <- starter.acceptRequest_DisableContext();
        centralControllers.moduleController.disableContext(ctx_id);

        debugLog.record($format("DISABLE Context %0d", ctx_id));
    endrule

    (* descending_urgency = "model_commits, model_tick" *)

    // Count the model cycle and send heartbeat updates
    rule model_tick (True);
        CONTEXT_ID ctx_id = link_model_cycle.receive();
        link_model_cycle.deq();

        debugLog.record($format("CTX %0d new cycle", ctx_id));

        // Not strictly correct, but approximate...
        if (ctx_id == 0)
        begin
            debugLog.nextModelCycle();
        end

        let trigger = curModelCycle[ctx_id][`HEARTBEAT_TRIGGER_BIT];
        if (trigger == 1)
        begin
            starter.makeRequest_Heartbeat(ctx_id, fpgaCycle, zeroExtend(curModelCycle[ctx_id]), instrCommits[ctx_id]);
            curModelCycle[ctx_id] <= 1;
            instrCommits[ctx_id] <= 0;
        end
        else
        begin
            curModelCycle[ctx_id] <= curModelCycle[ctx_id] + 1;
        end
    endrule

    rule model_commits (True);
        match { .ctx_id, .commits } = link_model_commit.receive();
        link_model_commit.deq();

        instrCommits[ctx_id] <= instrCommits[ctx_id] + zeroExtend(commits);

        debugLog.record($format("COMMIT %0d for CTX %0d, cycle %0d", commits, ctx_id, curModelCycle[ctx_id]));
    endrule

endmodule
