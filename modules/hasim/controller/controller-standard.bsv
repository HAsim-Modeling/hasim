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
`include "asim/provides/fpga_components.bsh"

`include "asim/provides/central_controllers.bsh"
`include "asim/provides/module_controller.bsh"
`include "asim/provides/events_controller.bsh"
`include "asim/provides/stats_controller.bsh"
`include "asim/provides/debug_scan_controller.bsh"
`include "asim/provides/params_controller.bsh"
`include "asim/provides/assertions_controller.bsh"
`include "asim/provides/starter.bsh"

typedef CONTEXT_ID                             CONTROL_MODEL_CYCLE_MSG;
typedef Tuple2#(CONTEXT_ID, MODEL_NUM_COMMITS) CONTROL_MODEL_COMMIT_MSG;

// control state
typedef enum
{
    CONTROL_STATE_idle,       // simulation halted, modules are sync'ed
    CONTROL_STATE_running,    // simulation running
    CONTROL_STATE_paused      // simulation halted, modules may not be sync'ed
}
CONTROL_STATE
    deriving (Bits, Eq);


// Instructions committed this cycle.  The width here must be large enough for
// the commit bandwidth of the largest model.
typedef Bit#(4) MODEL_NUM_COMMITS;


typedef Bit#(TAdd#(`HEARTBEAT_TRIGGER_BIT, 1)) HEARTBEAT_MODEL_CYCLES;


// ================ Standard Controller ===============

module [HASIM_MODULE] mkController ();

    TIMEP_DEBUG_FILE_MULTICTX debugLog <- mkTIMEPDebugFile_MultiCtx("controller.out");

    // instantiate all the sub-controllers
    CENTRAL_CONTROLLERS centralControllers <- mkCentralControllers();

    // instantiate starter
    Starter starter <- mkStarter();

    // The timing model must tell us the current model cycle.  By convention,
    // it is the token request stage at the head of the pipeline.
    Connection_Receive#(CONTROL_MODEL_CYCLE_MSG) link_model_cycle <- mkConnection_Receive("model_cycle");
    Connection_Receive#(CONTROL_MODEL_COMMIT_MSG) link_model_commit <- mkConnection_Receive("model_commits");

    // Link to the starter virtual device
    Connection_Send#(Bit#(8)) link_starter <- mkConnection_Send("vdev_starter_finish_run");

    // state
    Reg#(CONTROL_STATE) state <- mkReg(CONTROL_STATE_idle);

    // The current FPGA clock cycle
    Reg#(Bit#(64)) fpgaCycle <- mkReg(minBound);
  
    // Model cycles since last heartbeat message sent to software
    LUTRAM#(CONTEXT_ID, HEARTBEAT_MODEL_CYCLES) curModelCycle <- mkLUTRAM(0);

    // Committed instructions since last heartbeat message sent to software.
    // If Bit#(32) isn't big enough the heartbeat isn't being sent often enough.
    LUTRAM#(CONTEXT_ID, Bit#(32)) instrCommits <- mkLUTRAM(0);

    // In the middle of dumping statistics or a debug scan
    Reg#(Bool) dumpingStats <- mkReg(False);
    Reg#(Bool) debugScanActive <- mkReg(False);


    // === rules ===

    // Count the current FPGA cycle
    rule tick (True);
        fpgaCycle <= fpgaCycle + 1;
    endrule
  
    // accept Run request from starter
    rule acceptRequestRun (state == CONTROL_STATE_idle || state == CONTROL_STATE_paused);
        starter.acceptRequest_Run();
        centralControllers.moduleController.run();
        state <= CONTROL_STATE_running;
        debugLog.record_all($format("RUN"));
    endrule

    // accept Pause request from starter
    rule acceptRequestPause (state == CONTROL_STATE_running);
        starter.acceptRequest_Pause();
        centralControllers.moduleController.pause();
        state <= CONTROL_STATE_paused;
        debugLog.record_all($format("PAUSE"));
    endrule

    // accept Sync request from starter
    rule acceptRequestSync (state == CONTROL_STATE_paused);
        starter.acceptRequest_Sync();
        centralControllers.moduleController.sync();
        state <= CONTROL_STATE_idle;
        debugLog.record_all($format("IDLE"));
    endrule

    // monitor module controller
    rule monitorModuleController (state == CONTROL_STATE_running);
        let success = centralControllers.moduleController.queryResult();
        starter.makeRequest_EndSim(success);
        // link_starter.send(zeroExtend(pack(!success)));
        state <= CONTROL_STATE_paused;
    endrule

    // accept DumpStats request from starter
    rule acceptRequestDumpStats (! dumpingStats);
        starter.acceptRequest_DumpStats();
        centralControllers.statsController.doCommand(STATS_Dump);
        dumpingStats <= True;
        debugLog.record_all($format("STATS_DUMP Start"));
    endrule

    // monitor stats controller
    rule syncModel (dumpingStats && centralControllers.statsController.noMoreStats());
        starter.sendResponse_DumpStats();
        dumpingStats <= False;
    endrule

    // accept DebugScan request from starter
    rule acceptRequestDebugScan (! debugScanActive);
        starter.acceptRequest_DebugScan();
        centralControllers.debugScanController.scanStart();
        debugScanActive <= True;
        debugLog.record_all($format("DEBUG_SCAN Start"));
    endrule

    // monitor stats controller
    (* descending_urgency = "completeDebugScan, syncModel" *)
    rule completeDebugScan (debugScanActive && centralControllers.debugScanController.scanIsDone());
        starter.sendResponse_DebugScan();
        debugScanActive <= False;
    endrule

    // monitor requests to enable contexts
    rule acceptRequestEnableContext (True);
        let ctx_id <- starter.acceptRequest_EnableContext();
        centralControllers.moduleController.enableContext(ctx_id);

        debugLog.record(ctx_id, $format("ENABLE Context"));
    endrule

    // monitor requests to disable contexts
    (* descending_urgency = "monitorModuleController, acceptRequestSync, acceptRequestPause, acceptRequestDisableContext, acceptRequestEnableContext, acceptRequestRun" *)
    rule acceptRequestDisableContext (True);
        let ctx_id <- starter.acceptRequest_DisableContext();
        centralControllers.moduleController.disableContext(ctx_id);

        debugLog.record(ctx_id, $format("DISABLE Context"));
    endrule


    // Count the model cycle and send heartbeat updates
    rule modelTick (True);
        CONTEXT_ID ctx_id = link_model_cycle.receive();
        link_model_cycle.deq();

        debugLog.nextModelCycle(ctx_id);

        let cur_cycle = curModelCycle.sub(ctx_id);

        let trigger = cur_cycle[`HEARTBEAT_TRIGGER_BIT];
        if (trigger == 1)
        begin
            starter.makeRequest_Heartbeat(ctx_id, fpgaCycle, zeroExtend(cur_cycle), instrCommits.sub(ctx_id));
            curModelCycle.upd(ctx_id, 1);
            instrCommits.upd(ctx_id, 0);
        end
        else
        begin
            curModelCycle.upd(ctx_id, cur_cycle + 1);
        end
    endrule

    // Trigger heartbeat on FPGA cycle too, in case of model deadlock
    let fpgaClockTriggerBit = `HEARTBEAT_TRIGGER_BIT + 10;
    Reg#(Bit#(1)) lastFPGATrigger <- mkReg(0);

    rule fpgaClockHeartbeat (fpgaCycle[fpgaClockTriggerBit] != lastFPGATrigger);
        starter.makeRequest_FPGAHeartbeat(fpgaCycle);
        lastFPGATrigger <= fpgaCycle[fpgaClockTriggerBit];
    endrule


    //
    // Monitor committed instructions.
    //
    (* descending_urgency = "fpgaClockHeartbeat, modelCommits, modelTick, monitorModuleController" *)
    rule modelCommits (True);
        match { .ctx_id, .commits } = link_model_commit.receive();
        link_model_commit.deq();

        let cur_commits = instrCommits.sub(ctx_id);
        instrCommits.upd(ctx_id, cur_commits + zeroExtend(commits));

        debugLog.record(ctx_id, $format("COMMIT %0d", commits));
    endrule

endmodule
