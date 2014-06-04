//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

//BSV library imports
import Connectable::*;
import FIFO::*;

//HASim library imports

`include "asim/provides/fpga_components.bsh"
`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/soft_services.bsh"
`include "asim/provides/soft_services_lib.bsh"
`include "asim/provides/soft_services_deps.bsh"
`include "asim/provides/common_services.bsh"

`include "asim/rrr/remote_client_stub_COMMANDS.bsh"
`include "asim/rrr/remote_server_stub_COMMANDS.bsh"


typedef Bit#(TAdd#(`HEARTBEAT_TRIGGER_BIT, 1)) HEARTBEAT_MODEL_CYCLES;

// CON_STATE

// The internal state of the Controller

typedef enum
{
    CON_Init,      // Initializing and doing local bookkeeping.
    CON_Running,   // Running the program, waiting for termination.
    CON_Finished   // Program has finished. We allow some extra time for Event dumping.
}
CON_STATE
    deriving (Eq, Bits);

// mkCommandServices

module [HASIM_MODULE] mkCommandsService
    // interface:
        ();

    // *********** State ***********
  
    // The current FPGA clock cycle
    Reg#(Bit#(64)) curTick <- mkReg(minBound);
  
    // When the program ends we allow some extra time to finish dumping Events
    // If the Events Controller were a bit smarter we wouldn't need this.
    // Also there's no real guarantee that all events have been dumped.
    Reg#(Bit#(16)) finishing <- mkReg(`MODEL_COOLDOWN);
  
    // Did the testcase pass?
    Reg#(Bool)     passed <- mkReg(False);

    // Track our internal state
    Reg#(CON_STATE) state <- mkReg(CON_Init);

    // End model cycle (set by software, tests context 0's model cycle counter).
    Reg#(Maybe#(Bit#(64))) endModelCycle <- mkReg(tagged Invalid);

    // =========== Submodules ===========
    
    // Stubs to communicate to SW
    let clientStub <- mkClientStub_COMMANDS();
    let serverStub <- mkServerStub_COMMANDS();
  
    // Our way of communicating with the local controllers
    CONNECTION_CHAIN#(CONTROLLER_MSG) link_controllers <- mkConnectionChain("CONTROLLER_MESSAGES");

    // The timing model must tell us the current model cycle.  By convention,
    // it is the token request stage at the head of the pipeline.
    Connection_Receive#(CONTROL_MODEL_CYCLE_MSG)  link_model_cycle <- mkConnection_Receive("model_cycle");
    Connection_Receive#(CONTROL_MODEL_COMMIT_MSG) link_model_commit <- mkConnection_Receive("model_commits");

    // Model cycles since last heartbeat message sent to software
    LUTRAM#(CONTEXT_ID, HEARTBEAT_MODEL_CYCLES) curModelCycle <- mkLUTRAM(0);

    // Committed instructions since last heartbeat message sent to software.
    // If Bit#(32) isn't big enough the heartbeat isn't being sent often enough.
    LUTRAM#(CONTEXT_ID, Bit#(32)) instrCommits <- mkLUTRAM(0);

    STDIO#(Bit#(32)) stdio <- mkStdIO();
    let msgStart <- getGlobalStringUID("[%13u]: commands relay: program started.\n");
    let msgDone <- getGlobalStringUID("[%13u]: commands relay: test program finished successfully.\n");
    let msgErr <- getGlobalStringUID("[%13u]: commands relay: test program finished, one or more failures occurred.\n");


    // *********** Rules ***********

    // tick
    //
    // Count the current FPGA cycle.
    //
    rule tick (True);
        curTick <= curTick + 1;
    endrule

    //
    // fpgaHeartbeat --
    //     Monitor ticks and send a hardware-only heartbeat, used to detect
    //     model deadlocks.
    //
    Reg#(Bit#(1)) fpgaHeartbeatState <- mkReg(0);

    rule fpgaHeartbeat (fpgaHeartbeatState != curTick[`FPGA_HEARTBEAT_TRIGGER_BIT]);
        fpgaHeartbeatState <= curTick[`FPGA_HEARTBEAT_TRIGGER_BIT];
        clientStub.makeRequest_FPGAHeartbeat(?);
    endrule
  

    // getMessage

    // Get Responses from the Local Controllers, including when the program ends.
    rule getMessage (True);
        let msg <- link_controllers.recvFromPrev();

        case (msg) matches
            tagged COM_RunProgram: serverStub.sendResponse_Run(?);

            tagged COM_Pause .send_resp:
            begin
                if (send_resp)
                begin
                    serverStub.sendResponse_Pause(?);
                end
            end

            tagged COM_Resume .send_resp:
            begin
                if (send_resp)
                begin
                    serverStub.sendResponse_Resume(?);
                end
            end

            tagged COM_Synchronize: serverStub.sendResponse_Sync(?);

            tagged COM_SyncQuery .all_balanced:
            begin
                serverStub.sendResponse_IsSynced(zeroExtend(pack(all_balanced)));
            end

            tagged LC_DoneRunning .pf: // Program's done
            begin
                if (pf)  // It passed
                begin
                    stdio.printf(msgDone, list1(truncate(curTick)));
                    passed <= True;
                end
                else  // It failed
                begin
                    stdio.printf(msgErr, list1(truncate(curTick)));
                end
                // Either way we are done
                state <= CON_Finished;
            end

            tagged COM_Scan:
            begin
                // Sending "done" guarantees all scan data have reached host
                clientStub.makeRequest_ScanDone(0);
            end

            tagged LC_ScanData .sd:
            begin
                clientStub.makeRequest_ScanData(sd, 0);
            end

            tagged LC_ScanDataLast .sd:
            begin
                clientStub.makeRequest_ScanData(sd, 1);
            end

            tagged LC_ScanRunning .sr:
            begin
                clientStub.makeRequest_ScanData(zeroExtend(pack(sr)), 'b10);
            end

            tagged LC_ScanRunningLast .sr:
            begin
                clientStub.makeRequest_ScanData(zeroExtend(pack(sr)), 'b11);
            end

            tagged COM_TestThroughput:
            begin
                // Sending "done" guarantees all data have reached host
                clientStub.makeRequest_ScanDone(1);
            end

            tagged LC_ThroughputData .cycles:
            begin
                clientStub.makeRequest_ThroughputData(cycles, 0);
            end

            tagged LC_ThroughputLast:
            begin
                clientStub.makeRequest_ThroughputData(?, 1);
            end

            default:
            begin
                // Sink most messages
                noAction;
            end
        endcase
    endrule


    // finishUp: count down some extra time for the events services to dump stuff
    rule finishUp (state == CON_Finished && finishing != 0);
        finishing <= finishing - 1;
    endrule


    rule finishRun (state == CON_Finished && finishing == 0);
        clientStub.makeRequest_EndSim(zeroExtend(pack(passed)));
        state <= CON_Init;
        passed <= False;
        finishing <= `MODEL_COOLDOWN;
    endrule


    rule enableContext (True);
        let ctx_id <- serverStub.acceptRequest_EnableContext();
        link_controllers.sendToNext(tagged COM_EnableContext truncate(ctx_id));
    endrule


    rule disableContext (True);
        let ctx_id <- serverStub.acceptRequest_DisableContext();
        link_controllers.sendToNext(tagged COM_DisableContext truncate(ctx_id));
    endrule


    rule setEndModelCycle (True);
        let cycle <- serverStub.acceptRequest_SetEndModelCycle();
        endModelCycle <= tagged Valid cycle;
    endrule


    rule requestScan (True);
        let dummy <- serverStub.acceptRequest_Scan();
        link_controllers.sendToNext(tagged COM_Scan);
    endrule


    rule requestTestThroughput (True);
        let dummy <- serverStub.acceptRequest_TestThroughput();
        link_controllers.sendToNext(tagged COM_TestThroughput);
    endrule


    // run: begin/continue simulation when the main controller tells us to
    rule run (state == CON_Init);
        let dummy <- serverStub.acceptRequest_Run();

        link_controllers.sendToNext(COM_RunProgram);

        state <= CON_Running;

        // Program Started
        stdio.printf(msgStart, list1(truncate(curTick)));
    endrule


    // pause: pause simulation
    rule pause (True);
        let dummy <- serverStub.acceptRequest_Pause();
        // To Do: Sync and quiesce.  For now just sends a pause request.
        link_controllers.sendToNext(tagged COM_Pause True);
    endrule


    // resume: resume simulation
    rule resume (True);
        let dummy <- serverStub.acceptRequest_Resume();
        // To Do: Sync and quiesce.  For now just sends a resume request.
        link_controllers.sendToNext(tagged COM_Resume True);
    endrule


    // sync: sync ports and events
    rule sync (True);
        let dummy <- serverStub.acceptRequest_Sync();
        link_controllers.sendToNext(tagged COM_Synchronize);
    endrule

    (* descending_urgency = "sync, isSynced" *)
    rule isSynced (True);
        let dummy <- serverStub.acceptRequest_IsSynced();
        link_controllers.sendToNext(tagged COM_SyncQuery True);
    endrule


    // ====================================================================
    //
    // Count the model cycle and send heartbeat updates.
    //
    // ====================================================================

    // Context 0 is tested for the global end model cycle, which may be requested
    // by software.  Using a single 64 bit counter instead of making all model
    // cycle counters 64 bits saves space with large numbers of contexts.
    // Initializing to 1 makes the end model cycle comparison easier.
    Reg#(Bit#(64)) ctx0ModelCycles <- mkReg(0);

    (* descending_urgency = "getMessage, checkSimEnd, sync, isSynced, pause, resume, setEndModelCycle, disableContext, enableContext, requestScan, requestTestThroughput, run" *)
    rule checkSimEnd (state == CON_Running &&&
                      endModelCycle matches tagged Valid .end_cycle &&&
                      ctx0ModelCycles >= end_cycle);
        // Stop simulation
        link_controllers.sendToNext(tagged COM_Pause False);

        // Tell software we're done
        stdio.printf(msgDone, list1(truncate(curTick)));
        passed <= True;
        state <= CON_Finished;
    endrule


    (* descending_urgency = "finishRun, fpgaHeartbeat, modelTick, getMessage" *)
    rule modelTick (True);
        CONTEXT_ID ctx_id = link_model_cycle.receive();
        link_model_cycle.deq();

        let cur_cycle = curModelCycle.sub(ctx_id);

        let trigger = cur_cycle[`HEARTBEAT_TRIGGER_BIT];
        if (trigger == 1)
        begin
            clientStub.makeRequest_ModelHeartbeat(zeroExtend(ctx_id), curTick, zeroExtend(cur_cycle), instrCommits.sub(ctx_id));
            curModelCycle.upd(ctx_id, 1);
            instrCommits.upd(ctx_id, 0);
        end
        else
        begin
            curModelCycle.upd(ctx_id, cur_cycle + 1);
        end

        //
        // Context 0 is used to trigger the end of simulation if a cycle limit
        // is set.
        //
        if (ctx_id == 0)
        begin
            ctx0ModelCycles <= ctx0ModelCycles + 1;
        end
    endrule

    //
    // Monitor committed instructions.
    //
    (* descending_urgency = "modelCommits, modelTick" *)
    rule modelCommits (True);
        match { .ctx_id, .commits } = link_model_commit.receive();
        link_model_commit.deq();

        let cur_commits = instrCommits.sub(ctx_id);
        instrCommits.upd(ctx_id, cur_commits + zeroExtend(commits));
    endrule

endmodule
