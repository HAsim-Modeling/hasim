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

//BSV library imports
import PrimArray::*;
import Connectable::*;
import FIFO::*;

//HASim library imports

`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"

`include "asim/rrr/remote_client_stub_COMMANDS.bsh"
`include "asim/rrr/remote_server_stub_COMMANDS.bsh"

`include "asim/dict/RINGID.bsh"
`include "asim/dict/STREAMS.bsh"
`include "asim/dict/STREAMID.bsh"

typedef CONTEXT_ID                             CONTROL_MODEL_CYCLE_MSG;
typedef Tuple2#(CONTEXT_ID, MODEL_NUM_COMMITS) CONTROL_MODEL_COMMIT_MSG;

// Instructions committed this cycle.  The width here must be large enough for
// the commit bandwidth of the largest model.
typedef Bit#(4) MODEL_NUM_COMMITS;


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

    // =========== Submodules ===========
    
    // Stubs to communicate to SW
    let clientStub <- mkClientStub_COMMANDS();
    let serverStub <- mkServerStub_COMMANDS();
  
    // Our way of sending Commands to the Local Controllers
    Connection_Chain#(HASIM_COMMAND)  link_command   <- mkConnection_Chain(`RINGID_MODULE_COMMANDS);
  
    // Our way of receiving Responses from the Local Controllers
    Connection_Chain#(HASIM_RESPONSE) link_response <- mkConnection_Chain(`RINGID_MODULE_RESPONSES);

    Connection_Send#(STREAMS_REQUEST) link_streams <- mkConnection_Send("vdev_streams");

    // The timing model must tell us the current model cycle.  By convention,
    // it is the token request stage at the head of the pipeline.
    Connection_Receive#(CONTROL_MODEL_CYCLE_MSG)  link_model_cycle <- mkConnection_Receive("model_cycle");
    Connection_Receive#(CONTROL_MODEL_COMMIT_MSG) link_model_commit <- mkConnection_Receive("model_commits");

    // Model cycles since last heartbeat message sent to software
    LUTRAM#(CONTEXT_ID, HEARTBEAT_MODEL_CYCLES) curModelCycle <- mkLUTRAM(0);

    // Committed instructions since last heartbeat message sent to software.
    // If Bit#(32) isn't big enough the heartbeat isn't being sent often enough.
    LUTRAM#(CONTEXT_ID, Bit#(32)) instrCommits <- mkLUTRAM(0);

    // *********** Rules ***********

    // tick

    // Count the current FPGA cycle
    rule tick (True);
        curTick <= curTick + 1;
    endrule
  

    // finishCommands

    // As the end of the Command chain, we dequeue Commands when
    // they make their way back to us and send an ACK back to the host.
    rule finishCommands (True);
        let cmd <- link_command.receive_from_prev();
        case (cmd)
            COM_RunProgram: serverStub.sendResponse_Run(?);
            COM_Pause:      serverStub.sendResponse_Pause(?);
        endcase
    endrule


    // getResponse

    // Get Responses from the Local Controllers, including when the program ends.
    rule getResponse (True);
        let resp <- link_response.receive_from_prev();

        case (resp) matches
            tagged RESP_DoneRunning .pf: // Program's done
            begin
                if (pf)  // It passed
                begin
                    link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MESSAGE,
                                                        stringID: `STREAMS_MESSAGE_SUCCESS,
                                                        payload0: truncate(curTick),
                                                        payload1: ? });
                    passed <= True;
                end
                else  // It failed
                begin
                    link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MESSAGE,
                                                        stringID: `STREAMS_MESSAGE_FAILURE,
                                                        payload0: truncate(curTick),
                                                        payload1: ? });
                end
                // Either way we are done
                state <= CON_Finished;
            end

            default: // Unexpected Response
            begin
                link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MESSAGE,
                                                    stringID: `STREAMS_MESSAGE_ERROR,
                                                    payload0: truncate(curTick),
                                                    payload1: zeroExtend(pack(resp)) });
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
        link_command.send_to_next(tagged COM_EnableContext truncate(ctx_id));
    endrule


    rule disableContext (True);
        let ctx_id <- serverStub.acceptRequest_DisableContext();
        link_command.send_to_next(tagged COM_DisableContext truncate(ctx_id));
    endrule


    // run: begin/continue simulation when the main controller tells us to
    rule run (state == CON_Init);
        let dummy <- serverStub.acceptRequest_Run();

        link_command.send_to_next(COM_RunProgram);

        state <= CON_Running;

        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MESSAGE,
                                            stringID: `STREAMS_MESSAGE_START,
                                            payload0: truncate(curTick), // Program Started
                                            payload1: ? });
    endrule


    // pause: pause simulation
    rule pause (True);
        let dummy <- serverStub.acceptRequest_Pause();
        // To Do: Sync and quiesce.  For now just sends a pause request.
        link_command.send_to_next(COM_Pause);
    endrule


    // sync: sync ports and events
    (* descending_urgency = "sync, pause, disableContext, enableContext, run" *)
    rule sync (True);
        let dummy <- serverStub.acceptRequest_Sync();
        serverStub.sendResponse_Sync(?);
    endrule


    // Count the model cycle and send heartbeat updates
    (* descending_urgency = "modelTick, finishRun" *)
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
