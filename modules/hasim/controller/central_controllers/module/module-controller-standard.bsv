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

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/hasim_modellib.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/platform_interface.bsh"
`include "asim/provides/front_panel.bsh"
`include "asim/provides/module_local_controller.bsh"

`include "asim/provides/starter.bsh"
`include "asim/provides/streams.bsh"
`include "asim/dict/RINGID.bsh"
`include "asim/dict/STREAMS.bsh"
`include "asim/dict/STREAMID.bsh"

// ************* Module Controller **************

// MODULE_CONTROLLER

interface MODULE_CONTROLLER;

    method Action enableContext(CONTEXT_ID ctx_id);
    method Action disableContext(CONTEXT_ID ctx_id);

    method Action run();
    method Action pause();
    method Action sync();
    method Bool   queryResult();

endinterface

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

// mkModuleController
module [HASIM_MODULE] mkModuleController#(Connection_Send#(STREAMS_REQUEST) link_streams)
    // interface:
        (MODULE_CONTROLLER);

    // *********** State ***********
  
    // The current FPGA clock cycle
    Reg#(Bit#(64)) curTick <- mkReg(minBound);
  
    // When the program ends we allow some extra time to finish dumping Events
    // If the Events Controller were a bit smarter we wouldn't need this.
    // Also there's no real guarantee that all events have been dumped.
    Reg#(Bit#(16)) finishing <- mkReg(`HASIM_CONTROLLER_COOLDOWN);
  
    // Did the testcase pass?
    Reg#(Bool)     passed <- mkReg(False);

    // Track our internal state
    Reg#(CON_STATE) state <- mkReg(CON_Init);

    // =========== Submodules ===========
  
    // Our way of sending Commands to the Local Controllers
    Connection_Chain#(CONTROLLER_COMMAND) link_command   <- mkConnection_Chain(`RINGID_MODULE_COMMANDS);
  
    // Our way of receiving Responses from the Local Controllers
    Connection_Chain#(CONTROLLER_RESPONSE) link_response <- mkConnection_Chain(`RINGID_MODULE_RESPONSES);
  
    // We write our state to the LEDs, but ignore the switches and buttons.
    Connection_Send#(FRONTP_MASKED_LEDS) link_leds <- mkConnection_Send("fpga_leds");
    Connection_Receive#(FRONTP_SWITCHES) link_switches <- mkConnection_Receive("fpga_switches");
    Connection_Receive#(ButtonInfo)      link_buttons <- mkConnection_Receive("fpga_buttons");

    // *********** Rules ***********

    // tick

    // Count the current FPGA cycle
    rule tick (True);
        curTick <= curTick + 1;
    endrule
  

    // finishCommands

    // As the end of the Command chain, we simply dequeue Commands when
    // they make their way back to us.
    rule finishCommands (True);
        let cmd <- link_command.recvFromPrev();
    endrule


    // getResponse

    // Get Responses from the Local Controllers, including when the program ends.
    rule getResponse (state == CON_Running);
        let resp <- link_response.recvFromPrev();

        case (resp) matches
            tagged RESP_DoneRunning .pf: // Program's done
            begin
                if (pf)  // It passed
                begin
                    link_leds.send(FRONTP_MASKED_LEDS {state: zeroExtend(4'b1001), mask: zeroExtend(4'b1111)});
                    link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MESSAGE,
                                                        stringID: `STREAMS_MESSAGE_SUCCESS,
                                                        payload0: truncate(curTick),
                                                        payload1: ? });
                    passed <= True;
                end
                else  // It failed
                begin
                    link_leds.send(FRONTP_MASKED_LEDS {state: zeroExtend(4'b1101), mask: zeroExtend(4'b1111)});
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


    // finishUp: count down some extra time for the events controller to dump stuff
    rule finishUp (state == CON_Finished && finishing != 0);
        finishing <= finishing - 1;
    endrule


    method Action enableContext(CONTEXT_ID ctx_id);
        link_command.sendToNext(tagged COM_EnableContext ctx_id);
    endmethod


    method Action disableContext(CONTEXT_ID ctx_id);
        link_command.sendToNext(tagged COM_DisableContext ctx_id);
    endmethod


    // run: begin/continue simulation when the main controller tells us to
    // TEMPORARY: we only start running from CON_Init state
    method Action run() if (state == CON_Init);
        link_command.sendToNext(COM_RunProgram);

        state <= CON_Running;
        link_leds.send(FRONTP_MASKED_LEDS {state: zeroExtend(4'b0011), mask: zeroExtend(4'b1111)});

        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_MESSAGE,
                                            stringID: `STREAMS_MESSAGE_START,
                                            payload0: truncate(curTick), // Program Started
                                            payload1: ? });
    endmethod


    // pause: pause simulation
    method Action pause() if (state == CON_Running);
        noAction;
    endmethod


    // sync: sync ports and events
    method Action sync();
        noAction;
    endmethod


    // queryResult: tell the main controller that the simulation is over by ready-ing
    // the method, and return success or failure
    method Bool queryResult() if (state == CON_Finished && finishing == 0);
        return passed;
    endmethod

endmodule
