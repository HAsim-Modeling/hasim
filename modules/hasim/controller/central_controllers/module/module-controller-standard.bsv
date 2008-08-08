
//BSV library imports
import PrimArray::*;
import RegFile::*;
import Connectable::*;
import FIFO::*;

//HASim library imports
import hasim_common::*;
import soft_connections::*;
import platform_interface::*;
import hasim_modellib::*;
import front_panel::*;
import module_local_controller::*;

`include "starter.bsh"
`include "streams.bsh"
`include "asim/dict/RINGID.bsh"
`include "asim/dict/STREAMS.bsh"
`include "asim/dict/STREAMID.bsh"

// ************* Module Controller **************

// ConState

// The internal state of the Controller

typedef enum
{
    CON_Init,      // Initializing and doing local bookkeeping.
    CON_Running,   // Running the program, waiting for termination.
    CON_Finished   // Program has finished. We allow some extra time for Event dumping.
}
ConState
           deriving (Eq, Bits);

// MODULE_CONTROLLER

interface MODULE_CONTROLLER;

    method Action run();
    method Action pause();
    method Action sync();
    method Bool   queryResult();

endinterface

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
    Reg#(ConState) state <- mkReg(CON_Init);

    // =========== Submodules ===========
  
    // Our way of sending Commands to the Local Controllers
    Connection_Chain#(Command)   link_command   <- mkConnection_Chain(`RINGID_MODULE_COMMANDS);
  
    // Our way of receiving Responses from the Local Controllers
    Connection_Chain#(Response)  link_response  <- mkConnection_Chain(`RINGID_MODULE_RESPONSES);
  
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
  
    let cmd <- link_command.receive_from_prev();
  
  endrule
  
  
  // getResponse
  
  // Get Responses from the Local Controllers, including when the program ends.
  
  rule getResponse (state == CON_Running);
  
    let resp <- link_response.receive_from_prev();

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
  
  // run: begin/continue simulation when the main controller tells us to
  // TEMPORARY: we only start running from CON_Init state
  method Action run() if (state == CON_Init);
  
     link_command.send_to_next(COM_RunProgram);
     
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
