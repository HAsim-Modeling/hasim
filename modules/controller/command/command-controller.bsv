
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
import hasim_local_controller::*;
import hasim_stats_controller::*;
import hasim_events_controller::*;
import hasim_assertions_controller::*;

`include "starter.bsh"
`include "streams.bsh"

// ************* Command Controller **************

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

// CommandController
interface CommandController;
endinterface

// mkCommandController

// Just communicates Events and Stats to the software controller.
// Currently just runs one test and then exits. Doesn't really
// support too many commands.

module [HASim_Module] mkCommandController#(Connection_Send#(STREAMS_REQUEST) link_streams,
                                           EventsController events_controller,
                                           StatsController stats_controller,
                                           AssertionsController asserts_controller)
  // interface:
                (CommandController);

  // *********** State ***********
  
  // The current FPGA clock cycle
  Reg#(Bit#(64)) curTick <- mkReg(minBound);
  
  // The current model clock cycle, according to the Event Controller
  Reg#(Bit#(64)) modelTick <- mkReg(0);
  
  // Every so often we emit a heartbeat just to let the outside world
  // know that the hardware is still alive.
  Reg#(Bit#(64)) beat <- mkReg(0);
  
  // When the program ends we allow some extra time to finish dumping Events
  // If the Events Controller were a bit smarter we wouldn't need this.
  // Also there's no real guarantee that all events have been dumped.
  Reg#(Bit#(16)) finishing <- mkReg(`HASIM_CONTROLLER_COOLDOWN);
  
  // Did the testcase pass?
  Reg#(Bool)     passed <- mkReg(False);

  // Track our internal state
  Reg#(ConState) state <- mkReg(CON_Init);


  // *********** Submodules ***********
  
  // Our way of sending Commands to the Local Controllers
  Connection_Chain#(Command)   link_command   <- mkConnection_Chain(2);
  
  // Our way of receiving Responses from the Local Controllers
  Connection_Chain#(Response)  link_response  <- mkConnection_Chain(3);
  
  // We write our state to the LEDs, but ignore the switches and buttons.
  Connection_Send#(FRONTP_MASKED_LEDS)           link_leds <- mkConnection_Send("fpga_leds");
  Connection_Receive#(FRONTP_SWITCHES)    link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(ButtonInfo)  link_buttons <- mkConnection_Receive("fpga_buttons");

  // Starter
  Starter starter <- mkStarter();
  

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
  
  // runProg
  
  // Begin a run of the program when the software tells us to.
  
  rule runProg (state == CON_Init && starter.getSimState() == 1);
  
     link_command.send_to_next(COM_RunProgram);
     
     state <= CON_Running;
     link_leds.send(FRONTP_MASKED_LEDS {state: zeroExtend(4'b0011), mask: zeroExtend(4'b1111)});

     link_streams.send(STREAMS_REQUEST { streamID: STREAMS_MESSAGE,
                                         stringID: tagged STRINGID_message MESSAGES_START,
                                         payload0: truncate(curTick), // Program Started
                                         payload1: ? });

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
            link_streams.send(STREAMS_REQUEST { streamID: STREAMS_MESSAGE,
                                                stringID: tagged STRINGID_message MESSAGES_SUCCESS,
                                                payload0: truncate(curTick),
                                                payload1: ? });
            passed <= True;
          end
          else  // It failed
          begin
            link_leds.send(FRONTP_MASKED_LEDS {state: zeroExtend(4'b1101), mask: zeroExtend(4'b1111)});
            link_streams.send(STREAMS_REQUEST { streamID: STREAMS_MESSAGE,
                                                stringID: tagged STRINGID_message MESSAGES_FAILURE,
                                                payload0: truncate(curTick),
                                                payload1: ? });
          end
          // Either way we begin dumping.
          stats_controller.doCommand(Stats_Dump);
          state <= CON_Finished;
        end
      default: // Unexpected Response
      begin
        link_streams.send(STREAMS_REQUEST { streamID: STREAMS_MESSAGE,
                                            stringID: tagged STRINGID_message MESSAGES_ERROR,
                                            payload0: truncate(curTick),
                                            payload1: zeroExtend(pack(resp)) });
      end
    endcase

  endrule

  // passOnStat
  
  // Passes a stat from the Stats Controller on to the Software Controller for dumping.

  rule passOnStat (state == CON_Finished && finishing != 0);

    StatInfo si <- stats_controller.getNextStat();

    // TEMPORARY: translate stringID by cheating and looking at the stringIDs
    // produced by hasim-dict. We have to do this because the sub-controller
    // gives us a hand-written raw ID
    DICT_STATS stringID = case (si.statStringID)
                              0: STATS_INSTS_COMMITTED;
                              1: STATS_DCACHE_MISSES;
                              2: STATS_BPRED_MISPREDS;
                              3: STATS_ICACHE_MISSES;
                              4: STATS_INSTS_FETCHED;
                              5: STATS_TOTAL_CYCLES;
                          endcase;

    link_streams.send(STREAMS_REQUEST { streamID: STREAMS_STAT,
                                        stringID: tagged STRINGID_stat stringID,
                                        payload0: si.statValue,
                                        payload1: ? });

  endrule

  // passOnEvent
  
  // Passes an Event on to the Software Controller for dumping.

  rule passOnEvent (True);

    EventInfo ei <- events_controller.getNextEvent();

    // TEMPORARY: translate stringID by cheating and looking at the stringIDs
    // produced by hasim-dict. We have to do this because the sub-controller
    // gives us a hand-written raw ID
    DICT_EVENTS stringID = case (ei.eventStringID)
                               0: EVENTS_FETCH;
                               1: EVENTS_DECODE;
                               2: EVENTS_EXECUTE;
                               3: EVENTS_MEMORY;
                               4: EVENTS_WRITEBACK;
                           endcase;

    link_streams.send(STREAMS_REQUEST { streamID: STREAMS_EVENT,
                                        stringID: tagged STRINGID_event stringID,
                                        payload0: truncate(modelTick),
                                        payload1: pack(ei.eventData) });
    if (ei.eventBoundary == 1)
    begin
        modelTick <= modelTick + 1;
        beat <= beat + 1;
    end

  endrule

  // passOnAssertion
  
  // Passes an Assertion Failure on to the Software Controller for reporting

  rule passOnAssert (True);

    AssertInfo ai <- asserts_controller.getAssertion();

    // TEMPORARY: translate stringID by cheating and looking at the stringIDs
    // produced by hasim-dict. We have to do this because the sub-controller
    // gives us a hand-written raw ID
    DICT_ASSERTS stringID = case (ai.assertStringID)
                                0: ASSERTS_NOTOKENS;
                                1: ASSERTS_NOREGISTERS;
                            endcase;

    link_streams.send(STREAMS_REQUEST { streamID: STREAMS_ASSERT,
                                        stringID: tagged STRINGID_assert stringID,
                                        payload0: zeroExtend(pack(ai.assertSeverity)),
                                        payload1: ? });
 
  endrule

  // heartBeat

  // An occaisional heartbeat just to let the outside world know the hardware is alive.
  // Currently happens every 1000 model cycles.

  rule heartBeat (beat == 1000);

    link_streams.send(STREAMS_REQUEST { streamID: STREAMS_MESSAGE,
                                        stringID: tagged STRINGID_message MESSAGES_HEARTBEAT,
                                        payload0: truncate(curTick),
                                        payload1: truncate(modelTick) });
    beat <= 0;

  endrule
 
 
  // finishUp
  
  // Count down some extra time for the events controller to dump stuff
 
  rule finishUp (state == CON_Finished && finishing != 0);
  
    finishing <= finishing - 1;

  endrule
  
  // endSim
  
  // The extra time has run out and it's time to actually finish.
  // End the simulation. Our exit code is our pass-fail status.
  
  rule endSim (state == CON_Finished && finishing == 0);
  
    if (passed)
      starter.endSim(0);
    else
      starter.endSim(1);
    
  endrule

endmodule
