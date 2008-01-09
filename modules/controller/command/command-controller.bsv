
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

module [HASim_Module] mkCommandController#(Streams streams,
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
     link_leds.send(8'b00000011);

     streams.printMessage1P(0, truncate(curTick)); //Message 0 = Program Started.

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
            link_leds.send(8'b00001001);
            streams.printMessage1P(1, truncate(curTick)); // Message 1 = Success
            passed <= True;
          end
          else  // It failed
          begin
            link_leds.send(8'b00001101);
            streams.printMessage1P(2, truncate(curTick)); // Message 2 = Failure
          end
          // Either way we begin dumping.
          stats_controller.doCommand(Stats_Dump);
          state <= CON_Finished;
        end
      default: // Unexpected Response
      begin
        streams.printMessage2P(3, truncate(curTick), 0); // Message 3 = Unexpected Response
      end
    endcase

  endrule

  // passOnStat
  
  // Passes a stat from the Stats Controller on to the Software Controller for dumping.

  rule passOnStat (state == CON_Finished && finishing != 0);

    StatInfo si <- stats_controller.getNextStat();
    streams.printStat(si.statStringID, si.statValue);

  endrule

  // passOnEvent
  
  // Passes an Event on to the Software Controller for dumping.

  rule passOnEvent (True);

    EventInfo ei <- events_controller.getNextEvent();
    streams.printEvent(ei.eventStringID, modelTick, ei.eventData);
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
    streams.printAssertion(ai.assertStringID, zeroExtend(pack(ai.assertSeverity)));
 
  endrule

  // heartBeat

  // An occaisional heartbeat just to let the outside world know the hardware is alive.
  // Currently happens every 1000 model cycles.

  rule heartBeat (beat == 1000);

    streams.printMessage2P(5, truncate(curTick), truncate(modelTick)); //Message 5 = 1000 Cycles simulated.
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
  // (In real hardware this rule will be removed by the synthesis tools.)
  
  rule endSim (state == CON_Finished && finishing == 0);
  
    if (passed)
      $finish(0);
    else
      $finish(1);
    
  endrule

endmodule
