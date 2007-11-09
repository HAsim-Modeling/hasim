
// BSV library imports
import PrimArray::*;
import RegFile::*;
import Connectable::*;
import FIFO::*;

// HASim library imports
`include "hasim_common.bsh"
`include "soft_connections.bsh"
`include "hasim_modellib.bsh"
`include "platform_interface.bsh"
`include "front_panel.bsh"
`include "hasim_local_controller.bsh"
`include "hasim_events_controller.bsh"
`include "hasim_stats_controller.bsh"
`include "hasim_assertions_controller.bsh"

// ************* Simple Controller Hardware (Simulation) Only **************

//
// Simulation only. Does not use RRR. Not appropriate for actually placing onto an FPGA.

// AWB Parameters
// name:                 default:
// CTRL_EVENTS_LOGFILE    events_log.out
// CTRL_EVENTS_COOLDOWN   500
// CTRL_STATS_LOGFILE     stats_log.out
// CTRL_BMARK_TIMEOUT     100000

// ConState

// Internal mode of the Controller

typedef enum
{
  CON_Init,
  CON_Loading,
  CON_Running,
  CON_DumpingStats,
  CON_Cooldown
}
  ConState
           deriving (Eq, Bits);

//BEGIN DICTIONARY

//Eventually these will be separate dictionaries that will be user-generated.
//Currently hardcoded to 5-stage pipeline.

function String lookup_stat(Bit#(8) st);

  case (st)
    0: return "Instructions Committed";
    1: return "DCache Misses";
    2: return "Branch Mispredicts";
    3: return "ICache Misses";
    4: return "Instructions Fetched";
    5: return "Total Cycles";
    default: return "Unknown Stat";
  endcase

endfunction

function String lookup_event(Bit#(8) evt);

  case (evt)
    0: return "1 FET";
    1: return "2     DEC";
    2: return "3         EXE";
    3: return "4             MEM";
    4: return "5                 WB";
    default: return "Unknown Event";
  endcase

endfunction

function String lookup_assert(Bit#(8) ast);

  case (ast)
    0: return "FUNCP: Ran out of tokens. Increase the parameter HASIM_TOK_INDEX_BITS and recompile.";
    1: return "FUNCP: Ran out of physical registers. Recompile with a larger number of physical registers.";
    default: return "Unknown Assertion";
  endcase

endfunction
//END DICTIONARY

// mkController

// This controller simply loads a single test case, 
// runs it, and reports the result.

module [HASim_Module] mkController
  // interface:
                ();


  // *********** State ***********
  
  // Current FPGA Clock Cycle
  Reg#(Bit#(64))   cur_fpga_cc     <- mkReg(0);
  
  // Current Model Clock Cycle (according to the Events Controller)
  Reg#(Bit#(64))   cur_model_cc    <- mkReg(0);
  
  // A cooldown period when we continue to dump events.
  Reg#(Bit#(16))   cooldown_timer  <- mkReg(`CTRL_EVENTS_COOLDOWN);
  
  // Every so often we emit a heartbeat just to let the outside world
  // know that the hardware is still alive.
  Reg#(Bit#(64)) beat <- mkReg(0);
  
  // Did the program pass or fail?
  Reg#(Bool)       passed           <- mkReg(False);

  // Our internal mode.
  Reg#(ConState)   state           <- mkReg(CON_Init);
  
  // *********** Logfiles ***********

  // Simulation-only output file pointers.
  
  Reg#(File) event_log <- mkReg(InvalidFile);
  Reg#(File) stats_log <- mkReg(InvalidFile);
  
  
  // ********** Submodules ***********
  
  // Our communication links to the distributed Events and Stats counters.
  
  EventsController     events_controller    <- mkEventsController();
  StatsController      stats_controller     <- mkStatsController();
  AssertionsController    asserts_controller   <- mkAssertionsController();
  
  // *********** Soft Connections ***********
  
  // Our communication links to the local controllers of modules.
  
  Connection_Chain#(Command)   link_command   <- mkConnection_Chain(2);
  Connection_Chain#(Response)  link_response  <- mkConnection_Chain(3);
  
  // We tie off the LEDs, switches, and buttons.
  
  Connection_Send#(FRONTP_MASKED_LEDS) link_leds <- mkConnection_Send("fpga_leds");
  Connection_Receive#(FRONTP_SWITCHES) link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(ButtonInfo)      link_buttons <- mkConnection_Receive("fpga_buttons");
  
  
  //*********** Rules ***********
  
  // tick
  
  // Count the current FPGA cycle
  
  rule tick (True);
  
    cur_fpga_cc <= cur_fpga_cc + 1;
  
  endrule
  
  //finish_Commands
  
  // We are the end of the Command chain, so we just dequeue them when they get to us.
  
  rule finish_Commands (True);
  
    let cmd <- link_command.receive_from_prev();
  
  endrule
  
  //runProg
  
  // Begin our run. Open all logfiles, then wait until the program completes.
  
  rule runProg (state == CON_Init);
     
     // Open the events logfile
     
     let event_fd <- $fopen(`CTRL_EVENTS_LOGFILE);
     
     if (event_fd == InvalidFile)
     begin
       $display("ERROR: Controller: Could not open events logfile %s for writing!", `CTRL_EVENTS_LOGFILE);
       $finish(1);
     end
     
     event_log <= event_fd;
     
     // Open the stats logfile
     
     let stats_fd <- $fopen(`CTRL_STATS_LOGFILE);
     
     if (stats_fd == InvalidFile)
     begin
       $display("ERROR: Controller: Could not open stats logfile %s for writing!", `CTRL_STATS_LOGFILE);
       $finish(1);
     end

     stats_log <= stats_fd;

     // Start the program.

     link_command.send_to_next(COM_RunProgram);
     
     state <= CON_Running;
     link_leds.send(FRONTP_MASKED_LEDS{ state: 'b0011, mask: 'b1111 });
     $display("[%0d]: Controller: Program Started.", cur_fpga_cc);
     $fflush();

  endrule
  
  // getResponse
  
  // Get a response from the local controllers which are distributed throughout the system.
  
  rule getResponse (state == CON_Running);
  
    let resp <- link_response.receive_from_prev();
  
    case (resp) matches
      tagged RESP_DoneRunning .pf: // Program is done running.
        begin
          if (pf)  // It passed
          begin
            link_leds.send(FRONTP_MASKED_LEDS{ state: 'b1001, mask: 'b1111 });
            $display("[%0d]: Controller: Test program finished succesfully.", cur_fpga_cc);
            $fflush();
            passed <= True;
          end
          else // It failed
          begin
            link_leds.send(FRONTP_MASKED_LEDS{ state: 'b1101, mask: 'b1111 });
            $display("[%0d]: Controller: Test program finished. One or more failures occurred.", cur_fpga_cc);
            $fflush();
          end
          
          // Either way we begin dumping stats.
          
          $fdisplay(stats_log, "****** Stats Dump Begins ******");
          stats_controller.doCommand(Stats_Dump);
          state <= CON_DumpingStats;
        end
      default: // An unexpected response.
      begin
        $display("[%0d]: Controller: ERROR: Unexpected Timing Partition Response: 0x%h.", cur_fpga_cc, resp);
        $fflush();
      end
    endcase
    
  endrule
  
  // recordEvents
  
  // Record events to the logfile as they come in.
  
  rule recordEvents (True);
    
    let evt_info <- events_controller.getNextEvent();
    
    //Temporary convenience variable
    let tmp_model_cc = cur_model_cc;
    
    if (evt_info.eventBoundary == 1) //A model cycle boundary
    begin
      tmp_model_cc = tmp_model_cc + 1;
      $fdisplay(event_log, "[%d] ***********", tmp_model_cc);
      cur_model_cc <= cur_model_cc + 1;
      beat <= beat + 1;
    end
    
    String eventname = lookup_event(evt_info.eventStringID);
    $fdisplay(event_log, "[%d] %s: %0d", tmp_model_cc, eventname, evt_info.eventData);

  endrule
  
  // recordStats
  
  // Record stats to the logfile as they come in.
  
  rule recordStats (state == CON_DumpingStats);
    
    if (stats_controller.noMoreStats()) // Stats are done.
    begin
      $fdisplay(stats_log, "****** Stats Dump Ends ******");
      state <= CON_Cooldown;  // Start the cooldown.
    end
    else // Stats are still not done.
    begin
      let st_info <- stats_controller.getNextStat();
      String statname = lookup_stat(st_info.statStringID);
      $fdisplay(stats_log, "%s: %0d", statname, st_info.statValue);
    end
  endrule
  
  // assertionFailure
  
  // Report assertion failures as they come in.
  
  rule assertionFailure (True);
    
    let as_info <- asserts_controller.getAssertion();
    
    String assert_msg = strConcat("Assertion failed. ", lookup_assert(as_info.assertStringID));
    
    case (as_info.assertSeverity)
      ASSERT_Message: $display(assert_msg);
      ASSERT_Warning: $display(strConcat("WARNING: ", assert_msg));
      ASSERT_Error:
      begin
        $display(strConcat("ERROR: ", assert_msg));
        $finish(1);
      end
    endcase

  endrule
  
  // heartBeat

  // An occaisional heartbeat just to let the outside world know the hardware is alive.
  // Currently happens every 1000 model cycles.

  rule heartBeat (beat == 1000);
    
    $display("[%0d] Controller: 1000 model cycles simulated.", cur_fpga_cc);
    beat <= 0;

  endrule
 
  // finishUp
  
  // A cooldown period for Events to finish dumping. If the Events Controller were smarter
  // we wouldn't need this.
  
  rule finishUp (state == CON_Cooldown && cooldown_timer != 0);
    
    cooldown_timer <= cooldown_timer - 1;

  endrule
  
  // endSim
  
  // End the simulation. The cooldown period has ended.
  // The exit-code of the simulation is based on the program's exit status.
  
  rule endSim (state == CON_Cooldown && cooldown_timer == 0);
  
    if (passed)
      $finish(0);
    else
      $finish(1);
    
  endrule
 
endmodule
