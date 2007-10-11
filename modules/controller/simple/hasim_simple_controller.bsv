
//BSV library imports
import PrimArray::*;
import RegFile::*;
import Connectable::*;
import FIFO::*;

//HASim library imports
import hasim_common::*;
import hasim_modellib::*;
import soft_connections::*;
import platform_interface::*;
import hasim_local_controller::*;

//************* Simple Controller Simulation Only **************

//This controller simply loads a single test case, 
//runs it, and reports the result.
//
//Simulation only. No RRR.

//AWB Parameters
//name:                 default:
//CTRL_EVENTS_LOGFILE   events_log.out
//CTRL_EVENTS_COOLDOWN  500
//CTRL_STATS_LOGFILE    stats_log.out
//CTRL_BMARK_TIMEOUT    100000

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

//END DICTIONARY

module [HASim_Module] mkController ();


  //*********** State ***********
  
  Reg#(Bit#(64))   cur_fpga_cc     <- mkReg(0);
  Reg#(Bit#(64))   cur_model_cc    <- mkReg(0);
  Reg#(Bit#(16))   cooldown_timer  <- mkReg(`CTRL_EVENTS_COOLDOWN);
  Reg#(Bool)       passed	   <- mkReg(False);

  Reg#(ConState) state <- mkReg(CON_Init);
  
  //*********** Logfiles *********

  Reg#(File) event_log <- mkReg(InvalidFile);
  Reg#(File) stats_log <- mkReg(InvalidFile);
  
  
  //********** Submodules ********
  
  EventController     event_controller    <- mkEventController();
  StatController      stat_controller     <- mkStatController();
  
  //****** Soft Connections ********
  
  Connection_Chain#(Command)   link_command   <- mkConnection_Chain(2);
  Connection_Chain#(Response)  link_response  <- mkConnection_Chain(3);
  
  Connection_Send#(Bit#(4))           link_leds <- mkConnection_Send("fpga_leds");
  Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(ButtonInfo)  link_buttons <- mkConnection_Receive("fpga_buttons");
  
  
  //*********** Rules ***********
  
  //tick
  rule tick (True);
  
    cur_fpga_cc <= cur_fpga_cc + 1;
  
  endrule
  
  //finish_Commands
  
  rule finish_Commands (True);
  
    //Just finish the chain
    let cmd <- link_command.receive_from_prev();
  
  endrule
  
  //run_prog
  
  rule run_prog (state == CON_Init);
     
     let event_fd <- $fopen(`CTRL_EVENTS_LOGFILE);
     
     if (event_fd == InvalidFile)
     begin
       $display("ERROR: Controller: Could not open events logfile %s for writing!", `CTRL_EVENTS_LOGFILE);
       $finish(1);
     end
     
     event_log <= event_fd;
     
     let stats_fd <- $fopen(`CTRL_STATS_LOGFILE);
     
     if (stats_fd == InvalidFile)
     begin
       $display("ERROR: Controller: Could not open stats logfile %s for writing!", `CTRL_STATS_LOGFILE);
       $finish(1);
     end

     stats_log <= stats_fd;

     link_command.send_to_next(COM_RunProgram);
     
     state <= CON_Running;
     link_leds.send(4'b0011);
     $display("[%0d]: Controller: Program Started.", cur_fpga_cc);
     $fflush();
  endrule
  
  rule get_Response (state == CON_Running);
  
    let resp <- link_response.receive_from_prev();
  
    case (resp) matches
      tagged RESP_DoneRunning .pf:
	begin
	  if (pf)
	  begin
	    link_leds.send(4'b1001);
	    $display("[%0d]: Controller: Test program finished succesfully.", cur_fpga_cc);
            $fflush();
	    passed <= True;
	  end
	  else
	  begin
	    link_leds.send(4'b1101);
	    $display("[%0d]: Controller: Test program finished. One or more failures occurred.", cur_fpga_cc);
            $fflush();
	  end
	  $fdisplay(stats_log, "****** Stats Dump Begins ******");
	  stat_controller.dumpStats();
	  state <= CON_DumpingStats;
	end
      default:
      begin
	$display("[%0d]: Controller: ERROR: Unexpected Timing Partition Response: 0x%h.", cur_fpga_cc, resp);
        $fflush();
      end
    endcase

    
  endrule
  
  rule recordEvents (True);
    
    let evt_info <- event_controller.getNextEvent();
    
    //Temporary convenience variable
    let tmp_model_cc = cur_model_cc;
    
    if (evt_info.eventBoundary == 1) //A model cycle boundary
    begin
      tmp_model_cc = tmp_model_cc + 1;
      $fdisplay(event_log, "[%d] ***********", tmp_model_cc);
      cur_model_cc <= cur_model_cc + 1;
    end
    
    String eventname = lookup_event(evt_info.eventStringID);
    $fdisplay(event_log, "[%d] %s: %0d", tmp_model_cc, eventname, evt_info.eventData);

  endrule
  
  rule recordStats (state == CON_DumpingStats);
    
    if (stat_controller.noMoreStats())
    begin
      $fdisplay(stats_log, "****** Stats Dump Ends ******");
      state <= CON_Cooldown;
    end
    else
    begin
      let st_info <- stat_controller.getNextStat();
      String statname = lookup_stat(st_info.statStringID);
      $fdisplay(stats_log, "%s: %0d", statname, st_info.statValue);
    end
  endrule
  
  rule finishUp (state == CON_Cooldown && cooldown_timer != 0);
  
  //Allow some extra time for the event chain to dump
  
    cooldown_timer <= cooldown_timer - 1;
  endrule
  
  rule endSim (state == CON_Cooldown && cooldown_timer == 0);
  
    if (passed)
      $finish(0);
    else
      $finish(1);
    
  endrule
 
endmodule
