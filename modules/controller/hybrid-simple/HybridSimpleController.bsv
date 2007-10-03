
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
import software_controller::*;
import connection_terminus::*;

//************* OneTest Controller **************

//This controller simply loads a single test case, 
//runs it, and reports the result.

//Useful for RTL simulation verification rather than
//extensive benchmarking or testing

//mkController_Software_OneTest :: TestHarness -> TestCase -> Module ()

typedef enum
{
  CON_Init,
  CON_Loading,
  CON_Running,
  CON_Checking,
  CON_Finished
}
  ConState
           deriving (Eq, Bits);

module [HASim_Module] mkController ();


  //*********** State ***********
  
  Reg#(Bit#(64)) curTick <- mkReg(minBound);
  Reg#(Bit#(64)) eventTick <- mkReg(minBound);
  Reg#(Bit#(16)) finishing <- mkReg(`HASIM_CONTROLLER_COOLDOWN);
  Reg#(Bool)     passed <- mkReg(False);

  Reg#(ConState) state <- mkReg(CON_Init);
  
  EventController     event_controller    <- mkEventController_Hybrid();
  StatController      stat_controller     <- mkStatController_Hybrid();
  
  Connection_Chain#(Command)   link_command   <- mkConnection_Chain(2);
  Connection_Chain#(Response)  link_response  <- mkConnection_Chain(3);
  
  Connection_Send#(Bit#(4))           link_leds <- mkConnection_Send("fpga_leds");
  Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(ButtonInfo)  link_buttons <- mkConnection_Receive("fpga_buttons");

  SoftwareController    swcon   <- mkSoftwareController();

  let terminus <- mkConnectionTerminus();
  
  //*********** Rules ***********
  
  //tick
  rule tick (True);
  
    curTick <= curTick + 1;
  
  endrule
  
  //finish_Commands
  
  rule finish_Commands (True);
  
    //Just finish the chain
    let cmd <- link_command.receive_from_prev();
  
  endrule
  
  //run_prog
  
  rule run_prog (state == CON_Init && swcon.getSimState() == 1);
  
     link_command.send_to_next(COM_RunProgram);
     
     state <= CON_Running;
     link_leds.send(4'b0011);

     swcon.printMessage1P(0, truncate(curTick));

  endrule
  
  rule get_Response (state == CON_Running);
  
    let resp <- link_response.receive_from_prev();
  
    case (resp) matches
      tagged RESP_DoneRunning .pf:
	begin
	  if (pf)
	  begin
	    link_leds.send(4'b1001);
            swcon.printMessage1P(1, truncate(curTick));
	    passed <= True;
	  end
	  else
	  begin
	    link_leds.send(4'b1101);
            swcon.printMessage1P(2, truncate(curTick));
	  end
	  stat_controller.dumpStats();
	  state <= CON_Finished;
	end
      default:
      begin
        swcon.printMessage2P(3, truncate(curTick), 0);
      end
    endcase

    
  endrule

  rule dumpStat (state == CON_Finished && finishing != 0);

    StatInfo si <- stat_controller.getNextStat();
    swcon.printStat(si.statStringID, si.statValue);

  endrule

  rule dumpEvent (True);

    EventInfo ei <- event_controller.getNextEvent();
    swcon.printEvent(ei.eventStringID, eventTick, ei.eventData);
    if (ei.eventBoundary == 1)
        eventTick <= eventTick + 1;

  endrule
 
 
  rule finishUp (state == CON_Finished && finishing != 0);
  
  //Allow some extra time for the event chain to dump
  
    finishing <= finishing - 1;
  endrule
  
  rule endSim (state == CON_Finished && finishing == 0);
  
    if (passed)
      $finish(0);
    else
      $finish(1);
    
  endrule
 
endmodule
