
//BSV library imports
import PrimArray::*;
import RegFile::*;
import Connectable::*;
import FIFO::*;

//HASim library imports
import hasim_common::*;

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
  Reg#(Bit#(16)) finishing <- mkReg(500);
  Reg#(Bool)     passed <- mkReg(False);

  Reg#(ConState) state <- mkReg(CON_Init);
  
  EventController event_controller <- mkEventController_Software();
  StatController   stat_controller <- mkStatController_Software();
  
  Connection_Send#(Bit#(4))           link_leds <- mkConnection_Send("fpga_leds");
  Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(ButtonInfo)  link_buttons <- mkConnection_Receive("fpga_buttons");
  
  Connection_Client#(Command, Response)      link_tp <- mkConnection_Client("controller_to_tp");
  
  //*********** Rules ***********
  
  //tick
  rule tick (True);
  
    curTick <= curTick + 1;
  
  endrule
  
  //run_prog
  
  rule run_prog (state == CON_Init);
  
     link_tp.makeReq(COM_RunProgram);
     state <= CON_Running;
     link_leds.send(4'b0011);
     $display("Controller: Program Started on host CC %0d", curTick);
     
  endrule
  
  rule run_ends (state == CON_Running);
  
    let resp <- link_tp.getResp();
  
    case (resp) matches
      tagged RESP_DoneRunning .pf:
	begin
	  if (pf)
	  begin
	    link_leds.send(4'b1001);
	    $display("Controller: Test program finished succesfully.");
	    passed <= True;
	  end
	  else
	  begin
	    link_leds.send(4'b1101);
	    $display("Controller: Test program finished. One or more failures occurred.");
	  end
	  state <= CON_Finished;
	end
      default:
      begin
	$display("Controller: ERROR: Unexpected Timing Partition Response: 0x%h.", resp);
      end
    endcase

    
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
