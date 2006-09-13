
//BSV library imports
import PrimArray::*;
import RegFile::*;
import Connectable::*;
import FIFO::*;

//HASim library imports
import hasim_base::*;
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

module [HASim_Module] mkController#(TModule#(Command, Response) th) ();


  //*********** State ***********
  
  Reg#(Tick) curTick <- mkReg(minBound);
  Reg#(Bit#(16)) finishing <- mkReg(2000);

  Reg#(ConState) state <- mkReg(CON_Init);
  
  EventController event_controller <- mkEventController_Software();
  StatController   stat_controller <- mkStatController_Software();
  
  Connection_Send#(Bit#(4))           link_leds <- mkConnection_Send("fpga_leds");
  Connection_Receive#(Bit#(4))    link_switches <- mkConnection_Receive("fpga_switches");
  Connection_Receive#(ButtonInfo)  link_buttons <- mkConnection_Receive("fpga_buttons");
  
  //*********** Rules ***********
  
  //tick
  rule tick (True);
  
    curTick <= curTick + 1;
  
  endrule
  
  //load_imem
  
  rule load_prog(state == CON_Init);
  
    th.exec(COM_LoadState);
    state <= CON_Loading;
  
  endrule
  
  //run_prog
  
  rule run_prog (state == CON_Loading);
  
    let resp <- th.response();
    case (resp) matches
      tagged RESP_DoneLoading:
	begin
          th.exec(COM_RunProgram);
          state <= CON_Running;
          $display("Controller: Program Started on host CC %0d", curTick);
	end
      default:
        begin
          $display("Controller: Unexpected TModule response [0]: %0h", pack(resp));

	  $finish(1);
	end
    endcase 
  endrule
  /*
  rule test_stats1 (curTick == 250);
    event_controller.toggle();
  endrule
  
  rule test_stats2 (curTick == 750);
    event_controller.toggle();
  endrule
  */
  
  rule run_ends (state == CON_Running);
  
    let resp <- th.response();
  
    case (resp) matches
      tagged RESP_DoneRunning:
	begin
          th.exec(COM_CheckResult);
          state <= CON_Checking;
          $display("Controller: Program Finished on host CC %0d", curTick);
	end
      default:
        begin
          $display("Controller: Unexpected TModule response [1]: %0h", pack(resp));

	  $finish(1);
	end
    endcase
  endrule
  
  rule getFails (state == CON_Checking);
    
    let resp <- th.response();
  
    case (resp) matches
      tagged RESP_Failure .f:
	begin
	  $display("Controller: ERROR: Memory location 0x%0h does not match expected result", f.addr);
	  $display("            Expected Value: 0x%0h", f.exp_v);
	  $display("            Actual Value: 0x%0h", f.found_v);
	end
      tagged RESP_CheckPassed:
        begin
	  state <= CON_Finished;

	  $display("Controller: Test program finished succesfully.");
	end
      tagged RESP_CheckFailed:
        begin
	  state <= CON_Finished;

	  $display("Controller: Test program finished. One or more failures occurred.");
	end
      default:
        begin
          $display("Controller: Unexpected TModule response [2]: %0h", pack(resp));
	  $finish(1);
	end
    endcase

    
  endrule
 
  rule finishUp (state == CON_Finished && finishing != 0);
  
    finishing <= finishing - 1;
  endrule
  
  rule endSim (state == CON_Finished && finishing == 0);
  
  
    $finish(0);
    
  endrule
 
endmodule
