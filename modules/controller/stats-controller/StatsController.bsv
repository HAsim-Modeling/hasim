import FIFO::*;

import hasim_modellib::*;
import soft_connections::*;

// StatsController: Control all the stats throughout the hardware model.

`define CHAIN_IDX_STATS 1

// StatsConState

// An internal datatype to track the state of the stats controller

typedef enum
{
  SC_Initializing, //Starting up, doing local runtime initialization
  SC_Idle,         //Not executing any commands
  SC_Dumping,      //Executing the Dump command
  SC_Enabling,     //Executing the Enable command
  SC_Disabling,    //Executing the Disable command
  SC_Reseting      //Executing the Reset command
}
  StatsConState
               deriving (Eq, Bits);

// mkStatsController

// Abstracts all communication from the main controller to individual stat counters.

module [Connected_Module] mkStatsController
    //interface:
                (StatsController);

  // ****** State Elements ******

  // Communication link to the Stats themselves
  Connection_Chain#(StatData) chain <- mkConnection_Chain(`CHAIN_IDX_STATS);
  
  // Output FIFO of stats to send along
  FIFO#(StatInfo)  statQ  <- mkFIFO();
  
  // The current stat we are expecting to report
  Reg#(Bit#(8))      cur  <- mkReg(0);

  // Track if we are done dumping
  Reg#(Bool)      dump_finished  <- mkReg(False);
  
  // Our internal state
  Reg#(StatsConState)  state <- mkReg(SC_Idle);
    
  // ****** Rules ******
  
  // sendReq
  
  // Send a request to all the stats.
  // We only do this if we're in a state which requires new communication.
  // Afterwords we go back to idle.

  rule sendReq (!((state == SC_Idle) || (state == SC_Initializing)));
  

    let nextCommand = case (state) matches
                       tagged SC_Dumping:      return tagged ST_Boundary;
                       tagged SC_Enabling:     return tagged ST_Enable;
                       tagged SC_Disabling:    return tagged ST_Disable;
                       tagged SC_Reseting:     return tagged ST_Reset;
                       default:                return tagged ST_Boundary;
                     endcase;
  
    chain.send_to_next(nextCommand);
    state <= SC_Idle;
  
  endrule
  
  // processResp
  
  // Process a response from an individual stat. 
  // Most of the time this is just sent on to the outputQ.
  
  rule processResp (state != SC_Initializing);
  
    let st <- chain.receive_from_prev();
    
    case (st) matches
      tagged ST_Val .d: //A stat to dump
      begin
        cur <= cur + 1;
        statQ.enq(StatInfo {statStringID: cur, statValue: d});
      end
      tagged ST_Boundary:  //We're done dumping
      begin
        cur <= 0;
        state <= SC_Idle;
	dump_finished <= True;
      end
      default:  //This should never happen
      begin
        cur <= 0;
        state <= SC_Idle;
      end
    endcase
     
  endrule
  
  //doCommand
  
  //This method is the main method where the outside world tells us what to do.
  
  method Action doCommand(StatsCommand com) if (state == SC_Idle);
   
    case (com)
      Stats_Enable:   state <= SC_Enabling;
      Stats_Disable:  state <= SC_Disabling;
      Stats_Reset:    state <= SC_Reseting;
      Stats_Dump:
      begin
        state         <= SC_Dumping;
	dump_finished <= False;
      end
    endcase

  endmethod
  
  // getNextStat
  
  // This method returns the next stat we've received from the chain.
  
  method ActionValue#(StatInfo) getNextStat();
    
    let st = statQ.first();
    statQ.deq();
    return st;
    
  endmethod
  
  // noMoreStats
  
  // When this goes on the outside world knows not to expect any more stats.
  
  method Bool noMoreStats();
  
    return dump_finished;
    
  endmethod
  
endmodule
