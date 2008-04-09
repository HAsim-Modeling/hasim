import FIFO::*;

import hasim_modellib::*;
import soft_connections::*;

`include "streams.bsh"
`include "asim/dict/STREAMS.bsh"
`include "asim/dict/STREAMID.bsh"
`include "asim/dict/RINGID.bsh"

// StatsController: Control all the stats throughout the hardware model.

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

module [Connected_Module] mkStatsController#(Connection_Send#(STREAMS_REQUEST) link_streams)
    //interface:
                (STATS_CONTROLLER);

  // ****** State Elements ******

  // Communication link to the Stats themselves
  Connection_Chain#(StatData) chain <- mkConnection_Chain(`RINGID_STATS);
  
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
                       tagged SC_Dumping:      return tagged ST_Dump;
                       tagged SC_Enabling:     return tagged ST_Enable;
                       tagged SC_Disabling:    return tagged ST_Disable;
                       tagged SC_Reseting:     return tagged ST_Reset;
                       default:                return tagged ST_Dump;
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
      tagged ST_Val .stinfo: //A stat to dump
      begin

        link_streams.send(STREAMS_REQUEST { streamID: `STREAMID_STAT,
                                            stringID: stinfo.statID,
                                            payload0: stinfo.value,
                                            payload1: ? });
      end
      tagged ST_Dump:  //We're done dumping
      begin
        state <= SC_Idle;
        dump_finished <= True;
      end
      default:  //This should never happen
      begin
        state <= SC_Idle;
      end
    endcase
     
  endrule

  //doCommand
  
  //This method is the main method where the outside world tells us what to do.
  
  method Action doCommand(STATS_COMMAND com) if (state == SC_Idle);
   
    case (com)
      STATS_Enable:   state <= SC_Enabling;
      STATS_Disable:  state <= SC_Disabling;
      STATS_Reset:    state <= SC_Reseting;
      STATS_Dump:
      begin
        state         <= SC_Dumping;
        dump_finished <= False;
      end
    endcase

  endmethod
  
  // noMoreStats
  
  // When this goes on the outside world knows not to expect any more stats.
  
  method Bool noMoreStats();
  
    return dump_finished;
    
  endmethod
  
endmodule
