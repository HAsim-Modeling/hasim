import FIFO::*;

import hasim_modellib::*;
import soft_connections::*;

`include "asim/provides/rrr.bsh"

`include "asim/rrr/remote_client_stub_STATS.bsh"
`include "asim/dict/RINGID.bsh"
`include "asim/dict/STATS.bsh"

// STATS_CONTROLLER: Control all the stats throughout the hardware model.

// STATS_CONTROLLER

// Controls all the stats throughout the hardware model.

// A StatsController can accept commands from the main hardware controller.
// After Dump command is asserted it returns the next stat with 
// getNextStat() until noMoreStats() is true.

interface STATS_CONTROLLER;

  method Action doCommand(STATS_COMMAND com);
  method Bool   noMoreStats();

endinterface

// STATS_COMMAND

// Commands that can be given to the Stats Controller

typedef enum
{
  STATS_Enable,
  STATS_Disable,
  STATS_Reset,
  STATS_Dump
}
  STATS_COMMAND
               deriving (Eq, Bits);


// STATS_CON_STATE

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
  STATS_CON_STATE
               deriving (Eq, Bits);

// RRR type STAT_INFO
typedef struct
{
  UINT32 statID;
  UINT32 value;
}
  STAT_INFO
               deriving (Eq, Bits);

// mkStatsController

// Abstracts all communication from the main controller to individual stat counters.

module [Connected_Module] mkStatsController
    //interface:
                (STATS_CONTROLLER);

  // ****** State Elements ******

  // Communication link to the Stats themselves
  Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);
 
  // Communication to our RRR server
  ClientStub_STATS client_stub <- mkClientStub_STATS();
  // Connection_Send#(RRR_Request) link_rrr <- mkConnection_Send("rrr_client_stats");
  
  // Track if we are done dumping
  Reg#(Bool) done_requested <- mkReg(False);
  Reg#(Bool) dump_finished  <- mkReg(False);
  
  // Our internal state
  Reg#(STATS_CON_STATE)  state <- mkReg(SC_Idle);
    
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
          
        client_stub.makeRequest_Send(STAT_INFO { statID: zeroExtend(stinfo.statID), value: stinfo.value });
          
      end
      tagged ST_Dump:  //We're done dumping
      begin
        
        client_stub.makeRequest_Done(?);
        state <= SC_Idle;
        done_requested <= True;
          
      end
      default:  //This should never happen
      begin
            
        state <= SC_Idle;
          
      end
    endcase
     
  endrule
    
  // waitForDoneAck
    
  // Wait for response to Done() RRR request
    
  rule waitForDoneAck (done_requested);
    
      let a <- client_stub.getResponse_Done();
      dump_finished <= True;
      
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
