//
// Copyright (C) 2008 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

import FIFO::*;

import hasim_common::*;
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
  
  // Track if we are done dumping
  Reg#(Bool) dumpFinished  <- mkReg(False);
  
  // Our internal state
  Reg#(STATS_CON_STATE)  state <- mkReg(SC_Idle);
    
  // ****** Rules ******
  
  // sendReq
  
  // Send a request to all the stats.
  // We only do this if we're in a state which requires new communication.
  // Afterwords we go back to idle.

  rule sendReq (!((state == SC_Idle) || (state == SC_Initializing)));
  
    let nextCommand = case (state) matches
                       tagged SC_Dumping:      return tagged ST_DUMP;
                       tagged SC_Enabling:     return tagged ST_ENABLE;
                       tagged SC_Disabling:    return tagged ST_DISABLE;
                       tagged SC_Reseting:     return tagged ST_RESET;
                       default:                return tagged ST_DUMP;
                     endcase;
  
    chain.sendToNext(nextCommand);
    state <= SC_Idle;
  
  endrule
  
  // processResp
  
  // Process a response from an individual stat. 
  // Most of the time this is just sent on to the outputQ.
  
  rule processResp (state != SC_Initializing);
  
    let st <- chain.recvFromPrev();
    
    case (st) matches
      tagged ST_VAL .stinfo: //A stat to dump
      begin
        client_stub.makeRequest_Send(zeroExtend(stinfo.statID), stinfo.value);
      end
      tagged ST_DUMP:  //We're done dumping
      begin
        client_stub.makeRequest_Done(?);
      end
    endcase
     
  endrule
    
  // waitForDoneAck
    
  // Wait for response to Done() RRR request
    
  rule waitForDoneAck (! dumpFinished);
    
      let a <- client_stub.getResponse_Done();
      dumpFinished <= True;
      
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
        state <= SC_Dumping;
        dumpFinished <= False;
      end
    endcase

  endmethod
  
  // noMoreStats
  
  // When this goes on the outside world knows not to expect any more stats.
  
  method Bool noMoreStats();
    return dumpFinished;
  endmethod
  
endmodule
