//
// Copyright (C) 2008 Massachusetts Institute of Technology
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


import FIFO::*;
import Counter::*;

//AWB Parameters
//name:                  default:
//HASIM_STATS_ENABLED   True
//HASIM_STATS_SIZE      32
`include "asim/dict/RINGID.bsh"
`include "asim/dict/STATS.bsh"

interface Stat;
  
  method Action incr();
  method Action decr();
  
endinterface

typedef Bit#(`HASIM_STATS_SIZE) STAT_VALUE;

module [Connected_Module] mkStatCounter#(STATS_DICT_TYPE statID)
    //interface:
                (Stat);

    let m <- (`HASIM_STATS_ENABLED) ? mkStatCounter_Enabled(statID) : mkStatCounter_Disabled(statID);
    return m;

endmodule

typedef union tagged
{
  void ST_Dump;
  void ST_Enable;
  void ST_Disable;
  struct {STATS_DICT_TYPE statID; STAT_VALUE value;}  ST_Val;
  void ST_Reset;
}
  STAT_DATA
           deriving (Eq, Bits);

typedef enum
{
  Recording, Dumping, FinishingDump
}
  STAT_STATE
            deriving (Eq, Bits);

module [Connected_Module] mkStatCounter_Enabled#(STATS_DICT_TYPE myID)
  //interface:
              (Stat);

  Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);
  COUNTER#(`HASIM_STATS_SIZE) stat  <- mkLCounter(0);
  Reg#(STAT_STATE)            state <- mkReg(Recording);
  Reg#(Bool)                  enabled <- mkReg(True);
 
  rule dump (state == Dumping);
    
    chain.send_to_next(tagged ST_Dump);
    state <= Recording;

  endrule

  (* conservative_implicit_conditions *)
  rule shift (state == Recording);
  
    STAT_DATA st <- chain.receive_from_prev();

    case (st) matches 
      tagged ST_Dump:
      begin
        //
        // Send the current value of the counter along the chain and reset
        // the counter.  If the run continues the software side will request
        // more stats dumps and compute the sum.
        //
        chain.send_to_next(tagged ST_Val {statID: myID, value: stat.value()});
        stat.setC(0);
        state <= Dumping;
      end
      tagged ST_Reset: 
      begin
        chain.send_to_next(st);
        stat.setC(0);
      end
      tagged ST_Disable: 
      begin
        chain.send_to_next(st);
        enabled <= False;
      end
      tagged ST_Enable: 
      begin
        chain.send_to_next(st);
        enabled <= True;
      end
      default: chain.send_to_next(st);
    endcase

  endrule
  
  method Action incr();
    
    if (enabled)
      stat.up();
  
  endmethod

  method Action decr();
    
    if (enabled)
      stat.down();
  
  endmethod

endmodule

module [Connected_Module] mkStatCounter_Disabled#(STATS_DICT_TYPE statname)
  //interface:
              (Stat);

  Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);
  
  rule shift (True);
  
    let st <- chain.receive_from_prev();
    chain.send_to_next(st);
  
  endrule
  
  method Action incr();
    
    noAction;
  
  endmethod

  method Action decr();
    
    noAction;
  
  endmethod
endmodule


interface STAT_RECORDER_MULTIPLEXED#(type ni);
  
  method Action incr(INSTANCE_ID#(ni) iid);
  method Action decr(INSTANCE_ID#(ni) iid);
  
endinterface

module [Connected_Module] mkStatCounter_Multiplexed#(STATS_DICT_TYPE myID)
  //interface:
              (STAT_RECORDER_MULTIPLEXED#(ni));

  Connection_Chain#(STAT_DATA) chain <- mkConnection_Chain(`RINGID_STATS);
  MULTIPLEXED#(ni, COUNTER#(`HASIM_STATS_SIZE)) statPool  <- mkMultiplexed(mkLCounter(0));

  Reg#(STAT_STATE)          state <- mkReg(Recording);
  Reg#(Bool)                enabled <- mkReg(True);
  COUNTER#(INSTANCE_ID_BITS#(ni)) currentInstance <- mkLCounter(0);
 
  rule finishDump (state == FinishingDump);
    
    chain.send_to_next(tagged ST_Dump);
    state <= Recording;

  endrule

  rule dump (state == Dumping);
    
    chain.send_to_next(tagged ST_Val {statID: myID, value: statPool[currentInstance.value()].value()});
    
    statPool[currentInstance.value()].setC(0);
    currentInstance.up();

    if (currentInstance.value() == fromInteger(valueOf(ni) - 1)) 
        state <= FinishingDump;

  endrule

  (* conservative_implicit_conditions *)
  rule shift (state == Recording);
  
    STAT_DATA st <- chain.receive_from_prev();

    case (st) matches 
      tagged ST_Dump:
      begin
        //
        // Send the current value of the counter along the chain and reset
        // the counter.  If the run continues the software side will request
        // more stats dumps and compute the sum.
        //
        chain.send_to_next(tagged ST_Val {statID: myID, value: statPool[0].value()});
        statPool[0].setC(0);
        currentInstance.setC(1);
        state <= Dumping;
      end
      tagged ST_Reset: 
      begin
        chain.send_to_next(st);
        for (Integer x = 0; x < valueOf(ni); x = x + 1)
            statPool[x].setC(0);
      end
      tagged ST_Disable: 
      begin
        chain.send_to_next(st);
        enabled <= False;
      end
      tagged ST_Enable: 
      begin
        chain.send_to_next(st);
        enabled <= True;
      end
      default: chain.send_to_next(st);
    endcase

  endrule
  
  method Action incr(INSTANCE_ID#(ni) iid);
    
    if (enabled)
      statPool[iid].up();
  
  endmethod

  method Action decr(INSTANCE_ID#(ni) iid);
    
    if (enabled)
      statPool[iid].down();
  
  endmethod

endmodule
