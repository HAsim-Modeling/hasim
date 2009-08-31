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
//
// Tokens are the main way for HAsim to track data across simulator      
// partitions. The token type includes an index for token tables, epochs,
// and scratchpads which partitions can use as they see fit.             

import FIFO::*;

`include "asim/provides/common_services.bsh"
`include "asim/dict/STATS.bsh"


typedef enum
{
  Empty,
  OneElem,
  Full
}
  FState deriving (Bits, Eq);
  

module [Connected_Module] mkStatFIFO#(STATS_DICT_TYPE myID) (FIFO#(t)) provisos (Bits#(t, t_SZ));

  STAT stat <- mkStatCounter(myID);
  
  FIFO#(t) q <- mkFIFO();
  PulseWire enqW <- mkPulseWire();
  PulseWire deqW <- mkPulseWire();
  Reg#(FState) state <- mkReg(Empty);
  
  rule updStat (True);
  
    FState nextState = state;
  
    case (state)
      Empty:
      begin

        if (enqW)
        begin
          nextState = OneElem;
          //No stat change
          noAction;
        end
        else
        begin
          //No state change
          nextState = state;
          stat.incr();
        end
        
      end
      OneElem:
      begin

        if (enqW && deqW)
        begin
          //No stat or state change
          nextState = state;
          noAction;
        end
        else if (enqW)
        begin
          nextState = Full;
          stat.decr();
        end
        else if (deqW)
        begin
          nextState = Empty;
          stat.incr();
        end
        else
        begin
          //No stat or state change
          nextState = state;
          noAction;
        end
        
      end
      Full:
      begin

        if (deqW)
        begin
          nextState = OneElem;
          //No stat change
          noAction;
        end
        else
        begin
          //No state change
          nextState = state;
          stat.decr();
        end
        
      end
    endcase
    
    state <= nextState;
    
  endrule
  
  method Action enq(t x);
  
    q.enq(x);
    enqW.send();
  
  endmethod
  
  method Action deq();
  
    q.deq();
    deqW.send();
  
  endmethod
  
  method t first();
  
    return q.first();
  
  endmethod
  
  method Action clear();
    // Note that if clear is called this will probably be wrong.  
    q.clear();
  
  endmethod
  

endmodule
