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

import RWire::*;
import FIFO::*;
import FIFOF::*;

// A FIFO with latency zero. The data is buffered if no one is "listening"
// Then future requests are answered from the buffer.

module mkBypassFIFO(FIFO#(a)) provisos (Bits#(a,sa));

  RWire#(a) enqw <- mkRWire;
  RWire#(a) result <- mkRWire;
  RWire#(PrimUnit) deqw <- mkRWire;
  FIFOF#(a) the_fifof <- mkUGFIFOF;

  rule doResult;
    if (the_fifof.notEmpty)
      result.wset(the_fifof.first());
    else case (enqw.wget()) matches
      tagged Just .r:
        result.wset(r);
      tagged Nothing:
        noAction;
      endcase
  endrule

  rule doUpdate_enq;
    case (enqw.wget()) matches
      tagged Just .r: 
        if (the_fifof.notEmpty || !isJust(deqw.wget))
          the_fifof.enq(r); 
      tagged Nothing:
        noAction;
    endcase
  endrule

  rule doUpdate_deq;
    if (isJust(deqw.wget) && the_fifof.notEmpty)
      the_fifof.deq();
  endrule

  method Action clear();
    the_fifof.clear();
  endmethod: clear

  method Action enq(val) if (the_fifof.notFull);
    enqw.wset(val);
  endmethod: enq

  method Action deq() if ((the_fifof.notEmpty || isJust (enqw.wget())));
    deqw.wset(?); // I hate '?'.
  endmethod: deq

  method first() if (isJust(result.wget));
    return unJust(result.wget);
  endmethod: first

endmodule

module mkBypassSizedFIFO#(Integer x) (FIFO#(a)) provisos (Bits#(a,sa));

  RWire#(a) enqw <- mkRWire;
  RWire#(a) result <- mkRWire;
  RWire#(PrimUnit) deqw <- mkRWire;
  FIFOF#(a) the_fifof <- mkUGSizedFIFOF(x);

  rule doResult;
    if (the_fifof.notEmpty)
      result.wset(the_fifof.first());
    else case (enqw.wget()) matches
      tagged Just .r:
        result.wset(r);
      tagged Nothing:
        noAction;
      endcase
  endrule

  rule doUpdate_enq;
    case (enqw.wget()) matches
      tagged Just .r: 
        if (the_fifof.notEmpty || !isJust(deqw.wget))
          the_fifof.enq(r); 
      tagged Nothing:
        noAction;
    endcase
  endrule

  rule doUpdate_deq;
    if (isJust(deqw.wget) && the_fifof.notEmpty)
      the_fifof.deq();
  endrule

  method Action clear();
    the_fifof.clear();
  endmethod: clear

  method Action enq(val) if (the_fifof.notFull);
    enqw.wset(val);
  endmethod: enq

  method Action deq() if ((the_fifof.notEmpty || isJust (enqw.wget())));
    deqw.wset(?); // I hate '?'.
  endmethod: deq

  method first() if (isJust(result.wget));
    return unJust(result.wget);
  endmethod: first

endmodule


