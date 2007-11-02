
import RWire::*;
import FIFO::*;
import FIFOF::*;

//A FIFO with latency zero. The data is buffered if no one is "listening"
//Then future requests are answered from the buffer.

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

module mkBypassFIFO_old(FIFO#(a)) provisos (Bits#(a,sa));

  RWire#(a) enqw <- mkRWire;
  RWire#(PrimUnit) deqw <- mkRWire;
  FIFOF#(a) the_fifof <- mkUGFIFOF;

  rule doUpdate_enq;
    if ((isJust(enqw.wget())) &&
        (the_fifof.notEmpty || (!(isJust(deqw.wget())))))
       //Put into the FIFO if necessary
       the_fifof.enq (unJust(enqw.wget()));
  endrule

  rule doUpdate_deq;
    if((isJust(deqw.wget())) && (!(isJust(enqw.wget()) || the_fifof.notEmpty)))
       //dequeue if we should
       the_fifof.deq();

  endrule

  method Action clear();
    the_fifof.clear();
  endmethod

  method Action enq(val) if (the_fifof.notFull);
    enqw.wset(val);
  endmethod

  method Action deq() if ((the_fifof.notEmpty || isJust (enqw.wget())));
    deqw.wset(?); // I hate '?'.
  endmethod

  method first() if ((the_fifof.notEmpty || isJust(enqw.wget())));
    // if it's Empty, then the rwire must be something
    return (the_fifof.notEmpty)? (the_fifof.first()): (unJust (enqw.wget));
  endmethod

endmodule


//XXX check this for correctness
module mkBypassSizedFIFO_old#(Integer x)(FIFO#(a)) provisos (Bits#(a,sa));

  RWire#(a) enqw <- mkRWire;
  RWire#(PrimUnit) deqw <- mkRWire;
  FIFOF#(a) the_fifof <- mkSizedFIFOF(x);

  rule doUpdate;
    //Put into the FIFO if necessary
    if ((isJust(enqw.wget())) &&
        (the_fifof.notEmpty || (!(isJust(deqw.wget())))))
       the_fifof.enq (unJust(enqw.wget()));

    if((isJust(deqw.wget())) && (!(isJust(enqw.wget()) || the_fifof.notEmpty)))
       //dequeue if we should
       the_fifof.deq();

  endrule

  method Action clear();
    the_fifof.clear();
  endmethod

  method Action enq(val) if (the_fifof.notFull);
    enqw.wset(val);
  endmethod

  method Action deq() if (the_fifof.notEmpty || isJust (enqw.wget()));
    deqw.wset(?); // I hate '?'.
  endmethod

  method first() if (the_fifof.notEmpty || isJust (enqw.wget()));
    // if it's Empty, then the rwire must be something
    return (the_fifof.notEmpty)? (the_fifof.first): (unJust (enqw.wget));
  endmethod

endmodule

