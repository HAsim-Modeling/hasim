import FIFO::*;

typedef enum
{
  Empty,
  OneElem,
  Full
}
  FState deriving (Bits, Eq);
  

module [Connected_Module] mkStatFIFO#(String name) (FIFO#(t)) provisos (Bits#(t, t_SZ));

  let statname = strConcat("FIFO ", strConcat(name, " rating"));
  Stat stat <- mkStatCounter(statname);
  
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
  
    $display("WARNING: clear() called on StatFIFO %s. This stat will probably be wrong.", name);
    q.clear();
  
  endmethod
  

endmodule
