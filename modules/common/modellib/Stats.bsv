import FIFO::*;
import Counter::*;

//AWB Parameters
//name:                  default:
//HASIM_STATS_ENABLED   True
//HASIM_STATS_SIZE      32
`include "asim/dict/RINGID.bsh"
`include "asim/dict/STREAMS.bsh"

interface Stat;
  
  method Action incr();
  method Action decr();
  
endinterface

typedef Bit#(`HASIM_STATS_SIZE) STAT_VALUE;

module [Connected_Module] mkStatCounter#(STREAMS_DICT_TYPE statID)
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
  struct {STREAMS_DICT_TYPE statID; STAT_VALUE value;}  ST_Val;
  void ST_Reset;
}
  StatData
           deriving (Eq, Bits);

typedef enum
{
  Recording, Dumping
}
  StatState
            deriving (Eq, Bits);

module [Connected_Module] mkStatCounter_Enabled#(STREAMS_DICT_TYPE myID)
  //interface:
              (Stat);

  Connection_Chain#(StatData) chain <- mkConnection_Chain(`RINGID_STATS);
  Counter#(`HASIM_STATS_SIZE) stat  <- mkCounter(0);
  Reg#(StatState)             state <- mkReg(Recording);
  Reg#(Bool)                  enabled <- mkReg(True);
 
  rule dump (state == Dumping);
    
    chain.send_to_next(tagged ST_Dump);
    state <= Recording;

  endrule

  rule shift (state == Recording);
  
    StatData st <- chain.receive_from_prev();

    case (st) matches 
      tagged ST_Dump:
      begin
        chain.send_to_next(tagged ST_Val {statID: myID, value: stat.value()});
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

module [Connected_Module] mkStatCounter_Disabled#(STREAMS_DICT_TYPE statname)
  //interface:
              (Stat);

  Connection_Chain#(StatData) chain <- mkConnection_Chain(`RINGID_STATS);
  
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


