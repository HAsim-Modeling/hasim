import FIFO::*;
import ModuleCollect::*;

//AWB Parameters
//name:                  default:
//HASIM_STATS_ENABLED   True
//HASIM_STATS_SIZE      32
`define CHAIN_IDX_STATS 1

interface Stat;
  
  method Action incr();
  method Action decr();
  
endinterface

module [Connected_Module] mkStatCounter#(String statname)
    //interface:
                (Stat);

    let m <- (`HASIM_STATS_ENABLED) ? mkStatCounter_Enabled(statname) : mkStatCounter_Disabled(statname);
    return m;

endmodule

typedef union tagged
{
  void ST_Boundary;
  Bit#(`HASIM_STATS_SIZE) ST_Val;
  void ST_Enable;
  void ST_Disable;
  void ST_Reset;
}
  StatData
           deriving (Eq, Bits);

typedef enum
{
  Dumping, DoneDumping, Recording
}
  StatState
            deriving (Eq, Bits);

module [Connected_Module] mkStatCounter_Enabled#(String statname)
  //interface:
              (Stat);

  Connection_Chain#(StatData) chain <- mkConnection_Chain(`CHAIN_IDX_STATS);
  Reg#(Bit#(`HASIM_STATS_SIZE))   stat <- mkReg(0);
  Reg#(StatState)         state <- mkReg(Recording);
  Reg#(Bool) enabled <- mkReg(True);
 
  rule insert (state == Dumping);
    
    chain.send_to_next(tagged ST_Val stat);
    state <= DoneDumping;
  
  endrule
      
  rule endDump (state == DoneDumping);
  
    chain.send_to_next(tagged ST_Boundary);
    state <= Recording;
    
  endrule
  
  rule shift (state == Recording);
  
    StatData st <- chain.receive_from_prev();

    case (st) matches 
      tagged ST_Boundary .t:
      begin
        state <= Dumping;
      end
      tagged ST_Reset: 
      begin
        stat <= 0;
        chain.send_to_next(st);
      end
      tagged ST_Disable: 
      begin
        enabled <= False;
        chain.send_to_next(st);
      end
      tagged ST_Enable: 
      begin
        enabled <= True;
        chain.send_to_next(st);
      end
      default: chain.send_to_next(st);
    endcase
  endrule
  
  method Action incr();
    
    if (enabled)
      stat <= stat + 1;
  
  endmethod

  method Action decr();
    
    if (enabled)
      stat <= stat - 1;
  
  endmethod

endmodule

module [Connected_Module] mkStatCounter_Disabled#(String statname)
  //interface:
              (Stat);

  Connection_Chain#(StatData) chain <- mkConnection_Chain(`CHAIN_IDX_STATS);
  
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


