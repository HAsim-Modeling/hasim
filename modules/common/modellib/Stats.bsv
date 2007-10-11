import FIFO::*;
import ModuleCollect::*;

//AWB Parameters
//name:                  default:
//HASIM_STATS_ENABLED   True
//HASIM_STATS_SIZE      32
`define CHAIN_IDX_STATS 1

interface Stat;
  
  method Action incr();
  
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

endmodule



typedef enum
{
  SC_Initializing, SC_Idle, SC_Dumping, SC_Enabling, SC_Disabling, SC_Reseting
}
  StatConState
               deriving (Eq, Bits);

interface StatController;

  method Action enableStats();
  method Action disableStats();
  method Action resetStats();
  method Action dumpStats();
  method ActionValue#(StatInfo) getNextStat();
  method Bool   noMoreStats();

endinterface

typedef struct
{
        Bit#(8)  statStringID;
        Bit#(32) statValue;
}
  StatInfo deriving (Eq, Bits);


module [Connected_Module] mkStatController
    //interface:
                (StatController);
		
  Connection_Chain#(StatData) chain <- mkConnection_Chain(`CHAIN_IDX_STATS);
  FIFO#(StatInfo)  statQ  <- mkFIFO();
  Reg#(Bit#(8))      cur  <- mkReg(0);
  Reg#(Bool)      dump_finished  <- mkReg(False);
  Reg#(StatConState)  state <- mkReg(SC_Idle);
    

  rule processResp (state != SC_Initializing);
  
    let st <- chain.receive_from_prev();
    
    case (st) matches
      tagged ST_Boundary:
      begin
        cur <= 0;
        state <= SC_Idle;
	dump_finished <= True;
      end
      tagged ST_Val .d:
      begin
        cur <= cur + 1;
        statQ.enq(StatInfo {statStringID: cur, statValue: d});
      end
      default:
      begin
        cur <= 0;
        state <= SC_Idle;
      end
    endcase
     
  endrule
  
  rule sendReq (!((state == SC_Idle) || (state == SC_Initializing)));
  

    let nextCommand = case (state) matches
                       tagged SC_Dumping:      return tagged ST_Boundary;
                       tagged SC_Enabling:     return tagged ST_Enable;
                       tagged SC_Disabling:    return tagged ST_Disable;
                       tagged SC_Reseting:     return tagged ST_Reset;
                       default:                return tagged ST_Boundary;
                     endcase;
  
    chain.send_to_next(nextCommand);
    state <= SC_Idle;
  
  endrule
  
  method Action enableStats if (state == SC_Idle);
  
    state <= SC_Enabling;
  
  endmethod
  
  method Action disableStats if (state == SC_Idle);
  
    state <= SC_Disabling;
  
  endmethod
  
  method Action resetStats if (state == SC_Idle);
  
    state <= SC_Reseting;
  
  endmethod
  
  method Action dumpStats if (state == SC_Idle);
  
    state <= SC_Dumping;
    dump_finished <= False;
  
  endmethod
  
  method ActionValue#(StatInfo) getNextStat();
  
    statQ.deq();
    return statQ.first();
    
  endmethod
  
  method Bool noMoreStats();
  
    return dump_finished;
    
  endmethod
  
endmodule
