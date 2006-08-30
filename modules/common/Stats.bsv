import FIFO::*;
import ModuleCollect::*;

import HASim::*;


`define STAT_SIZE 32

interface Stat;
  
  method Action incr();
  
endinterface

typedef union tagged
{
  void ST_Boundary;
  void ST_Toggle;
  void ST_Reset;
  Bit#(`STAT_SIZE) ST_Val;
}
  StatData
           deriving (Eq, Bits);

typedef enum
{
  Dumping, DoneDumping, Recording
}
  StatState
            deriving (Eq, Bits);

module [Connected_Module] mkStatCounter#(String statname)
  //interface:
              (Stat);

  FIFO#(StatData)        chainQ <- mkFIFO();
  Reg#(Bit#(`STAT_SIZE))   stat <- mkReg(0);
  Reg#(StatState)         state <- mkReg(Recording);
  Reg#(Bool) enabled <- mkReg(True);
 
  rule insert (state == Dumping);
  
    chainQ.enq(ST_Val stat);
    state <= DoneDumping;
  
  endrule
      
  rule endDump (state == DoneDumping);
  
    chainQ.enq(ST_Boundary);
    state <= Recording;
    
  endrule
  
  let chn = (interface FIFO;
  
	        method CON_Data first() = marshall(chainQ.first());
		method Action deq() = chainQ.deq();
		method Action clear() = noAction;
	        method Action enq(CON_Data x) if (state == Recording);
		
		  StatData st = unmarshall(x);
		  
		  case (st) matches 
		    tagged ST_Boundary .t: 
		    begin
		      state <= Dumping;
		    end
		    tagged ST_Reset: 
		    begin
		      stat <= 0;
		      chainQ.enq(st);
		    end
		    tagged ST_Toggle: 
		    begin
		      enabled <= !enabled;
		      chainQ.enq(st);
		    end
		    default: chainQ.enq(st);
		  endcase
		  
		endmethod

	     endinterface);
	     
  //Add our interface to the ModuleCollect collection
  addToCollection(LChain tuple2(2, chn));

  method Action incr();
    
    if (enabled)
      stat <= stat + 1;
  
  endmethod

endmodule

typedef enum
{
  SC_Toggle, //Enable/Disable stat collection 
  SC_Reset,  //Reset all counters
  SC_Dump    //Dump current counter values
}
  StatCommand
              deriving (Eq, Bits);


typedef enum
{
  Ready, Waiting, Gathering
}
  StatConState
               deriving (Eq, Bits);

interface StatController;

  method Action exec(StatCommand c);

endinterface

module [Connected_Module] mkStatController_Software
    //interface:
                (StatController);

  FIFO#(StatData)  chainQ <- mkFIFO();
  Reg#(Bit#(32))      cur <- mkReg(0);
  Reg#(StatCommand) nextCommand <- mkRegU();
  Reg#(StatConState)  state <- mkReg(Waiting);
    
  rule process (state == Gathering);
  
    chainQ.deq();
    
    case (chainQ.first()) matches
      tagged ST_Boundary:
      begin
	cur <= 0;
	state <= Waiting;
	$display("STAT END");
      end
      tagged ST_Reset:
      begin
	cur <= 0;
	state <= Waiting;
      end
      tagged ST_Toggle:
      begin
	cur <= 0;
	state <= Waiting;
      end
      tagged ST_Val .d:
      begin
        $display("STAT %0d: 0x%h", cur, d);
	cur <= cur + 1;
      end
    endcase
     
  endrule
  
  let chn = (interface FIFO;
  
	        method CON_Data first() if (state == Ready);
		
		  return marshall(case (nextCommand) matches
                     tagged SC_Toggle: return ST_Toggle;
		     tagged SC_Reset:  return ST_Reset;
		     tagged SC_Dump:   return ST_Boundary;
		   endcase);
		   
		endmethod
		
		method Action deq() if (state == Ready);
		  state <= Gathering;
		endmethod
	        method Action enq(CON_Data x) = chainQ.enq(unmarshall(x));
		method Action clear() = noAction;

	     endinterface);

  //Add our interface to the ModuleCollect collection
  addToCollection(LChain tuple2(2, chn));
  
  method Action exec(StatCommand c) if (state == Waiting);
    state <= Ready;
    nextCommand <= c;
  endmethod
  
endmodule
