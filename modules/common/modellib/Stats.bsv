import FIFO::*;
import ModuleCollect::*;

//AWB Parameters
//name:                  default:
//HASIM_STATS_ENABLED   True
//HASIM_STATS_SIZE      32
//HASIM_STATS_LOGFILE   stats_log.out

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
  File ST_LogFile;
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

  FIFO#(StatData)        chainQ <- mkFIFO();
  Reg#(Bit#(`HASIM_STATS_SIZE))   stat <- mkReg(0);
  Reg#(StatState)         state <- mkReg(Recording);
  Reg#(Bool) enabled <- mkReg(True);
  let stats_log <- mkReg(InvalidFile);
 
  rule insert (state == Dumping);
    
    $fdisplay(stats_log, "%s: %0d", statname, stat);
    chainQ.enq(tagged ST_Val stat);
    state <= DoneDumping;
  
  endrule
      
  rule endDump (state == DoneDumping);
  
    chainQ.enq(tagged ST_Boundary);
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
		    tagged ST_LogFile .fd: 
		    begin
		      stats_log <= fd;
		      chainQ.enq(st);
		    end
		    tagged ST_Reset: 
		    begin
		      stat <= 0;
		      chainQ.enq(st);
		    end
		    tagged ST_Disable: 
		    begin
		      enabled <= False;
		      chainQ.enq(st);
		    end
		    tagged ST_Enable: 
		    begin
		      enabled <= True;
		      chainQ.enq(st);
		    end
		    default: chainQ.enq(st);
		  endcase
		  
		endmethod

	     endinterface);
	     
  //Get our type for typechecking
  Bit#(`HASIM_STATS_SIZE) msg = ?;
  let mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let inf = CChain_Info {cnum: 1, ctype: mytype, conn: chn};
  addToCollection(tagged LChain inf);

  method Action incr();
    
    if (enabled)
      stat <= stat + 1;
  
  endmethod

endmodule

module [Connected_Module] mkStatCounter_Disabled#(String statname)
  //interface:
              (Stat);

  FIFO#(StatData)        chainQ <- mkFIFO();
  
  let chn = (interface FIFO;
  
	        method CON_Data first() = marshall(chainQ.first());
		method Action deq() = chainQ.deq();
		method Action clear() = noAction;
	        method Action enq(CON_Data x);
		
		  chainQ.enq(unmarshall(x));
		  
		endmethod

	     endinterface);

  //Get our type for typechecking
  Bit#(`HASIM_STATS_SIZE) msg = ?;
  let mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let inf = CChain_Info {cnum: 1, ctype: mytype, conn: chn};
  addToCollection(tagged LChain inf);
  
  method Action incr();
    
    noAction;
  
  endmethod

endmodule



typedef enum
{
  SC_Initializing, SC_SetLogFile, SC_Idle, SC_Dumping, SC_Enabling, SC_Disabling, SC_Reseting
}
  StatConState
               deriving (Eq, Bits);

interface StatController;

  method Action enableStats();
  method Action disableStats();
  method Action resetStats();
  method Action dumpStats();

endinterface

module [Connected_Module] mkStatController_Simulation
    //interface:
                (StatController);

  FIFO#(StatData)  chainQ <- mkFIFO();
  Reg#(Bit#(32))      cur <- mkReg(0);
  Reg#(StatConState)  state <- mkReg(SC_Initializing);
  let stats_log <- mkReg(InvalidFile);
    
  
  rule initialize (state == SC_Initializing);
  
    let fd <- $fopen(`HASIM_STATS_LOGFILE, "w");
    
    if (fd == InvalidFile)
    begin
      $display("Event Controller: ERROR: Could not create file %s", `HASIM_STATS_LOGFILE);
      $finish(1);
    end
    
    stats_log <= fd;
    state <= SC_SetLogFile;
  
  endrule
  
  rule process (state != SC_Initializing);
  
    chainQ.deq();
    
    case (chainQ.first()) matches
      tagged ST_Boundary:
      begin
	cur <= 0;
	state <= SC_Idle;
	$fdisplay(stats_log, "****** STATS DUMP ENDS ******");
      end
      tagged ST_Val .d:
      begin
        //$display("STAT %0d: 0x%h", cur, d);
	//Record stat here
	cur <= cur + 1;
      end
      default:
      begin
	cur <= 0;
	state <= SC_Idle;
      end
    endcase
     
  endrule
  
  let canStart = !((state == SC_Idle) || (state == SC_Initializing));
  
  let nextCommand = case (state) matches
                     tagged SC_SetLogFile:   return tagged ST_LogFile stats_log;
                     tagged SC_Dumping:      return tagged ST_Boundary;
		     tagged SC_Enabling:     return tagged ST_Enable;
		     tagged SC_Disabling:    return tagged ST_Disable;
		     tagged SC_Reseting:     return tagged ST_Reset;
		   endcase;
  
  let chn = (interface FIFO;
  
	        method CON_Data first() if (canStart);
		
		  return marshall(nextCommand);
		   
		endmethod
		
		method Action deq() if (canStart);
		  state <= SC_Idle;
		endmethod
	        method Action enq(CON_Data x) = chainQ.enq(unmarshall(x));
		method Action clear() = noAction;

	     endinterface);

  //Get our type for typechecking
  Bit#(`HASIM_STATS_SIZE) msg = ?;
  let mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let inf = CChain_Info {cnum: 1, ctype: mytype, conn: chn};
  addToCollection(tagged LChain inf);
  
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
  
    $fdisplay(stats_log, "****** STATS DUMP BEGINS ******");
    state <= SC_Dumping;
  
  endmethod
  
  
endmodule
