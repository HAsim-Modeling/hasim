import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import Datatypes::*;
import FunctionalPartition::*;

module [Module] mkCPU_Test#(Memory#(Addr, Inst, Value, Token) mem) (CPU);

  let fp <- mkFP_Test(mem);
  let tp <- mkTP_Test(fp);
  
  method start = tp.start;
  method done = tp.done;

endmodule


typedef enum 
{ 
  TOK, FET, DEC, EXE, MEM, LCO, GCO 
} 
  Stage deriving (Eq, Bits);

 

module [Module] mkTP_Test#(FunctionalPartition#(Tick, Token,
                				Addr, Inst,
						void, DepInfo,
						void, InstResult,
						void, void,
						void, void,
						void, void) func) (TimingPartition);
  
  Reg#(Bool) running <- mkReg(False);
  
  Reg#(Bool) madeReq <- mkReg(False);
  
  Reg#(Stage) stage <- mkReg(TOK);
  
  Reg#(Token) cur_tok <- mkReg(0);
  Reg#(Inst)  cur_inst <- mkRegU();
  
  Reg#(Addr) pc <- mkReg(0);
  
  Reg#(Tick) baseTick <- mkReg(0);
  
  rule process (running);
   $display("WHEEE!");
    
    baseTick <= baseTick + 1;

    case (stage)
      TOK:
        if (!madeReq)
	  begin
	    //Request a token
	    $display("Requesting a new token.");
	    func.tokgen.request.put(tuple3(?, ?, baseTick));
	    madeReq <= True;
	  end
	else
	  begin
	    //Get the response
	    match {.tok, .*} <- func.tokgen.response.get();
	    $display("Responded with token %0d.", tok);
	    cur_tok <= tok;
	    
	    stage <= FET;
	    madeReq <= False;
	  end
      FET:
        if (!madeReq)
	  begin
	    //Fetch next instruction
	    $display("Fetching token %0d at address %h", cur_tok, pc);
            func.fetch.request.put(tuple3(cur_tok, pc, baseTick));
	    madeReq <= True;
	  end
	else
	  begin
	    //Get the response
            match {.tok, .inst} <- func.fetch.response.get();
	    
	    if (tok != cur_tok) $display ("FET ERROR");
	    
	    stage <= DEC;
	    madeReq <= False;
	  end
      DEC:
        if (!madeReq)
	  begin
	    //Decode current inst
            func.decode.request.put(tuple3(cur_tok, ?, baseTick));
	    madeReq <= True;
	  end
	else
	  begin
 	    //Get the response
            match {.tok, .deps} <- func.decode.response.get();
	    
	    if (tok != cur_tok) $display ("DEC ERROR");
	    
	    stage <= EXE;
	    madeReq <= False;
	  end
      EXE:
        if (!madeReq)
	  begin
	    //Execute instruction
            func.execute.request.put(tuple3(cur_tok, ?, baseTick));
	    madeReq <= True;
	  end
	else
	  begin
 	    //Get the response
            match {.tok, .res} <- func.execute.response.get();
	    
	    if (tok != cur_tok) $display ("EXE ERROR");
	   	
	    case (res) matches
	      tagged RBranchTaken .addr:
	      begin
	        $display("Branch taken to address %h", addr);
	   	pc <= addr;
	      end
              tagged RBranchNotTaken:
	   	pc <= pc + 1;
              tagged RNop:
	   	pc <= pc + 1;
              tagged RTerminate:
	   	running <= False;
	    endcase
	    
	    stage <= MEM;
	    madeReq <= False;
	  end
      MEM:
        if (!madeReq)
	  begin
	    //Request memory ops
            func.memory.request.put(tuple3(cur_tok, ?, baseTick));
	    madeReq <= True;
	  end
	else
	  begin
 	    //Get the response
	    match {.tok, .*} <- func.memory.response.get();
	    
	    if (tok != cur_tok) $display ("MEM ERROR");
	    
	    stage <= LCO;
	    madeReq <= False;
	  end
      LCO:
        if (!madeReq)
	  begin
	    //Request memory ops
            func.local_commit.request.put(tuple3(cur_tok, ?, baseTick));
	    madeReq <= True;
	  end
	else
	  begin
 	    //Get the response
  
            match {.tok, .*} <- func.local_commit.response.get();
	    
	    if (tok != cur_tok) $display ("LCO ERROR");
	    
	    stage <= GCO;
	    madeReq <= False;
	  end
      GCO:
        if (!madeReq)
	  begin
	    //Request memory ops
            func.global_commit.request.put(tuple3(cur_tok, ?, baseTick));
	    madeReq <= True;
	  end
	else
	  begin
 	    //Get the response
  
            match {.tok, .*} <- func.global_commit.response.get();
	    
	    if (tok != cur_tok) $display ("GCO ERROR");
	    
	    $display("Committed token %0d", cur_tok);
	    
	    stage <= GCO;
	    madeReq <= False;
	  end
    endcase    
  endrule
  
  method Action start();
  
    running <= True;
    
  endmethod
  
  method Bool done();
  
    return !running;
    
  endmethod

endmodule

module [Module] mkTP_TestOld#(FunctionalPartition#(Tick, Token,
                				Addr, Inst,
						void, DepInfo,
						void, InstResult,
						void, void,
						void, void,
						void, void) func) (TimingPartition);
  
  //Latencies
  Tick fet_L = 1;
  Tick dec_L = 1;
  Tick exe_L = 1;
  Tick mem_L = 1;
  Tick lco_L = 1;
  Tick gco_L = 1;
  

  Reg#(Bool) running <- mkReg(False);

  Reg#(Addr) pc <- mkReg(0);
  Reg#(Tick) baseTick <- mkReg(0);
  
  RegFile#(Token, Maybe#(Tick))   tbl_valids   <- mkRegFileFull();	  
  RegFile#(Token, Maybe#(Tick))   tbl_fetch    <- mkRegFileFull();	  
  RegFile#(Token, Maybe#(Tick))   tbl_decode   <- mkRegFileFull();	  
  RegFile#(Token, Maybe#(Tick))   tbl_execute  <- mkRegFileFull();	  
  RegFile#(Token, Maybe#(Tick))   tbl_memory   <- mkRegFileFull();	  
  RegFile#(Token, Maybe#(Tick))   tbl_lcommit  <- mkRegFileFull();	  
  RegFile#(Token, Maybe#(Tick))   tbl_gcommit  <- mkRegFileFull();	  
  RegFile#(Token, Inst)           tbl_inst     <- mkRegFileFull();
  RegFile#(Token, DepInfo)        tbl_depinfo  <- mkRegFileFull();
  RegFile#(Token, InstResult)     tbl_result   <- mkRegFileFull();
  
  FIFO#(Token) retireQ <- mkFIFO();
  
  rule tok_req (running);
    func.tokgen.request.put(tuple3(?, ?, baseTick));
  endrule
  
  
  rule tok_resp_fet_req (running);
  
    match {.tok, .*} <- func.tokgen.response.get();
    
    tbl_valids.upd(tok, Just baseTick);
    
    func.fetch.request.put(tuple3(tok, pc, baseTick));
        
    pc <= pc + 1;
    baseTick <= baseTick + 1;
    
  endrule
  
  
  rule fet_resp_dec_req (running);
  
    match {.tok, .inst} <- func.fetch.response.get();
    
    
    case (tbl_valids.sub(tok)) matches
      tagged Nothing:
        $display("ERROR: No valid tick for fetch on token %0h", tok);
      tagged Valid .oldtick: 
      begin
        Tick newTick = oldtick + fet_L;
        tbl_fetch.upd(tok, Just newTick);
        tbl_inst.upd(tok, inst);
        func.decode.request.put(tuple3(tok, ?, newTick));
      end
    endcase
         
  endrule
  
  
  rule dec_resp_exe_req (running);
  
    match {.tok, .deps} <- func.decode.response.get();
    
    case (tbl_fetch.sub(tok)) matches
      tagged Nothing:
        $display("ERROR: No fetch tick for decode on token %0h", tok);
      tagged Valid .oldtick: 
      begin
        Tick newTick = oldtick + dec_L;
        tbl_decode.upd(tok, Just newTick);
        tbl_depinfo.upd(tok, deps);
        func.execute.request.put(tuple3(tok, ?, newTick));
      end
    endcase
    
  endrule


  rule exe_resp_mem_req (running);
  
    match {.tok, .res} <- func.execute.response.get();
    
    case (tbl_decode.sub(tok)) matches
      tagged Nothing:
        $display("ERROR: No decode tick for exec on token %0h", tok);
      tagged Valid .oldtick: 
      begin
        Tick newTick = oldtick + exe_L;
        tbl_execute.upd(tok, Just newTick);
        tbl_result.upd(tok, res);
        func.memory.request.put(tuple3(tok, ?, newTick));
		
	case (res) matches
	  tagged RBranchTaken .addr:
	    noAction; //XXX Branch Here
          tagged RBranchNotTaken:
	    noAction;
          tagged RNop:
	    noAction;
          tagged RTerminate:
	    running <= False;
	endcase
      end
    endcase
    
  endrule
  
  rule mem_resp_lco_req (running);
  
    match {.tok, .res} <- func.memory.response.get();
    
    case (tbl_memory.sub(tok)) matches
      tagged Nothing:
        $display("ERROR: No exec tick for mem on token %0h", tok);
      tagged Valid .oldtick: 
      begin
        Tick newTick = oldtick + mem_L;
        tbl_memory.upd(tok, Just newTick);
        func.local_commit.request.put(tuple3(tok, ?, newTick));
      end
    endcase
    
  endrule
  
  rule lco_resp_gco_req (running);
  
    match {.tok, .*} <- func.local_commit.response.get();
    
    case (tbl_lcommit.sub(tok)) matches
      tagged Nothing:
        $display("ERROR: No mem tick for lcommit on token %0h", tok);
      tagged Valid .oldtick: 
      begin
        Tick newTick = oldtick + lco_L;
        tbl_lcommit.upd(tok, Just newTick);
        func.global_commit.request.put(tuple3(tok, ?, newTick));
      end
    endcase
    
  endrule
  
  rule gco_resp (running);
  
    match {.tok, .*} <- func.global_commit.response.get();
    
    case (tbl_gcommit.sub(tok)) matches
      tagged Nothing:
        $display("ERROR: No lcommit tick for gcommit on token %0h", tok);
      tagged Valid .oldtick: 
      begin
        Tick newTick = oldtick + gco_L;
        tbl_gcommit.upd(tok, Just newTick);
	retireQ.enq(tok);
      end
    endcase
    
  endrule
  
  rule retire (running);
    
    Token tok = retireQ.first();
    retireQ.deq();
    
    $display("Committed:\t%0d\t%0d\t%0d\t%0d\t%0d\t%0d\t%0d", 
              validValue(tbl_valids.sub(tok)),
              validValue(tbl_fetch.sub(tok)), 
              validValue(tbl_decode.sub(tok)), 
              validValue(tbl_execute.sub(tok)), 
	      validValue(tbl_memory.sub(tok)),
	      validValue(tbl_lcommit.sub(tok)),
	      validValue(tbl_gcommit.sub(tok)));
    
    tbl_valids.upd(tok, Nothing);
    tbl_fetch.upd(tok, Nothing);
    tbl_decode.upd(tok, Nothing);
    tbl_execute.upd(tok, Nothing);
    tbl_memory.upd(tok, Nothing);
    tbl_lcommit.upd(tok, Nothing);
    tbl_gcommit.upd(tok, Nothing);
    
  endrule
  
  method Action start();
  
    running <= True;
    
  endmethod
  
  method Bool done();
  
    return !running;
    
  endmethod
  
endmodule
