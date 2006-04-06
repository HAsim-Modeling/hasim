import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import ToyMIPS::*;
import TOY_FunctionalPartition::*;
import Debug::*;

`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Timing"

module [Module] mkCPU_Test#(Memory#(TOY_Addr, TOY_Inst, Value, TOY_Token) mem) (CPU);

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

 

`define MODULE_NAME "mkTP_Test"
module [Module] mkTP_Test#(FunctionalPartition#(TOY_Tick, TOY_Token,
                				TOY_Addr, TOY_Inst,
						void, TOY_DepInfo,
						void, TOY_InstResult,
						void, void,
						void, void,
						void, void) func) (TimingPartition);
  
  Reg#(Bool) running <- mkReg(False);
  
  Reg#(Bool) madeReq <- mkReg(False);
  
  Reg#(Stage) stage <- mkReg(TOK);
  
  Reg#(TOY_Token) cur_tok <- mkReg(0);
  Reg#(TOY_Inst)  cur_inst <- mkRegU();
  
  Reg#(TOY_Addr) pc <- mkReg(0);
  
  Reg#(TOY_Tick) baseTick <- mkReg(0);
  
  rule process (running);
    debug_rule("process");
    
    baseTick <= baseTick + 1;

    case (stage)
      TOK:
      begin
        debug_case("stage", "TOK");
	
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request a token
	    debug(2, $display("%h: Requesting a new token.", baseTick));
	    func.tokgen.request.put(tuple3(?, ?, baseTick));
	    
	    madeReq <= True;
	    
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
	    match {.tok, .*} <- func.tokgen.response.get();
	    debug(2, $display("%h: TOK Responded with token %0d.", baseTick, tok));
	    
	    cur_tok <= tok;
	    
	    stage <= FET;
	    madeReq <= False;
	  end
      end
      FET:
      begin
        debug_case("stage", "FET");
	
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Fetch next instruction
	    debug(2, $display("%h: Fetching token %0d at address %h", baseTick, cur_tok, pc));
            func.fetch.request.put(tuple3(cur_tok, pc, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
            match {.tok, .inst} <- func.fetch.response.get();
	    debug(2, $display("%h: FET Responded with token %0d.", baseTick, tok));
	    
	    if (tok != cur_tok) $display ("FET ERROR");
	    
	    stage <= DEC;
	    madeReq <= False;
	  end
      end
      DEC:
      begin
        debug_case("stage", "DEC");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Decode current inst
	    debug(2, $display("%h: Decoding token %0d", baseTick, cur_tok));
            func.decode.request.put(tuple3(cur_tok, ?, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .deps} <- func.decode.response.get();
	    debug(2, $display("%h: DEC Responded with token %0d.", baseTick, tok));
	    
	    case (deps.dest) matches
	      tagged Valid {.rname, .prname}:
	        debug(2, $display("Destination: (%d, %d)", rname, prname));
	      tagged Invalid:
	        debug(2, $display("No destination."));
	    endcase
	    
	    case (deps.src1) matches
	      tagged Valid {.rname, .prname}:
	        debug(2, $display("Source 1: (%d, %d)", rname, prname));
	      tagged Invalid:
	        debug(2, $display("No Source 1."));
	    endcase
	    
	    case (deps.src2) matches
	      tagged Valid {.rname, .prname}:
	        debug(2, $display("Source 2: (%d, %d)", rname, prname));
	      tagged Invalid:
	        debug(2, $display("No Source 2."));
	    endcase
	    
	    if (tok != cur_tok) $display ("DEC ERROR");
	    
	    stage <= EXE;
	    madeReq <= False;
	  end
      end
      EXE:
      begin
        debug_case("stage", "EXE");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    //Execute instruction
	    debug(2, $display("%h: Executing token %0d", baseTick, cur_tok));
            func.execute.request.put(tuple3(cur_tok, ?, baseTick));
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .res} <- func.execute.response.get();
	    debug(2, $display("%h: EXE Responded with token %0d.", baseTick, tok));
	    
	    if (tok != cur_tok) $display ("EXE ERROR");
	   	
	    case (res) matches
	      tagged RBranchTaken .addr:
	      begin
	        debug(2, $display("Branch taken to address %h", addr));
	   	pc <= addr;
	      end
              tagged RBranchNotTaken:
	      begin
	        debug(2, $display("Branch not taken"));
	   	pc <= pc + 1;
	      end
              tagged RNop:
	      begin
	        debug(2, $display("Nop"));
	   	pc <= pc + 1;
	      end
              tagged RTerminate:
	      begin
	        debug(2, $display("Terminating Execution"));
	   	running <= False;
	      end
	    endcase
	    
	    stage <= MEM;
	    madeReq <= False;
	  end
      end
      MEM:
      begin
        debug_case("stage", "MEM");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request memory ops
	    debug(2, $display("%h: Memory ops for token %0d", baseTick, cur_tok));
            func.memory.request.put(tuple3(cur_tok, ?, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
	    match {.tok, .*} <- func.memory.response.get();
	    debug(2, $display("%h: MEM Responded with token %0d.", baseTick, tok));
	    
	    if (tok != cur_tok) $display ("MEM ERROR");
	    
	    stage <= LCO;
	    madeReq <= False;
	  end
      end
      LCO:
      begin
        debug_case("stage", "LCO");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request memory ops
	    debug(2, $display("%h: Locally committing token %0d", baseTick, cur_tok));
            func.local_commit.request.put(tuple3(cur_tok, ?, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
  
            match {.tok, .*} <- func.local_commit.response.get();
	    debug(2, $display("%h: LCO Responded with token %0d.", baseTick, tok));
	    
	    if (tok != cur_tok) $display ("LCO ERROR");
	    
	    stage <= GCO;
	    madeReq <= False;
	  end
      end
      GCO:
      begin
        debug_case("stage", "GCO");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request memory ops
	    debug(2, $display("%h: Globally committing token %0d", baseTick, cur_tok));
            func.global_commit.request.put(tuple3(cur_tok, ?, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .*} <- func.global_commit.response.get();
	    debug(2, $display("%h: GCO Responded with token %0d.", baseTick, tok));
	    
	    if (tok != cur_tok) $display ("GCO ERROR");
	    
	    debug(1, $display("Committed token %0d", cur_tok));
	    
	    stage <= TOK;
	    madeReq <= False;
	  end
      end
    endcase    
  endrule
  
  method Action start();

    debug_method("start");
    running <= True;
    
  endmethod
  
  method Bool done();
  
    return !running;
    
  endmethod

endmodule
`undef MODULE_NAME

module [Module] mkTP_TestOld#(FunctionalPartition#(TOY_Tick, TOY_Token,
                				TOY_Addr, TOY_Inst,
						void, TOY_DepInfo,
						void, TOY_InstResult,
						void, void,
						void, void,
						void, void) func) (TimingPartition);
  
  //Latencies
  TOY_Tick fet_L = 1;
  TOY_Tick dec_L = 1;
  TOY_Tick exe_L = 1;
  TOY_Tick mem_L = 1;
  TOY_Tick lco_L = 1;
  TOY_Tick gco_L = 1;
  

  Reg#(Bool) running <- mkReg(False);

  Reg#(TOY_Addr) pc <- mkReg(0);
  Reg#(TOY_Tick) baseTick <- mkReg(0);
  
  RegFile#(TOY_Token, Maybe#(TOY_Tick))   tbl_valids   <- mkRegFileFull();	  
  RegFile#(TOY_Token, Maybe#(TOY_Tick))   tbl_fetch    <- mkRegFileFull();	  
  RegFile#(TOY_Token, Maybe#(TOY_Tick))   tbl_decode   <- mkRegFileFull();	  
  RegFile#(TOY_Token, Maybe#(TOY_Tick))   tbl_execute  <- mkRegFileFull();	  
  RegFile#(TOY_Token, Maybe#(TOY_Tick))   tbl_memory   <- mkRegFileFull();	  
  RegFile#(TOY_Token, Maybe#(TOY_Tick))   tbl_lcommit  <- mkRegFileFull();	  
  RegFile#(TOY_Token, Maybe#(TOY_Tick))   tbl_gcommit  <- mkRegFileFull();	  
  RegFile#(TOY_Token, TOY_Inst)           tbl_inst     <- mkRegFileFull();
  RegFile#(TOY_Token, TOY_DepInfo)        tbl_depinfo  <- mkRegFileFull();
  RegFile#(TOY_Token, TOY_InstResult)     tbl_result   <- mkRegFileFull();
  
  FIFO#(TOY_Token) retireQ <- mkFIFO();
  
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
        TOY_Tick newTick = oldtick + fet_L;
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
        TOY_Tick newTick = oldtick + dec_L;
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
        TOY_Tick newTick = oldtick + exe_L;
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
        TOY_Tick newTick = oldtick + mem_L;
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
        TOY_Tick newTick = oldtick + lco_L;
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
        TOY_Tick newTick = oldtick + gco_L;
        tbl_gcommit.upd(tok, Just newTick);
	retireQ.enq(tok);
      end
    endcase
    
  endrule
  
  rule retire (running);
    
    TOY_Token tok = retireQ.first();
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
