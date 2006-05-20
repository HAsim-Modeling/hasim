import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import TOY_Datatypes::*;
import Debug::*;

`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Timing"


/************************** Simple Timing Partition ************************/
/*                                                    `                    */
/* This is about the simplest timing partition you can conceive of. It     */
/* simply fetches one instruction at a time, executes it, then moves to    */
/* the next instruction. This can serve as a good mechanism to verify      */
/* the functional partition and can serve as a "golden model" for more     */
/* complex timing partitions.                                              */
/*                                                    `                    */
/***************************************************************************/



typedef enum 
{ 
  TOK, FET, DEC, EXE, MEM, LCO, GCO 
} 
  Stage deriving (Eq, Bits);

//mkTOY_TP_Simple :: FunctionalPartition -> Timing Partition 

(* synthesize *)
`define MODULE_NAME "mkTOY_TP_Simple"
module [Module] mkTOY_TP_Simple
     //interface:
                 (TimingPartition#(TOY_Tick, void, void));

  
  //********* State Elements *********//
  
  //Are we running the program or not?
  Reg#(Bool) running <- mkReg(False);
  
  //Are we in the middle of simulating a clock cycle or not?
  Reg#(Bool) simulating <- mkReg(False);
  
  //Have we made a req to FP and are waiting for a response?
  Reg#(Bool) madeReq <- mkReg(False);
  
  //The current stage
  Reg#(Stage) stage <- mkReg(TOK);
  
  //Current token (response from TOK stage)
  Reg#(TOY_Token) cur_tok <- mkReg(0);
  
  //Current instruction (response from FET stage)
  Reg#(TOY_Inst)  cur_inst <- mkRegU();
  
  //The Program Counter
  Reg#(TOY_Addr) pc <- mkReg(0);
  
  //The simulation Clock Cycle, or "tick"
  Reg#(TOY_Tick) baseTick <- mkReg(0);
  
  //********* Ports *********//
  
  Link_Client#(Tuple3#(TOY_Token, TOY_Tick, void),
               Tuple2#(TOY_Token, void))             link_to_tok <- mkLink_Client("fp_tok");
  Link_Client#(Tuple3#(TOY_Token, TOY_Tick, TOY_Addr),
               Tuple2#(TOY_Token, TOY_Inst))       link_to_fet <- mkLink_Client("fp_fet");
  Link_Client#(Tuple3#(TOY_Token, TOY_Tick, void),
               Tuple2#(TOY_Token, TOY_DepInfo))    link_to_dec <- mkLink_Client("fp_dec");
  Link_Client#(Tuple3#(TOY_Token, TOY_Tick, void),
               Tuple2#(TOY_Token, TOY_InstResult)) link_to_exe <- mkLink_Client("fp_exe");
  Link_Client#(Tuple3#(TOY_Token, TOY_Tick, void),
               Tuple2#(TOY_Token, void))             link_to_mem <- mkLink_Client("fp_mem");
  Link_Client#(Tuple3#(TOY_Token, TOY_Tick, void),
               Tuple2#(TOY_Token, void))             link_to_lco <- mkLink_Client("fp_lco");
  Link_Client#(Tuple3#(TOY_Token, TOY_Tick, void),
               Tuple2#(TOY_Token, void))             link_to_gco <- mkLink_Client("fp_gco");

  Link_Send#(TOY_Token) link_to_killToken <- mkLink_Send("fp_killToken");

  
  //********* Rules *********//
  
  //process
  
  rule process (running && simulating);
    debug_rule("process");
    
    case (stage)
      TOK:
      begin
        debug_case("stage", "TOK");
	
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request a token
	    debug(2, $display("%h: Requesting a new token.", baseTick));
	    link_to_tok.makeReq(tuple3(?, baseTick, ?));
	    
	    madeReq <= True;
	    
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
	    match {.tok, .*} <- link_to_tok.getResp();
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
            link_to_fet.makeReq(tuple3(cur_tok, baseTick, pc));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
            match {.tok, .inst} <- link_to_fet.getResp();
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
            link_to_dec.makeReq(tuple3(cur_tok, baseTick, ?));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .deps} <- link_to_dec.getResp();
	    debug(2, $display("%h: DEC Responded with token %0d.", baseTick, tok));
	    
	    case (deps.dep_dest) matches
	      tagged Valid {.rname, .prname}:
	        debug(2, $display("Destination: (%d, %d)", rname, prname));
	      tagged Invalid:
	        debug(2, $display("No destination."));
	    endcase
	    
	    case (deps.dep_src1) matches
	      tagged Valid {.rname, .prname}:
	        debug(2, $display("Source 1: (%d, %d)", rname, prname));
	      tagged Invalid:
	        debug(2, $display("No Source 1."));
	    endcase
	    
	    case (deps.dep_src2) matches
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
            link_to_exe.makeReq(tuple3(cur_tok, baseTick, ?));
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .res} <- link_to_exe.getResp();
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
            link_to_mem.makeReq(tuple3(cur_tok, baseTick, ?));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
	    match {.tok, .*} <- link_to_mem.getResp();
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
            link_to_lco.makeReq(tuple3(cur_tok, baseTick, ?));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
  
            match {.tok, .*} <- link_to_lco.getResp();
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
            link_to_gco.makeReq(tuple3(cur_tok, baseTick, ?));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .*} <- link_to_gco.getResp();
	    debug(2, $display("%h: GCO Responded with token %0d.", baseTick, tok));
	    
	    if (tok != cur_tok) $display ("GCO ERROR");
	    
	    debug(1, $display("Committed token %0d", cur_tok));
	    
	    stage <= TOK;
	    madeReq <= False;
	    simulating <= False;
	  end
      end
    endcase    
  endrule
  
  //TModule Interface
  method Action tick(TOY_Tick t);

    simulating <= True;
    baseTick <= t;

  endmethod

  method Bool done();

    return !simulating;

  endmethod

  method Action exec(void v);

    running <= True;

  endmethod

  method void exec_response() if (!running);

    return ?;

  endmethod

  
endmodule
`undef MODULE_NAME
