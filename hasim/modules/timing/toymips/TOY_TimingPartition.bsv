import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import TOY_Datatypes::*;
import Ports::*;
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
                 (TimingPartition#(TOY_Tick, TOY_Token,  //tick type, token type
		                   void, void,  	 //tokenReq, tokenResp,
  				   TOY_Addr, TOY_Inst,   //fetchReq, fetchResp
  				   void, TOY_DepInfo,	 //decodeReq, decodeResp
  				   void, TOY_InstResult, //execReq, execResp
  				   void, void,  	 //memReq, memResp
  				   void, void,  	 //lcommitReq, lcommitResp
  				   void, void));	 //gcommitReq, gcommitResp  

  
  //********* State Elements *********//
  
  //Are we running or not?
  Reg#(Bool) running <- mkReg(False);
  
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
  
  let port_to_tok <- mkPort_Client("fp_tok");
  let port_to_fet <- mkPort_Client("fp_fet");
  let port_to_dec <- mkPort_Client("fp_dec");
  let port_to_exe <- mkPort_Client("fp_exe");
  let port_to_mem <- mkPort_Client("fp_mem");
  let port_to_lco <- mkPort_Client("fp_lco");
  let port_to_gco <- mkPort_Client("fp_gco");

  let port_to_killToken <- mkPort_Send("fp_killToken");

  
  //********* Rules *********//
  
  //process
  
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
	    port_to_tok.makeReq(tuple3(?, ?, baseTick));
	    
	    madeReq <= True;
	    
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
	    match {.tok, .*} <- port_to_tok.getResp();
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
            port_to_fet.makeReq(tuple3(cur_tok, pc, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
            match {.tok, .inst} <- port_to_fet.getResp();
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
            port_to_dec.makeReq(tuple3(cur_tok, ?, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .deps} <- port_to_dec.getResp();
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
            port_to_exe.makeReq(tuple3(cur_tok, ?, baseTick));
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .res} <- port_to_exe.getResp();
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
            port_to_mem.makeReq(tuple3(cur_tok, ?, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
	    match {.tok, .*} <- port_to_mem.getResp();
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
            port_to_lco.makeReq(tuple3(cur_tok, ?, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
  
            match {.tok, .*} <- port_to_lco.getResp();
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
            port_to_gco.makeReq(tuple3(cur_tok, ?, baseTick));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .*} <- port_to_gco.getResp();
	    debug(2, $display("%h: GCO Responded with token %0d.", baseTick, tok));
	    
	    if (tok != cur_tok) $display ("GCO ERROR");
	    
	    debug(1, $display("Committed token %0d", cur_tok));
	    
	    stage <= TOK;
	    madeReq <= False;
	  end
      end
    endcase    
  endrule
  
  //Interface for CPU Ports
  interface Put start;

    method Action put(void v);

      debug_method("start");
      running <= True;

    endmethod
  
  endinterface
  
  
  interface Get done;

    method ActionValue#(Bool) get();

      noAction;
      return !running;

    endmethod

  endinterface

  
  //Interface for Functional Partition Ports
  
  interface tokgen = port_to_tok.client;
  interface fetch = port_to_fet.client;
  interface decode = port_to_dec.client;
  interface execute = port_to_exe.client;
  interface memory = port_to_mem.client;
  interface local_commit = port_to_lco.client;
  interface global_commit = port_to_gco.client;
  
  interface killToken = port_to_killToken.outgoing;

endmodule
`undef MODULE_NAME
