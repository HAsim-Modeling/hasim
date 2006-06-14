//BSV library imports
import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

//HASim library imports
import HASim::*;
import Debug::*;

//Model-specific imports
import ISA::*;

`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Timing"


//************************* Simple Timing Partition ***********************//
//                                                    `                    //
// This is about the simplest timing partition you can conceive of. It     //
// simply fetches one instruction at a time, executes it, then moves to    //
// the next instruction. This can serve as a good mechanism to verify      //
// the functional partition and can serve as a "golden model" for more     //
// complex timing partitions.                                              //
//                                                    `                    //
//*************************************************************************//



typedef enum 
{ 
  TOK, FET, DEC, EXE, MEM, LCO, GCO 
} 
  Stage deriving (Eq, Bits);

`define MODULE_NAME "mkChip"
module [HASim_Module] mkChip
     //interface:
                 (TModule#(Command, Response));

  
  //********* State Elements *********//
  
  //Are we running the program or not?
  Reg#(Bool) running <- mkReg(False);
  
  //Have we run the program or not?
  Reg#(Bool) ran <- mkReg(False);
  
  //Have we made a req to FP and are waiting for a response?
  Reg#(Bool) madeReq <- mkReg(False);
  
  //The current stage
  Reg#(Stage) stage <- mkReg(TOK);
  
  //Current token (response from TOK stage)
  Reg#(Token) cur_tok <- mkReg(0);
  
  //Current instruction (response from FET stage)
  Reg#(Inst)  cur_inst <- mkRegU();
  
  //The Program Counter
  Reg#(Addr) pc <- mkReg(0);
  
  //The simulation Clock Cycle, or "tick"
  Reg#(Tick) baseTick <- mkReg(0);
  
  //********* Ports *********//
  
  Connection_Client#(Tuple2#(Bit#(8), Tick), Token)
  //...
  link_to_tok <- mkConnection_Client("fp_tok");
  
  Connection_Client#(Tuple3#(Token, Tick, Addr),
                     Tuple2#(Token, Inst))
  //...
  link_to_fet <- mkConnection_Client("fp_fet");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, DepInfo))
  //...
  link_to_dec <- mkConnection_Client("fp_dec");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, InstResult))
  //...
  link_to_exe <- mkConnection_Client("fp_exe");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, void))
  //...
  link_to_mem <- mkConnection_Client("fp_mem");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, void))
  //...
  link_to_lco <- mkConnection_Client("fp_lco");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, void))
  //...
  link_to_gco <- mkConnection_Client("fp_gco");

  //Connection_Send#(Token) 
  //...
  //link_to_killToken <- mkConnection_Send("fp_killToken");

  
  //********* Rules *********//
  
  //process
  
  rule process (running);
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
	    link_to_tok.makeReq(tuple2(17, baseTick));
	    
	    madeReq <= True;
	    
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
	    let tok <- link_to_tok.getResp();
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
	    
	    if (tok != cur_tok) $display ("FET ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok, tok);
	    
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
	    
	    if (tok != cur_tok) $display ("DEC ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok, tok);
	    
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
	    
	    if (tok != cur_tok) $display ("EXE ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok, tok);
	   	
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
	    
	    if (tok != cur_tok) $display ("MEM ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok, tok);
	    
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
	    
	    if (tok != cur_tok) $display ("LCO ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok, tok);
	    
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
	    
	    if (tok != cur_tok) $display ("GCO ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok, tok);
	    
	    debug(1, $display("Committed token %0d", cur_tok));
	    
	    stage <= TOK;
	    madeReq <= False;
	    baseTick <= baseTick + 1;
	  end
      end
    endcase    
  endrule

  method Action exec(Command c);

    running <= True;
    ran <= True;
    
  endmethod

  method ActionValue#(Response) response() if (ran && !running);
    ran <= False;
    return RESP_DoneRunning;

  endmethod

  
endmodule
`undef MODULE_NAME
