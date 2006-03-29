import ConfigReg::*;
import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import Datatypes::*;
import FunctionalPartition::*;

module [Module] mkCPU_Pipe#(Memory#(Addr, Inst, Value, Token) mem) (CPU);

  let fp <- mkFP_Test(mem);
  let tp <- mkTP_Test_Pipe(fp);
  
  method start = tp.start;
  method done = tp.done;

endmodule

module [Module] mkTP_Test_Pipe#(FunctionalPartition#(Tick, Token,
                				Addr, Inst,
						void, DepInfo,
						void, InstResult,
						void, void,
						void, void,
						void, void) func) (TimingPartition);


  void unit = ?;
  Reg#(Tick) baseTick <- mkReg(0);

  Reg#(Maybe#(Token)) mstopToken <- mkConfigReg(Nothing);
  Reg#(Bool) running <- mkReg(False);
  Reg#(Addr) pc <- mkReg(0);

  FIFO#(Token) tok2fetQ <- mkLFIFO();
  FIFO#(Tuple2#(Token,Addr)) fet2decQ <- mkFIFO();
  FIFO#(Tuple3#(Token,Addr,DepInfo)) dec2exeQ <- mkFIFO();
  FIFO#(Tuple2#(Token,DepInfo)) exe2memQ <- mkLFIFO();
  FIFO#(Tuple2#(Token,DepInfo)) mem2lcoQ <- mkLFIFO();
  FIFO#(Token) lco2gcoQ <- mkLFIFO();
  FIFO#(Token) gco2tokQ <- mkLFIFO();

  rule tick(True);
    baseTick <= baseTick + 1;   
  endrule 

  rule tokenReq(True);
    $display("%h: Requesting a new token.", baseTick);
    func.tokgen.request.put(tuple3(?,unit,baseTick));
  endrule
  
  rule tokenGen(True);
    match {.tok,.*} <- func.tokgen.response.get();
    $display("%h: Fetching token %0d at address %h", baseTick, tok, pc);
    pc <= pc + 1;
    tok2fetQ.enq(tok);
    func.fetch.request.put(tuple3(tok, pc, baseTick));
  endrule

  rule fetch(True);
    match {.tok,.inst} <- func.fetch.response.get();
    $display("%h: Decoding token %0d", baseTick, tok);
    tok2fetQ.deq();
    fet2decQ.enq(tuple2(tok,pc));
    if (tok != tok2fetQ .first)
       $display ("FET ERROR");
    func.decode.request.put(tuple3(tok, unit, baseTick));
  endrule

  rule decode(True);
    match {.tok, .deps} <- func.decode.response.get();
    match {.ftok,.fpc} = fet2decQ.first();
    fet2decQ.deq();
    $display("%h: DEC Responded with token %0d.", baseTick, tok);
    case (deps.dest) matches
      tagged Valid {.rname, .prname}:
	$display("Destination: (%d, %d)", rname, prname);
      tagged Invalid:
	$display("No destination.");
    endcase
    case (deps.src1) matches
      tagged Valid {.rname, .prname}:
        $display("Source 1: (%d, %d)", rname, prname);
      tagged Invalid:
        $display("No Source 1.");
    endcase
    case (deps.src2) matches
      tagged Valid {.rname, .prname}:
        $display("Source 2: (%d, %d)", rname, prname);
      tagged Invalid:
        $display("No Source 2.");
    endcase  
    dec2exeQ.enq(tuple3(tok, fpc,deps));
    func.execute.request.put(tuple3(tok, unit, baseTick));
  endrule

  rule execute(True);
    match {.tok, .res} <- func.execute.response.get();
    match {.ftok,.fpc,.fdeps} = dec2exeQ.first();
    dec2exeQ.deq();
    $display("%h: Executing token %0d", baseTick, tok);

    case (res) matches
      tagged RBranchTaken .addr:
	begin
	  $display("Branch taken to address %h", addr);
	  pc <= addr;
          func.killToken(tok); //mispred in FP
          fet2decQ.clear(); // clear out wrongpath TP insts
          //dec2exeQ.clear(); //YYY: this happens automatically
	end
      tagged RBranchNotTaken:
        noAction;
      tagged RNop:
        noAction;
      tagged RTerminate:
	mstopToken <= Just(tok);
    endcase
    exe2memQ.enq(tuple2(tok, fdeps));
    func.memory.request.put(tuple3(tok, unit, baseTick));
  endrule

  rule memory(True);
    match {.tok, .*} <- func.memory.response.get();
    match {.ftok,.fdeps} = exe2memQ.first();
    exe2memQ.deq();
     $display("%h: MEM Responded with token %0d.", baseTick, tok);
    mem2lcoQ.enq(tuple2(tok, fdeps));
    func.local_commit.request.put(tuple3(tok, unit, baseTick));
  endrule

  rule local_commit(True);
    match {.tok, .*} <- func.local_commit.response.get();
    match {.ftok,.fdeps} = mem2lcoQ.first();
    mem2lcoQ.deq();
     $display("%h: LCO Responded with token %0d.", baseTick, tok);
    lco2gcoQ.enq(tok);
    func.global_commit.request.put(tuple3(tok, unit, baseTick));
  endrule

  rule global_commit(True);
    match {.tok, .*} <- func.global_commit.response.get();
    match {.ftok} = lco2gcoQ.first();
    lco2gcoQ.deq();
    $display("%h: GCO Responded with token %0d.", baseTick, tok);
    func.global_commit.request.put(tuple3(tok, unit, baseTick));
  endrule

  rule finishToken(True);
    match{.tok,.*} <- func.global_commit.response.get();
    $display(" %h: finished token %0d", baseTick, tok);
    if(Just(tok) == mstopToken)
      running <= False;
  endrule
  
  method Action start();
    running <= True;
  endmethod
  
  method Bool done();
    return !running;
  endmethod

endmodule