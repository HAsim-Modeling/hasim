//Memory system with connections

import HASim::*;
import ISA::*;

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;
import BypassFIFO::*;

import FUNCP_StoreBuffer::*;

/************* Memory System Interface *************/


// Data Memory request

typedef union tagged 
{
  struct {Token token; Addr addr;            } Ld;
  struct {Token token; Addr addr; Value val; } St;
}
  MemReq 
    deriving
            (Eq, Bits);


// Data Memory Response

typedef union tagged {
  Value LdResp;
  void  StResp;
}
  MemResp 
    deriving
            (Eq, Bits);


// Memory System Interface

// The memory system consists of two major parts: the IMem and DMem.
// The IMem is a simple Server (Address, Instruction)
// The DMem uses the above MemReq/MemResp types
// Additionally requests can be committed or killed in the DMem
// They are committed by Global Commit and killed by killToken

// For now the memory also has a "magic" link for the controller to
// load the test case. This may disappear in the future.


/************* Simple Memory System Implementation *************/

// This is intended for software simulation. An FPGA version would
// be a memory controller.

module [HASim_Module] mkMem_Software ()
    provisos
            (Bits#(Token, token_SZ),
	     Transmittable#(Addr),
	     Transmittable#(Inst),
	     Transmittable#(Value),
	     Transmittable#(Token),
	     Transmittable#(MemReq),
	     Transmittable#(MemResp),
	     Transmittable#(Tuple2#(Token, Token)));
	    
  SoftAddr maxSoftAddr = maxBound();
  Addr maxAddr = zeroExtend(maxSoftAddr);

  //State elements

  RegFile#(SoftAddr, Inst)  imemory <- mkRegFileFull();
  RegFile#(SoftAddr, Value) dmemory <- mkRegFileFull();

  FIFO#(Tuple2#(Token, Value))  waitingQ <- mkFIFO();

  StoreBuffer st_buffer <- mkFUNCP_StoreBuffer();

  //Connections

  Connection_Server#(Addr, Inst)      link_imem      <- mkConnection_Server("mem_imem");
  Connection_Server#(MemReq, MemResp) link_dmem      <- mkConnection_Server("mem_dmem");
  Connection_Receive#(Token)          link_commit    <- mkConnection_Receive("mem_commit");
  Connection_Receive#(Token)          link_killToken <- mkConnection_Receive("mem_kill");

  Connection_Receive#(Tuple2#(Addr, Inst))  magic_imem_write <- mkConnection_Receive("magic_imem");
  Connection_Receive#(Tuple2#(Addr, Value)) magic_dmem_write <- mkConnection_Receive("magic_dmem_write");
  Connection_Server#(Addr, Value)           magic_dmem_read  <- mkConnection_Server("magic_dmem_read");
  
  //handleIMEM
  
  //Handles all IMem requests
  rule handleIMEM (True);
  
    Addr a <- link_imem.getReq();
    
    if (a > maxAddr)
      $display("WARNING [0]: Address 0x%h out of bounds. Increase software address length!", a);
    
    SoftAddr sa = truncate(a);
    link_imem.makeResp(imemory.sub(sa));
    
  endrule

  //handleDMEM
  
  //handles Dmem loads/stores but not commits/rollbacks
 
  rule handleDMEM (True);

    MemReq req <- link_dmem.getReq();
    
    case (req) matches
      tagged Ld .ld_info:
        begin
	  if (ld_info.addr > maxAddr)
            $display("WARNING [1]: Address 0x%h out of bounds. Increase software address length!", ld_info.addr);

	  SoftAddr sa = truncate(ld_info.addr);
	  
	  if (st_buffer.mayHaveAddress(ld_info.addr))
	  begin  
	    //Store buffer may have addr. Take the slow path
	    waitingQ.enq(tuple2(ld_info.token, dmemory.sub(sa)));
	    st_buffer.retrieve(ld_info.token, ld_info.addr);
	  end
	  else
	      //Store buffer does not have addr, return.
	      link_dmem.makeResp(LdResp dmemory.sub(sa));  
        end
      tagged St .st_info:
        begin
		  
	  //place value in store buffer	  	  
	  st_buffer.insert(st_info.token, st_info.addr, st_info.val);
	    
          link_dmem.makeResp(StResp);
	  
        end
    endcase
  
  endrule
 
  //handleCommit
  
  //Actually commits stores
 
  rule handleCommit (True);
  
    Token tok <- link_commit.receive();
    
    match {.a, .v} <- st_buffer.commit(tok);
    
    SoftAddr sa = truncate(a);

    if (a > maxAddr)
        $display("WARNING [2]: Address 0x%h out of bounds. Increase software address length!", a);
    
    dmemory.upd(sa, v);

  endrule
  
  //handleKillRange
  
  //Rolls back killed tokens
  
  rule handleKill (True);
  
    Token tok <- link_killToken.receive();
    
    st_buffer.kill(tok);

  endrule

  //finishSlowPath
  
  rule finishSlowPath (True);
  
    match {.tok, .cur_val} = waitingQ.first();
    waitingQ.deq();
    
    match {.t2, .mnew_val} <- st_buffer.result();
    
    if (tok != t2)
      $display("ERROR, unexpected response from Store Buffer!");
    
    case (mnew_val) matches
      tagged Invalid:  //All that work for nothing.
        link_dmem.makeResp(LdResp cur_val);
      tagged Valid .new_val:
	link_dmem.makeResp(LdResp new_val);
    endcase
        
  endrule

  //Magic interface for testharness

  rule magic_imem (True);
  
    match {.addr, .inst} <- magic_imem_write.receive();
    imemory.upd(truncate(addr), inst);
    
  endrule
  
  rule magic_dmem_w (True);
  
    match {.addr, .val} <- magic_dmem_write.receive();
    dmemory.upd(truncate(addr), val);
    
  endrule
  
  rule magic_dmem_r (True);
  
    let addr <- magic_dmem_read.getReq();
    magic_dmem_read.makeResp(dmemory.sub(truncate(addr)));
    
  endrule
  
endmodule
