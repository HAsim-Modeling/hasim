//Memory system with connections

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_funcp_base::*;
import hasim_isa::*;

import hasim_funcp_storebuffer::*;

import hasim_funcp_memstate_ifc::*;

/************* Simple Memory System Implementation *************/

// This is intended for software simulation. An FPGA version would
// be a memory controller.

typedef union tagged
{
  void Fast;
  Token Slow;
}
  PathSpeed deriving (Eq, Bits);

module [HASim_Module] mkFUNCP_Memstate ()
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

  BRAM#(SoftAddr, Inst)  imemory <- mkBRAM_Full();
  BRAM#(SoftAddr, Value) dmemory <- mkBRAM_Full();

  FIFO#(Tuple2#(Token, Value))  st_bufQ <- mkFIFO();
  FIFO#(PathSpeed)             waitingQ <- mkFIFO();

  StoreBuffer st_buffer <- mkFUNCP_StoreBuffer();

  //Connections

  Connection_Server#(Addr, Inst)      link_imem      <- mkConnection_Server("mem_imem");
  Connection_Server#(MemReq, MemResp) link_dmem      <- mkConnection_Server("mem_dmem");
  Connection_Receive#(Token)          link_commit    <- mkConnection_Receive("mem_commit");
  Connection_Receive#(Token)          link_killToken <- mkConnection_Receive("fp_memstate_kill");

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
    imemory.read_req(sa);
    
  endrule
  
  rule handleIMEM_resp (True);
  
    let i <- imemory.read_resp();
    link_imem.makeResp(i);
    
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
	  
	  dmemory.read_req(sa);

	  if (st_buffer.mayHaveAddress(ld_info.addr))
	  begin  
	    //Store buffer may have addr. Take the slow path
	    waitingQ.enq(Slow ld_info.token);
	    st_buffer.retrieve(ld_info.token, ld_info.addr);
	  end
	  else
	  begin
	    waitingQ.enq(Fast);
	  end
	    
        end
      tagged St .st_info:
        begin
		  
	  //place value in store buffer	  	  
	  st_buffer.insert(st_info.token, st_info.addr, st_info.val);
	    
          link_dmem.makeResp(StResp);
	  
        end
    endcase
  
  endrule
 
  rule handleDMEM_resp (True);
  
    let v <- dmemory.read_resp();

    case (waitingQ.first()) matches
      tagged Fast:
      begin
	link_dmem.makeResp(LdResp v);  
      end
      tagged Slow .tok:
      begin
        st_bufQ.enq(tuple2(tok, v));
      end
    endcase
    
    st_bufQ.deq();

  endrule
 
  //handleCommit
  
  //Actually commits stores
 
  rule handleCommit (True);
  
    Token tok <- link_commit.receive();
    
    match {.a, .v} <- st_buffer.commit(tok);
    
    SoftAddr sa = truncate(a);

    if (a > maxAddr)
        $display("WARNING [2]: Address 0x%h out of bounds. Increase software address length!", a);
    
    dmemory.write(sa, v);

  endrule
  
  //handleKillRange
  
  //Rolls back killed tokens
  
  rule handleKill (True);
  
    Token tok <- link_killToken.receive();
    
    st_buffer.kill(tok);

  endrule

  //finishSlowPath
  
  rule finishSlowPath (True);
  
    match {.tok, .cur_val} = st_bufQ.first();
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
    imemory.write(truncate(addr), inst);
    
  endrule
  
  rule magic_dmem_w (True);
  
    match {.addr, .val} <- magic_dmem_write.receive();
    dmemory.write(truncate(addr), val);
    
  endrule
  
  rule magic_dmem_r_req (True);
  
    let addr <- magic_dmem_read.getReq();
    dmemory.read_req(truncate(addr));
    
  endrule
 
  rule magic_dmem_r_resp (True);
  
    let v <- dmemory.read_resp();
    magic_dmem_read.makeResp(v);
    
  endrule
  
endmodule
