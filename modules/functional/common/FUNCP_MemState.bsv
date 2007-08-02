//Memory system with connections

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import fpga_components::*;
import hasim_common::*;

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

typedef Bit#(`FUNCP_MEM_ADDR_BITS) SimAddr;

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
      
  SimAddr maxSimAddr = maxBound();
  Addr maxAddr = zeroExtend(maxSimAddr);

  //State elements

  BRAM_2#(SimAddr, Bit#(32))  memory <- mkBRAM_2_Full_Load("program.vmh");

  FIFO#(Tuple2#(Token, Addr))     loadQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Value))  st_bufQ <- mkFIFO();
  FIFO#(PathSpeed)             waitingQ <- mkFIFO();

  StoreBuffer st_buffer <- mkFUNCP_StoreBuffer();

  //Connections

  Connection_Server#(Addr, PackedInst) link_imem      <- mkConnection_Server("mem_imem");
  Connection_Server#(MemReq, MemResp)  link_dmem      <- mkConnection_Server("mem_dmem");
  Connection_Receive#(Token)           link_commit    <- mkConnection_Receive("mem_commit");
  Connection_Receive#(Token)           link_killToken <- mkConnection_Receive("fp_memstate_kill");

  //handleIMEM
  
  //Handles all IMem requests

  rule handleIMEM (True);
  
    Addr a <- link_imem.getReq();
    
    Addr sa = a>>2;
    
    if (sa > maxAddr)
      $display("WARNING [0]: Address 0x%h out of bounds. Increase software address length!", a);
    
    memory.read_req1(truncate(sa));
    
  endrule
  
  rule handleIMEM_resp (True);
  
    Bit#(32) i <- memory.read_resp1();
    link_imem.makeResp(i);
    
  endrule

  //handleDMEM
  
  //handles Dmem loads/stores but not commits/rollbacks
 
  rule handleDMEM (True);

    MemReq req <- link_dmem.getReq();
    
    //$display("Request at %0d", $time);
    case (req) matches
      tagged Ld .ld_info:
        begin
  
          Addr sa = ld_info.addr>>2;

          if (sa > maxAddr)
            $display("WARNING [1]: Address 0x%h out of bounds. Increase software address length!", ld_info.addr);

          //$display("Load at %0d", $time);
          memory.read_req2(truncate(sa));
          st_buffer.checkAddress(ld_info.addr);
          loadQ.enq(tuple2(ld_info.token, ld_info.addr));
      
        end
      tagged St .st_info:
        begin
      
          //place value in store buffer
          //$display("Store at %0d", $time);        
          st_buffer.insert(st_info.token, st_info.addr, st_info.val);
      
          link_dmem.makeResp(tagged StResp);
    
        end
    endcase
  
  endrule

  rule handleStBufferCheck (True);
    
    match {.tok, .addr} = loadQ.first();
    loadQ.deq();

    let slow <- st_buffer.mayHaveAddress();
    let v <- memory.read_resp2();
    
    if (slow)
    begin  
      //Store buffer may have addr. Take the slow path
      st_bufQ.enq(tuple2(tok, v));
      st_buffer.retrieve(tok, addr);
    end
    else
    begin
      //Fast path response
      link_dmem.makeResp(tagged LdResp v); 
    end
      
  endrule
 
  //handleCommit
  
  //Actually commits stores
 
  rule handleCommit (True);
  
    Token tok <- link_commit.receive();
    st_buffer.commit_req(tok);
    
  endrule
  
  rule handleCommit_2 (True);
  
    match {.a, .v} <- st_buffer.commit_resp();
    
    Addr sa = a>>2;

    if (sa > maxAddr)
        $display("WARNING [2]: Address 0x%h out of bounds. Increase software address length!", a);
    
    memory.write(truncate(sa), v);

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
    st_bufQ.deq();
    
    match {.t2, .mnew_val} <- st_buffer.result();
    
    if (tok != t2)
      $display("ERROR, unexpected response from Store Buffer!");
    
    case (mnew_val) matches
      tagged Invalid:  //All that work for nothing.
        link_dmem.makeResp(tagged LdResp cur_val);
      tagged Valid .new_val:
        link_dmem.makeResp(tagged LdResp new_val);
    endcase
        
  endrule
  
endmodule
