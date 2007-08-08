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

  FIFO#(PathSpeed)             waitingQ <- mkFIFO();

  let st_buffer <- mkFUNCP_StoreBuffer();

  //Connections

  Connection_Server#(Addr, PackedInst) link_imem      <- mkConnection_Server("mem_imem");
  Connection_Server#(MemReq, MemResp)  link_dmem      <- mkConnection_Server("mem_dmem");
  Connection_Client#(SB_Command, SB_Response) link_stbuffer <- mkConnection_Client("mem_storebuf");
  Connection_Receive#(Token)           link_commit    <- mkConnection_Receive("mem_commit");
  Connection_Receive#(Tuple2#(TokIndex, TokIndex))           link_rewindToToken <- mkConnection_Receive("mem_rewind");

  //handleIMEM
   
  //Handles all IMem requests

  rule handleIMEM (True);
  
    Addr a = link_imem.getReq();
    link_imem.deq();
    
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

    MemReq req = link_dmem.getReq();
    link_dmem.deq();
    
    //$display("Request at %0d", $time);
    case (req) matches
      tagged Ld .ld_info:
        begin
  
          link_stbuffer.makeReq(tagged SB_Lookup {a: ld_info.addr, t: ld_info.token});
      
        end
      tagged St .st_info:
        begin
      
          //place value in store buffer
          //$display("Store at %0d", $time);        
          link_stbuffer.makeReq(tagged SB_Insert {v: st_info.val, a: st_info.addr, t: st_info.token});
      
          link_dmem.makeResp(tagged StResp);
    
        end
    endcase
  
  endrule

  //handleCommit
  
  //Actually commits stores
 
  rule handleCommit (True);
  
    Token tok = link_commit.receive();
    link_commit.deq();
    
    link_stbuffer.makeReq(tagged SB_Commit tok);
    
  endrule
  
  rule handleCommit_2 (link_stbuffer.getResp() matches tagged SBR_Commit {a: .a, unused: .*, v: .v, t: .t});
    
    link_stbuffer.deq();
    
    Addr sa = a>>2;

    if (sa > maxAddr)
        $display("WARNING [2]: Address 0x%h out of bounds. Increase software address length!", a);
    
    memory.write(truncate(sa), v);

  endrule
  
  //handleKillRange
  
  //Rolls back killed tokens
  
  rule handleRewind (True);
  
    match {.rewind_tok, .youngest} = link_rewindToToken.receive();
    link_rewindToToken.deq();
    
    link_stbuffer.makeReq(tagged SB_Rewind {rewind: rewind_tok, youngest: youngest});

  endrule

  rule sbHit (link_stbuffer.getResp() matches tagged SBR_Lookup {a: .addr, mv: .mnew_val, t: .tok}
                   &&& mnew_val matches tagged Valid .new_val);
  
    //The Store Buffer had it
  
    link_stbuffer.deq();
    link_dmem.makeResp(tagged LdResp new_val);
        
  endrule
  
  //The Store Buffer doesn't have it, so we use the result from the dmem
  
  rule sbMiss (link_stbuffer.getResp() matches tagged SBR_Lookup {a: .addr, mv: .mnew_val, t:.tok}
                   &&& mnew_val matches tagged Invalid);
    
    link_stbuffer.deq();
    
  
     Addr sa = addr>>2;

     if (sa > maxAddr)
       $display("WARNING [1]: Address 0x%h out of bounds. Increase software address length!", addr);

     //$display("Load at %0d", $time);
     memory.read_req2(truncate(sa));

  endrule
  
  rule sbMissFinish (True);
    let v <- memory.read_resp2();
    link_dmem.makeResp(tagged LdResp v);
  endrule
  
endmodule
