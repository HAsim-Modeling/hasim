//Memory system with connections

import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import fpga_components::*;
import hasim_common::*;
import soft_connections::*;
import funcp_memory::*;

import hasim_isa::*;

import hasim_funcp_storebuffer::*;
import hasim_funcp_cache::*;

import hasim_funcp_memstate_ifc::*;

/************* Simple Memory System Implementation *************/

// This is intended for software simulation. An FPGA version would
// be a memory controller.

typedef enum
{
  IMem, DMem
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

  //State elements
  FIFO#(PathSpeed)             pathQ <- mkFIFO();

  let st_buffer <- mkFUNCP_StoreBuffer();
  let cache     <- mkFUNCP_Cache();

  //Connections

  Connection_Server#(Addr, PackedInst) link_imem      <- mkConnection_Server("mem_imem");
  Connection_Server#(MemReq, MemResp)  link_dmem      <- mkConnection_Server("mem_dmem");
  Connection_Client#(SB_Command, SB_Response) link_stbuffer <- mkConnection_Client("mem_storebuf");
  Connection_Client#(MEM_REQUEST, MEM_VALUE)  link_cache     <- mkConnection_Client("mem_cache");
  Connection_Receive#(Token)           link_commit    <- mkConnection_Receive("mem_commit");
  Connection_Receive#(Tuple2#(TokIndex, TokIndex))           link_rewindToToken <- mkConnection_Receive("mem_rewind");

  //handleIMEM
   
  //Handles all IMem requests

  rule handleIMEM (True);
  
    Addr a = link_imem.getReq();
    link_imem.deq();
    
    //We assume no self-modifying code, otherwise we would go to the store buffer here.
    link_cache.makeReq(tagged MEM_LOAD a);
    pathQ.enq(IMem);
    
  endrule
  
  rule handleIMEM_resp (pathQ.first() == IMem);
  
    MEM_VALUE v = link_cache.getResp();
    link_cache.deq();
    
    link_imem.makeResp(v);
    
    pathQ.deq();
    
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
      
          //place value in store buffer, but don't actually change memory
          //$display("Store at %0d", $time);        
          link_stbuffer.makeReq(tagged SB_Insert {v: st_info.val, a: st_info.addr, t: st_info.token});
          
          //Fast-forward the response
          link_dmem.makeResp(tagged StResp);
        end
    endcase
  
  endrule

  //handleCommit
  
  //Actually commits stores to memory
 
  rule handleCommit (True);
  
    Token tok = link_commit.receive();
    link_commit.deq();
    
    link_stbuffer.makeReq(tagged SB_Commit tok);
    
  endrule
  
  rule handleCommit_2 (link_stbuffer.getResp() matches tagged SBR_Commit {a: .a, unused: .*, v: .v, t: .t});
    
    link_stbuffer.deq();
    
    link_cache.makeReq(tagged MEM_STORE {addr:a, val: v});
    
  endrule
  
  //handleKillRange
  
  //Rolls back killed tokens
  
  rule handleRewind (True);
  
    match {.rewind_tok, .youngest} = link_rewindToToken.receive();
    link_rewindToToken.deq();
    
    link_stbuffer.makeReq(tagged SB_Rewind {rewind: rewind_tok, youngest: youngest});

  endrule

  //Finish DMem requests

  rule sbHit (link_stbuffer.getResp() matches tagged SBR_Lookup {a: .addr, mv: .mnew_val, t: .tok}
                   &&& mnew_val matches tagged Valid .new_val);
  
    //The Store Buffer had it. No need to go to cache.
  
    link_stbuffer.deq();
    link_dmem.makeResp(tagged LdResp new_val);
        
  endrule
  
  //The Store Buffer doesn't have it, so we use the result from the dmem
  
  rule sbMiss (link_stbuffer.getResp() matches tagged SBR_Lookup {a: .addr, mv: .mnew_val, t:.tok}
                   &&& mnew_val matches tagged Invalid);
    
    link_stbuffer.deq();
    
    link_cache.makeReq(tagged MEM_LOAD addr);
    pathQ.enq(DMem);
  
  endrule
  
  rule sbMissFinish (pathQ.first() == DMem);
  
    let v = link_cache.getResp();
    link_cache.deq();
    
    link_dmem.makeResp(tagged LdResp v);
    
    pathQ.deq();
        
  endrule
  
endmodule
