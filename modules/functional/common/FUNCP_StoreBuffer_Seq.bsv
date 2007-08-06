
import fpga_components::*;
import hasim_common::*;

import hasim_isa::*;

//BSV Library imports
import Vector::*;
import FIFO::*;
import RegFile::*;
 

typedef union tagged
{
  struct {Value v; Addr a; Token t;} SB_Insert;
  struct {Addr a; Token t;}          SB_Lookup;
  Token                              SB_Commit;
  Token                              SB_Kill;
}
  SB_Command 
    deriving (Eq, Bits);

typedef union tagged
{
  struct {Addr a; Maybe#(Value) mv; Token t;} SBR_Lookup;
  struct {Addr a; Bit#(1) unused; Value v; Token t;}  SBR_Commit;
}
  SB_Response
    deriving (Eq, Bits);


typedef Bit#(`FUNCP_STOREB_HASH_BITS) AddrHash;

module [HASim_Module] mkFUNCP_StoreBuffer ()
  provisos
          (Bits#(TokIndex, idx_SZ));

  //******* State Elements

  Reg#(Vector#(TExp#(idx_SZ), Bool))                      tvalids  <- mkReg(Vector::replicate(False));
  BRAM#(TokIndex, Tuple3#(Addr, Value, Maybe#(Token)))    listmem  <- mkBRAM_Full();
  RegFile#(AddrHash, Maybe#(Token))                        heads   <- mkRegFileFull();

  Connection_Server#(SB_Command, SB_Response)  link_memstate <- mkConnection_Server("mem_storebuf");
  
  //Intermediate state for lookups
  
  Reg#(Bool) searching <- mkReg(False);
  Reg#(Bool) committing <- mkReg(False);
  Reg#(Token) candidate <- mkRegU();
  Reg#(Maybe#(Tuple2#(Token, Value))) best_so_far <- mkReg(Invalid);
  
  Reg#(Bool) initializing <- mkReg(True);
  Reg#(TokIndex) cur <- mkReg(0);

  //Change hash here
  function AddrHash hash(Addr a) = truncate(a);
  
  function Bool isBetter(Token t, Maybe#(Tuple2#(Token, Value)) mt);
  
    case (mt) matches
      tagged Invalid:
        return True;
      tagged Valid {.old_t, .v}:
        return isOlder(old_t.index, t.index) && (t.timep_info.epoch == old_t.timep_info.epoch);
    endcase
  
  endfunction
  
  rule initialize (initializing);
  
    listmem.write(cur, tuple3(0, 0, tagged Invalid));
    heads.upd(truncate(cur), tagged Invalid);
    
    let newcur = cur + 1;
    
    cur <= newcur;
    
    if (newcur == 0)
      initializing <= False;
  
  endrule
  
  //Second pipeline stage: handle the most recent intermediate work
  
  //Insert a new value at the head of a list
  rule insert (!initializing &&& link_memstate.getReq() matches tagged SB_Insert {v:.v, a: .a, t: .tok});

    let h = hash(a);
    let old_head = heads.sub(h);
    listmem.write(tok.index, tuple3(a, v, old_head));
    tvalids <= update(tvalids, tok.index, True);
    heads.upd(h, tagged Valid tok);
    
    link_memstate.deq();

  endrule
  
  //Invalidate, and return value for writeback
  
  rule commit_req (!initializing &&& !committing &&& link_memstate.getReq() matches tagged SB_Commit .tok);
  
    listmem.read_req(tok.index);
    committing <= True;
  
  endrule
  
  rule commit_resp (!initializing &&& committing &&& link_memstate.getReq() matches tagged SB_Commit .tok);

    //We invalidate the node's data, but 
    //do not remove the node from the list
    //which is correct, but may slow down lookups

    tvalids <= update(tvalids, tok.index, False);
    match {.a, .v, .next_tok} <- listmem.read_resp();
    link_memstate.makeResp(tagged SBR_Commit {a:a, unused: 0, v:v, t:tok});
    committing <= False;
    link_memstate.deq();

  endrule

  //Invalidate, but no need to send the response
  rule kill (!initializing &&& link_memstate.getReq() matches tagged SB_Kill .tok);
        
    //We invalidate the node's data, but
    //do not remove the node from the list
    //which is correct, but may slow down lookups
    tvalids <= update(tvalids, tok.index, False); 
    link_memstate.deq(); 

  endrule

  //Check for the oldest store younger than tok
  
  
  rule begin_lookup (!initializing &&& !searching &&& link_memstate.getReq() matches tagged SB_Lookup {a:.addr, t:.tok});
      
    case (heads.sub(hash(addr))) matches
      tagged Invalid: //Store Buffer doesn't have it. Short circuit the response
      begin
        link_memstate.makeResp(tagged SBR_Lookup {a: addr, mv: tagged Invalid, t:tok});
        link_memstate.deq();
      end
      tagged Valid .c:
      begin
        listmem.read_req(c.index);
        candidate <= c;
        searching <= True;
        best_so_far <= tagged Invalid;
      end
    endcase
  
  endrule
  
  rule lookup (!initializing &&& searching &&& link_memstate.getReq() matches tagged SB_Lookup {a:.addr, t:.tok});
            
    match {.a, .v, .mnext_tok} <- listmem.read_resp();

    let newbest = (tvalids[candidate.index] 
                   && (a == addr) 
                   && isOlder(candidate.index, tok.index) 
                   && isBetter(candidate, best_so_far)) 
                        ? tagged Valid tuple2(candidate, v) 
                        : best_so_far;

    case (mnext_tok) matches
      tagged Invalid: //Wether or not we've found it, we're done
      begin
        let finalres  = case (newbest) matches
                          tagged Invalid: tagged Invalid;
                          tagged Valid {.tk, .v}: tagged Valid v;
                        endcase;

        link_memstate.makeResp(tagged SBR_Lookup {mv:finalres, a: a, t:tok});

        searching <= False;
        link_memstate.deq();

      end
    tagged Valid .next_tok: //Keep looking for a better match
      begin
        listmem.read_req(next_tok.index);
        candidate <= next_tok;
        best_so_far <= newbest;
      end
    endcase
  
  endrule
   
endmodule
