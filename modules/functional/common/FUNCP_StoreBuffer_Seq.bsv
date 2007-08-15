
import fpga_components::*;
import hasim_common::*;

import hasim_isa::*;

//BSV Library imports
import Vector::*;
import FIFO::*;
import RegFile::*;
 

typedef union tagged
{
  struct {Value v; Addr a; Token t;}     SB_Insert;
  struct {Addr a; Token t;}              SB_Lookup;
  Token                                  SB_Commit;
  struct {TokIndex rewind; TokIndex youngest;} SB_Rewind;
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

typedef enum
{
  SB_Initializing,
  SB_Ready,
  SB_Inserting,
  SB_Finishing_Insert,
  SB_Searching,
  SB_Committing
}
  SB_State
    deriving (Eq, Bits);


typedef Bit#(`FUNCP_STOREB_HASH_BITS) AddrHash;

module [HASim_Module] mkFUNCP_StoreBuffer ()
  provisos
          (Bits#(TokIndex, idx_SZ));

  //******* State Elements

  Reg#(Vector#(TExp#(idx_SZ), Bool))      tvalids     <- mkReg(Vector::replicate(False));
  BRAM#(TokIndex, Maybe#(Token))          listnodes   <- mkBRAM_Full();
  BRAM#(TokIndex, Tuple2#(Addr, Value))   listvalues  <- mkBRAM_Full();
  RegFile#(AddrHash, Maybe#(Token))       heads       <- mkRegFileFull();

  Connection_Server#(SB_Command, SB_Response)  link_memstate <- mkConnection_Server("mem_storebuf");
  
  Reg#(SB_State) state <- mkReg(SB_Initializing);
  
  //Intermediate state for lookups
  
  Reg#(Token) candidate <- mkRegU();
  Reg#(Token) last_valid_tok <- mkRegU();
  Reg#(Maybe#(Tuple2#(Token, Value))) best_so_far <- mkReg(Invalid);
  
  Reg#(Bool) initializing <- mkReg(True);
  Reg#(TokIndex) cur <- mkReg(0);

  //Debug
  let debug_log <- mkReg(InvalidFile);
  Reg#(Bit#(32)) cc <- mkReg(0);

  //Change hash here
  function AddrHash hash(Addr a) = truncate(a>>2);
  
  function Bool isBetter(Token t, Maybe#(Tuple2#(Token, Value)) mt);
  
    case (mt) matches
      tagged Invalid:
        return True;
      tagged Valid {.old_t, .v}:
        return isOlder(old_t.index, t.index) && (t.timep_info.epoch == old_t.timep_info.epoch);
    endcase
  
  endfunction
  
  rule currentCC (True);
  
    cc <= cc + 1;
  
  endrule
 
  rule initialize (state == SB_Initializing);
  
    if (debug_log == InvalidFile)
    begin
      let fd <- $fopen("hasim_funcp_storebuff.out", "w");

      if (fd == InvalidFile)
      begin
        $display("Error opening FUNCP logfile hasim_funcp_storebuff.out");
        $finish(1);
      end

      debug_log <= fd;
      
    end

  
    listnodes.write(cur, tagged Invalid);
    heads.upd(truncate(cur), tagged Invalid);
    
    let newcur = cur + 1;
    
    cur <= newcur;
    
    if (newcur == 0)
      state <= SB_Ready;
  
  endrule
  
  //Insert a new value sorted by token age
  rule begin_insert (state == SB_Ready &&& link_memstate.getReq() matches tagged SB_Insert {v:.v, a: .a, t: .tok});

    let h = hash(a);
    let old_head = heads.sub(h);
    let insert_at_head = case (old_head) matches
        tagged Invalid: True;
        tagged Valid .t2: return isOlder(t2.index, tok.index);
      endcase;
    
    listvalues.write(tok.index, tuple2(a, v));
    
    if (insert_at_head)
      begin
        $fdisplay(debug_log, "[%d]: Inserting Token %0d at head of list #%0d", cc, tok.index, h);
        listnodes.write(tok.index, old_head);
        tvalids <= update(tvalids, tok.index, True);
        heads.upd(h, tagged Valid tok);
        link_memstate.deq();
      end
    else
      begin
        $fdisplay(debug_log, "[%d]: Token %0d cannot go at head of list #%0d", cc, tok.index, h);
        state <= SB_Inserting;
        listnodes.read_req(validValue(old_head).index);
        candidate <= validValue(old_head);
      end

  endrule
  
  //Insert a new value sorted by token age
  rule insert (state == SB_Inserting &&& link_memstate.getReq() matches tagged SB_Insert {v:.v, a: .a, t: .tok});

   let mnext <- listnodes.read_resp();

   let is_younger_than_next = case (mnext) matches 
                                tagged Invalid: True;
                                tagged Valid .t2: isOlder(tok.index, t2.index);
                              endcase;
   
   if (is_younger_than_next)
      begin
        $fdisplay(debug_log, "[%d]: Inserting Token %0d after Token %0d", cc, tok.index, candidate.index);
        listnodes.write(tok.index, mnext);
        listnodes.read_req(candidate.index);
        tvalids <= update(tvalids, tok.index, True);
        state <= SB_Finishing_Insert;
      end
   else
      begin
        $fdisplay(debug_log, "[%d]: Could not insert Token %0d after Token %0d", cc, tok.index, candidate.index);
        listnodes.read_req(validValue(mnext).index);
        candidate <= validValue(mnext);
      end
   
  endrule
  
  //Update the prev to point to newly inserted node
  rule finish_insert (state == SB_Finishing_Insert &&& link_memstate.getReq() matches tagged SB_Insert {v:.v, a: .a, t: .tok});

   let mnext <- listnodes.read_resp();

   $fdisplay(debug_log, "[%d]: Finishing Insert. Token %0d now point at Token %0d", cc, candidate.index, tok.index);

   listnodes.write(candidate.index, tagged Valid tok);
   state <= SB_Ready;
   link_memstate.deq();

  endrule
  
  //Invalidate, and return value for writeback
  
  rule commit_req (state == SB_Ready &&& link_memstate.getReq() matches tagged SB_Commit .tok);
  
    listvalues.read_req(tok.index);
    listnodes.read_req(tok.index);
    state <= SB_Committing;
  
  endrule
  
  rule commit_resp (state == SB_Committing &&& link_memstate.getReq() matches tagged SB_Commit .tok);

    //We invalidate the node's data, but 
    //do not remove the node from the list unless it's the head
    //which is correct, but may slow down lookups

    $fdisplay(debug_log, "[%d]: Committing Token %0d", cc, tok.index);

    match {.a, .v} <- listvalues.read_resp();
    let next_node <- listnodes.read_resp();
    let h = hash(a);
    
    case (heads.sub(h)) matches
      tagged Invalid:
      begin
        $display("[%d]: ERROR: FUNCP: Store Buffer: Committing Token %0d which was not in a list.", cc, tok.index);
        $finish(1);
      end
      tagged Valid .t2:
      begin
        if (tok.index == t2.index) //The dead node was the head of the list
        begin
          heads.upd(h, next_node);
          $fdisplay(debug_log, "[%d]: Resetting head of list!", cc);
        end
      end
    endcase
    
    tvalids <= update(tvalids, tok.index, False);
    
    link_memstate.makeResp(tagged SBR_Commit {a:a, unused: 0, v:v, t:tok});
    state <= SB_Ready;
    link_memstate.deq();

  endrule

  //Invalidate, but no need to send the response
  rule rewind (state == SB_Ready &&& link_memstate.getReq() matches tagged SB_Rewind .rinfo);
     
    //We invalidate the nodes' data, but
    //do not remove them from the list
    //which is correct, but may slow down lookups
    $fdisplay(debug_log, "[%d]: Rewinding to %0d (Youngest is %0d)", cc, rinfo.rewind, rinfo.youngest);
    
    Vector#(TExp#(idx_SZ), Bool) as = newVector();
    
    for (Integer x = 0; x < valueof(TExp#(idx_SZ)); x = x + 1)
    begin
      TokIndex cur = fromInteger(x);
      let newval = (rinfo.youngest > rinfo.rewind) ? 
                 //No overflow
                 ((cur > rinfo.rewind) && (cur <= rinfo.youngest) ? False : tvalids[x]) :
                 //Overflow
                 ((cur > rinfo.rewind) || (cur <= rinfo.youngest) ? False : tvalids[x]);

      if (newval != tvalids[x])
        $fdisplay(debug_log, "[%d]: Changing Token %0d from %0d to %0d", cc, cur, tvalids[x], newval);
      
      as[x] = newval;
    end

    tvalids <= as;
    link_memstate.deq(); 

  endrule

  //Check for the oldest store younger than tok
  
  
  rule begin_lookup (state == SB_Ready &&& link_memstate.getReq() matches tagged SB_Lookup {a:.addr, t:.tok});
      
    case (heads.sub(hash(addr))) matches
      tagged Invalid: //Store Buffer doesn't have it. Short circuit the response
      begin
        $fdisplay(debug_log, "[%d]: Looking up %0d: Hash says we don't have it!", cc, tok.index);
        link_memstate.makeResp(tagged SBR_Lookup {a: addr, mv: tagged Invalid, t:tok});
        link_memstate.deq();
      end
      tagged Valid .c:
      begin
        $fdisplay(debug_log, "[%d]: Begining lookup of Token %0d in list #%0d", cc, tok.index, hash(addr)); 
        listvalues.read_req(c.index);
        listnodes.read_req(c.index);
        candidate <= c;
        state <= SB_Searching;
        best_so_far <= tagged Invalid;
        last_valid_tok <= c;
      end
    endcase
  
  endrule
  
  rule lookup (state == SB_Searching &&& link_memstate.getReq() matches tagged SB_Lookup {a:.addr, t:.tok});
            
    match {.a, .v} <- listvalues.read_resp();
    let mnext_tok <- listnodes.read_resp();

    let newbest = (tvalids[candidate.index] 
                   && (a == addr) 
                   && isOlder(candidate.index, tok.index) 
                   && isBetter(candidate, best_so_far)) 
                        ? tagged Valid tuple2(candidate, v) 
                        : best_so_far;

    if (newbest != best_so_far)
       $fdisplay(debug_log, "[%d]: Token %0d is the new best match.", cc);

    case (mnext_tok) matches
      tagged Invalid: //Wether or not we've found it, we're done
      begin
        let finalres  = case (newbest) matches
                          tagged Invalid: tagged Invalid;
                          tagged Valid {.tk, .v}: tagged Valid v;
                        endcase;

        $fwrite(debug_log, "[%d]: Hit the end of list.", cc); 
        
        if (!tvalids[candidate.index])
        begin
          listnodes.write(last_valid_tok.index, tagged Invalid);
          $fdisplay(debug_log, "[%d]: Setting %0d to be the tail of the list", cc, last_valid_tok.index); 
        end

        if (isValid(finalres))
          $fwrite(debug_log, "A success!\n");
        else
          $fwrite(debug_log, "A false positive...\n");
        
        link_memstate.makeResp(tagged SBR_Lookup {mv:finalres, a: addr, t:tok});

        state <= SB_Ready;
        link_memstate.deq();

      end
    tagged Valid .next_tok: //Keep looking for a better match
      begin
        if (next_tok.index == candidate.index)
        begin
          $display("ERROR: FUNCP: Store Buffer: Infinite loop in list on Token %0d (Addr: 0x%h, Value: 0x%h)", candidate.index, a, v);
          $finish(1);
        end
          
        $fdisplay(debug_log, "[%d]: Still more list to go.", cc); 

        listnodes.write(last_valid_tok.index, tagged Valid candidate);
        $fdisplay(debug_log, "[%d]: Pointing Token %0d at Token %0d", cc, last_valid_tok.index, candidate.index); 

        if (tvalids[candidate.index])
        begin
          $fdisplay(debug_log, "[%d]: Token %0d is last-known good node.", cc, candidate.index); 
          last_valid_tok <= candidate;
        end
        else
        begin
          $fdisplay(debug_log, "[%d]: Removing invalid Token %0d from list.", cc, candidate.index); 
        end
   
        listvalues.read_req(next_tok.index);
        listnodes.read_req(next_tok.index);
        candidate <= next_tok;
        best_so_far <= newbest;
      end
    endcase
  
  endrule
   
endmodule
