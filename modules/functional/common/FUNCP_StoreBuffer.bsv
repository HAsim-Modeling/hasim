
import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_isa::*;

//BSV Library imports
import Vector::*;
import FIFO::*;
import RegFile::*;


interface StoreBuffer;

  method Bool mayHaveAddress(Addr a);
  method Action retrieve(Token t, Addr a);
  method ActionValue#(Tuple2#(Token, Maybe#(Value))) result();
  method Action insert(Token t, Addr a, Value v);
  method ActionValue#(Tuple2#(Addr, Value)) commit(Token t);
  method Action kill(Token t);

endinterface

typedef Bit#(3) AddrHash;

module [HASim_Module] mkFUNCP_StoreBuffer (StoreBuffer)
  provisos
          (Bits#(Token, token_SZ));

  //******* State Elements

  Reg#(Vector#(TExp#(token_SZ), Bool))              tvalids   <- mkReg(Vector::replicate(False));
  RegFile#(TokIndex, Tuple3#(Addr, Value, Maybe#(Token))) tokens <- mkRegFileFull();
  RegFile#(AddrHash, Maybe#(Token))                 hashes    <- mkRegFileFull();

  FIFO#(Tuple4#(Token, Addr, Token, Maybe#(Tuple2#(Token, Value))))  workingQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Maybe#(Tuple2#(Token, Value))))  resultQ <- mkFIFO();
  
  //Change hash here
  function AddrHash hash(Addr a) = truncate(a);
  
  function Bool isBetter(Token t, Maybe#(Tuple2#(Token, Value)) mt);
  
    case (mt) matches
      tagged Invalid:
        return True;
      tagged Valid {.t2, .v}:
        return (t2.index > t.index) && (t.info.epoch == t2.info.epoch);
    endcase
  
  endfunction
  
  //Looks up the most recent store to an addr
   
  rule doLookup (True);
  
    match {.tok, .addr, .cur_tok, .best} = workingQ.first();
    workingQ.deq();
    
    match {.a, .v, .next_tok} = tokens.sub(cur_tok.index);
    
    let done = !tvalids[cur_tok.index] || !isValid(next_tok);
    let newbest = (tvalids[cur_tok.index] && (a == addr) && (cur_tok.index < tok.index) && isBetter(cur_tok, best)) ? Valid tuple2(cur_tok, v) : best;

    if (done)
      resultQ.enq(tuple2(tok, newbest));
    else
      workingQ.enq(tuple4(tok, addr, validValue(next_tok), newbest));
  
  endrule
  
  //mayHaveAddress
  
  method Bool mayHaveAddress(Addr a);
    AddrHash h = hash(a);
    return isJust(hashes.sub(h));
  endmethod
  
  //retrieve
  
  method Action retrieve(Token t, Addr a);
    AddrHash h = hash(a);
    workingQ.enq(tuple4(t, a, unJust(hashes.sub(h)), Invalid));
  endmethod
  
  //result
  
  method ActionValue#(Tuple2#(Token, Maybe#(Value))) result();
  
    match {.tok, .best} = resultQ.first();
    resultQ.deq();
    
    case (best) matches
      tagged Valid {.t, .v}:
        return tuple2(tok, Valid v);
      tagged Invalid:
        return tuple2(tok, Invalid);
    endcase
    
  endmethod
  
  //insert
  
  method Action insert(Token t, Addr a, Value v);
  
  
    AddrHash h = hash(a);
    let ntok = hashes.sub(h);
  
    tokens.upd(t.index, tuple3(a, v, ntok));
    hashes.upd(h, Valid t);
    tvalids <= update(tvalids, t.index, True);
    
  endmethod
  
  method ActionValue#(Tuple2#(Addr, Value)) commit(Token t);
  
    tvalids <= update(tvalids, t.index, False);
    match {.a, .v, .ntok} = tokens.sub(t.index);
    //XXX We should really update the hash here.
 
    return tuple2(a, v);
    
  endmethod
  
  method Action kill(Token t);
  
    tvalids <= update(tvalids, t.index, False);
  
  endmethod
  
endmodule
