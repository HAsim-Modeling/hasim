
import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_isa::*;

//BSV Library imports
import Vector::*;
import FIFO::*;
import RegFile::*;
 

interface StoreBuffer;

  method Action checkAddress(Addr a);
  method ActionValue#(Bool) mayHaveAddress();
  
  method Action retrieve(Token t, Addr a);
  method ActionValue#(Tuple2#(Token, Maybe#(Value))) result();
  
  method Action insert(Token t, Addr a, Value v);
  
  method Action commit_req(Token t);
  method ActionValue#(Tuple2#(Addr, Value)) commit_resp();

  method Action kill(Token t);

endinterface

typedef Bit#(3) AddrHash;

module [HASim_Module] mkFUNCP_StoreBuffer (StoreBuffer)
  provisos
          (Bits#(TokIndex, idx_SZ));

  //******* State Elements

  Reg#(Vector#(TExp#(idx_SZ), Bool))                      tvalids  <- mkReg(Vector::replicate(False));
  BRAM_2#(TokIndex, Tuple3#(Addr, Value, Maybe#(Token)))  tokens   <- mkBRAM_2_Full();
  BRAM_2#(AddrHash, Maybe#(Token))                          hashes <- mkBRAM_2_Full();

  FIFO#(Tuple4#(Token, Addr, Token, Maybe#(Tuple2#(Token, Value))))  workingQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Addr))                                       retrieveQ <- mkFIFO();
  FIFO#(Tuple4#(Token, Addr, Value, AddrHash))                        insertQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Maybe#(Tuple2#(Token, Value))))                resultQ <- mkFIFO();
  
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
    
    match {.a, .v, .next_tok} <- tokens.read_resp1();
    
    let done = !tvalids[cur_tok.index] || !isValid(next_tok);
    let newbest = (tvalids[cur_tok.index] && (a == addr) && (cur_tok.index < tok.index) && isBetter(cur_tok, best)) ? tagged Valid tuple2(cur_tok, v) : best;

    if (done)
      resultQ.enq(tuple2(tok, newbest));
    else
    begin
      tokens.read_req1(validValue(next_tok).index);
      workingQ.enq(tuple4(tok, addr, validValue(next_tok), newbest));
    end
  
  endrule
  
  rule insert_2 (True);
  
    match {.t, .a, .v, .h} = insertQ.first();
    insertQ.deq();
    
    let ntok <- hashes.read_resp1();
  
    tokens.write(t.index, tuple3(a, v, ntok));
    hashes.write(h, tagged Valid t);
    tvalids <= update(tvalids, t.index, True);
    
  endrule

  rule retrieve_2 (True);
    
    match {.t, .a} = retrieveQ.first();
    retrieveQ.deq();
   
    let res <- hashes.read_resp1();
  
    let cur = unJust(res); //Must be valid
    
    tokens.read_req1(cur.index);
    workingQ.enq(tuple4(t, a, cur, Invalid));
  
  endrule
  
  //mayHaveAddress
  
  method Action checkAddress(Addr a);
  
    AddrHash h = hash(a);
    hashes.read_req2(h);
    
  endmethod
  
  method ActionValue#(Bool) mayHaveAddress();
  
    let res <- hashes.read_resp2();
    return isJust(res);
  
  endmethod
  
  //retrieve
  
  method Action retrieve(Token t, Addr a);
  
    AddrHash h = hash(a);
    hashes.read_req1(h);
    retrieveQ.enq(tuple2(t, a));
    
  endmethod
  
  //result
  
  method ActionValue#(Tuple2#(Token, Maybe#(Value))) result();
  
    match {.tok, .best} = resultQ.first();
    resultQ.deq();
    
    case (best) matches
      tagged Valid {.t, .v}:
        return tuple2(tok, tagged Valid v);
      tagged Invalid:
        return tuple2(tok, tagged Invalid);
    endcase
    
  endmethod
  
  //insert
  
  method Action insert(Token t, Addr a, Value v);
  
  
    AddrHash h = hash(a);
    hashes.read_req1(h);    
    insertQ.enq(tuple4(t, a, v, h));
 
  endmethod
  
  
  method Action commit_req(Token t);
  
    //XXX We should really update the hash here.
    tvalids <= update(tvalids, t.index, False);
    tokens.read_req2(t.index);
    
  endmethod
  
  method ActionValue#(Tuple2#(Addr, Value)) commit_resp();
  
    match {.a, .v, .ntok} <- tokens.read_resp2();
    return tuple2(a, v);
    
  endmethod
  
  method Action kill(Token t);
  
    tvalids <= update(tvalids, t.index, False);
  
  endmethod
  
endmodule
