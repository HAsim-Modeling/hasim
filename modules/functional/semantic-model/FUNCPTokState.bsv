
import hasim_common::*;

import Vector::*;
import RegFile::*;

//A One-Hot encoding of Token State to get maximum throughput

typedef RegFile#(TokIndex, Bool) TokStateRF;

interface FUNCP_TokState;

  method Action deallocate(TokIndex t);
  method ActionValue#(TokIndex) allocate();
  
  method Action fet_start(TokIndex t);
  method Action fet_finish(TokIndex t);
  method Action dec_start(TokIndex t);
  method Action dec_finish(TokIndex t);
  method Action is_a_load(TokIndex t);
  method Action is_a_store(TokIndex t);
  method Action exe_start(TokIndex t);
  method Action exe_finish(TokIndex t);
  method Action mem_start(TokIndex t);
  method Action mem_finish(TokIndex t);
  method Action finalize(TokIndex t);
  
  method Action rewindTo(TokIndex t);
  
  method Bool isAllocated(TokIndex t);
  method Bool isReady(TokIndex t);
  method Bool isLoad(TokIndex t);
  method Bool isStore(TokIndex t);
  
  method TokIndex youngest();
  method TokIndex oldest();
  
endinterface


module mkFUNCP_TokState (FUNCP_TokState)
  provisos 
          (Bits#(TokIndex, idx_SZ));

  Reg#(Vector#(TExp#(idx_SZ), Bool)) alloc      <- mkReg(replicate(False));
  TokStateRF fetReq     <- mkRegFileFull();
  TokStateRF fetResp    <- mkRegFileFull();
  TokStateRF decReq     <- mkRegFileFull();
  TokStateRF decResp    <- mkRegFileFull();
  TokStateRF is_load    <- mkRegFileFull();
  TokStateRF is_store   <- mkRegFileFull();
  TokStateRF exeReq     <- mkRegFileFull();
  TokStateRF exeResp    <- mkRegFileFull();
  TokStateRF memReq     <- mkRegFileFull();
  TokStateRF memResp    <- mkRegFileFull();
  TokStateRF finalized  <- mkRegFileFull();

  Reg#(TokIndex) youngest_tok <- mkReg(0);
  Reg#(TokIndex) oldest_tok <- mkReg(0);

  function Bool isBusy(TokIndex t);
  
    let fet_busy = fetReq.sub(t) && !fetResp.sub(t);
    let dec_busy = decReq.sub(t) && !decResp.sub(t);
    let exe_busy = exeReq.sub(t) && !exeResp.sub(t);
    let mem_busy = memReq.sub(t) && !memResp.sub(t);
  
    return fet_busy || dec_busy || exe_busy || mem_busy;
  
  endfunction
  
  method Action deallocate(TokIndex t);
  
    if (!alloc[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Deallocating un-allocated Token %0d", t);
      $finish(1);
    end
    /*
    if (t != oldest_tok)
    begin
      $display("FUNCP: WARNING: TokState: Deallocating Tokens out of order (Given: %0d Oldest: %0d)", t, oldest_tok);
    end
    */
    oldest_tok <= oldest_tok + 1;
    alloc <=  update(alloc, t, False);
    
  endmethod
  
  method ActionValue#(TokIndex) allocate() if (!isBusy(youngest_tok)); //Stall until the youngest quiesces
  /*
    if (alloc.sub(t))
    begin
      $display("ERROR: Funcp Exception! TokState: Allocating already-allocated Token %0d.", t);
      $finish(1);
    end
    */
    let t = youngest_tok;
    
    alloc <= update(alloc, t, True);
    fetReq.upd(t, False);
    fetResp.upd(t, False);
    decReq.upd(t, False);
    decResp.upd(t, False);
    is_load.upd(t, False);
    is_store.upd(t, False);
    exeReq.upd(t, False);
    exeResp.upd(t, False);
    memReq.upd(t, False);
    memResp.upd(t, False);
    finalized.upd(t, False);
    
    youngest_tok <= youngest_tok + 1;
    
    return t;
    
  endmethod
  
  method Action fet_start(TokIndex t);
  /*
    if (!alloc[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Fetching un-allocated Token %0d", t);
      $finish(1);
    end
  */
    fetReq.upd(t, True);
    
  endmethod

  method Action fet_finish(TokIndex t);
  
    if (!fetReq.sub(t))
    begin
      $display("ERROR: Funcp Exception! TokState: Finishing Fetch without beginning on Token %0d", t);
      $finish(1);
    end
  
    fetResp.upd(t, True);
    
  endmethod

  method Action dec_start(TokIndex t);
  
    if (!fetResp.sub(t))
    begin
      $display("ERROR: Funcp Exception! TokState: Decoding without finishing Fetch on Token %0d", t);
      $finish(1);
    end
    
    decReq.upd(t, True);
    
  endmethod

  method Action dec_finish(TokIndex t);
  
    if (!decReq.sub(t))
    begin
      $display("ERROR: Funcp Exception! TokState: Finishing Decode without beginning on Token %0d", t);
      $finish(1);
    end
  
    decResp.upd(t, True);
    
  endmethod
  
  method Action is_a_load(TokIndex t);
    
    is_load.upd(t, True);
    
  endmethod
  
  method Action is_a_store(TokIndex t);
    
    is_store.upd(t, True);
    
  endmethod
  
  method Action exe_start(TokIndex t);
  
  
    if (!decResp.sub(t))
    begin
      $display("ERROR: Funcp Exception! TokState: Executing without finishing Decode on Token %0d", t);
      $finish(1);
    end
    
    exeReq.upd(t, True);
    
  endmethod

  method Action exe_finish(TokIndex t);
  
    if (!exeReq.sub(t))
    begin
      $display("ERROR: Funcp Exception! TokState: Finishing Execute without beginning on Token %0d", t);
      $finish(1);
    end
  
    exeResp.upd(t, True);
    
  endmethod

  method Action mem_start(TokIndex t);
  
  
    if (!exeResp.sub(t))
    begin
      $display("ERROR: Funcp Exception! TokState: Meming without finishing Execute on Token %0d", t);
      $finish(1);
    end
    
    memReq.upd(t, True);
    
  endmethod

  method Action mem_finish(TokIndex t);
  
    if (!memReq.sub(t))
    begin
      $display("ERROR: Funcp Exception! TokState: Finishing Mem without beginning on Token %0d", t);
      $finish(1);
    end
  
    memResp.upd(t, True);
    
  endmethod

  method Action finalize(TokIndex t);
  
    if (!memResp.sub(t))
    begin
      $display("ERROR: Funcp Exception! TokState: Finalizing without finishing Mem on Token %0d", t);
      $finish(1);
    end
    
    finalized.upd(t, True);
    
  endmethod
  
  method Bool isAllocated(TokIndex t);
  
    return alloc[t];
  
  endmethod
  
  method Bool isReady(TokIndex t);
  
    return (!is_load.sub(t) && exeResp.sub(t)) || memResp.sub(t);
  
  endmethod
  
  method Bool isLoad(TokIndex t);
  
    return is_load.sub(t);
  
  endmethod
  
  method Bool isStore(TokIndex t);
  
    return is_store.sub(t);
  
  endmethod
  
  method Action rewindTo(TokIndex t);
    
    Vector#(TExp#(idx_SZ), Bool) as = newVector();
    
    for (Integer x = 0; x < valueof(TExp#(idx_SZ)); x = x + 1)
    begin
      TokIndex cur = fromInteger(x);
      as[x] = (cur > t) && (cur <= youngest_tok) ? False : alloc[x];
    end
  
    alloc <= as;
    
  endmethod

  method TokIndex youngest();
  
    return youngest_tok;
  
  endmethod

  method TokIndex oldest();
  
    return oldest_tok;
  
  endmethod
  
endmodule
