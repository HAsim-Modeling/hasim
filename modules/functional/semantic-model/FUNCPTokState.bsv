
import hasim_common::*;

import Vector::*;

//A One-Hot encoding of Token State to get maximum throughput

typedef Vector#(TokIndex, Reg#(Bool)) TokStateRF;

Vector#(TokIndex, Bool) initstate = replicate(False);

module mkTokStateRF (TokStateRF);

  TokStateRF rf <- replicateM(mkReg(initstate));

endmodule

interface FUNCP_TokState;

  method Action deallocate(TokIndex t);
  method Action allocate(TokIndex t);
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
  
  method Bool isBusy(TokIndex t);
  method Bool isReady(TokIndex t);
  method Bool isLoad(TokIndex t);
  method Bool isStore(TokIndex t);
  
endinterface

module [HASim_Module] mkFUNCP_TokState (FUNCP_TokState);

  TokStateRF alloc      <- mkTokStateRF();
  TokStateRF fetReq     <- mkTokStateRF();
  TokStateRF fetResp    <- mkTokStateRF();
  TokStateRF decReq     <- mkTokStateRF();
  TokStateRF decResp    <- mkTokStateRF();
  TokStateRF is_load    <- mkTokStateRF();
  TokStateRF is_store   <- mkTokStateRF();
  TokStateRF exeReq     <- mkTokStateRF();
  TokStateRF exeResp    <- mkTokStateRF();
  TokStateRF memReq     <- mkTokStateRF();
  TokStateRF memResp    <- mkTokStateRF();
  TokStateRF final      <- mkTokStateRF();

  method Action deallocate(TokIndex t);
  
    if (!alloc[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Deallocating un-allocated Token %0d", t);
      $finish(1);
    end
    
    alloc[t] <= False;
    
  endmethod
  
  method Action allocate(TokIndex t);
  
    if (alloc[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Allocating already-allocated Token %0d.", t);
      $finish(1);
    end
    
    alloc[t] <= True;
    fetReq[t] <= False;
    fetResp[t] <= False;
    decReq[t] <= False;
    decResp[t] <= False;
    needsMem[t] <= False
    exeReq[t] <= False;
    exeResp[t] <= False;
    memReq[t] <= False;
    memResp[t] <= False;
    final[t] <= False;
    
  endmethod
  
  method Action fet_start(TokIndex t);
  
    if (!alloc[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Fetching un-allocated Token %0d", t);
      $finish(1);
    end
  
    fetReq[t] <= True;
    
  endmethod

  method Action fet_finish(TokIndex t);
  
    if (!fetReq[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Finishing Fetch without beginning on Token %0d", t);
      $finish(1);
    end
  
    fetResp[t] <= True;
    
  endmethod

  method Action dec_start(TokIndex t);
  
    if (!fetResp[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Decoding without finishing Fetch on Token %0d", t);
      $finish(1);
    end
    
    decReq[t] <= True;
    
  endmethod

  method Action dec_finish(TokIndex t);
  
    if (!decReq[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Finishing Decode without beginning on Token %0d", t);
      $finish(1);
    end
  
    decResp[t] <= True;
    
  endmethod
  
  method Action is_a_load(TokIndex t);
    
    is_load[t] <= True;
    
  endmethod
  
  method Action is_a_store(TokIndex t);
    
    is_store[t] <= True;
    
  endmethod
  
  method Action exe_start(TokIndex t);
  
  
    if (!decResp[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Executing without finishing Decode on Token %0d", t);
      $finish(1);
    end
    
    exeReq[t] <= True;
    
  endmethod

  method Action exe_finish(TokIndex t);
  
    if (!exeReq[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Finishing Execute without beginning on Token %0d", t);
      $finish(1);
    end
  
    exeResp[t] <= True;
    
  endmethod

  method Action mem_start(TokIndex t);
  
  
    if (!exeResp[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Meming without finishing Execute on Token %0d", t);
      $finish(1);
    end
    
    memReq[t] <= True;
    
  endmethod

  method Action mem_finish(TokIndex t);
  
    if (!memReq[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Finishing Mem without beginning on Token %0d", t);
      $finish(1);
    end
  
    memResp[t] <= True;
    
  endmethod

  method Action finalize(TokIndex t);
  
    if (!memResp[t])
    begin
      $display("ERROR: Funcp Exception! TokState: Finalizing without finishing Mem on Token %0d", t);
      $finish(1);
    end
    
    final[t] <= True;
    
  endmethod
  
  method Bool isBusy(TokIndex t);
  
    let fet_busy = fetReq[t] && !fetResp[t];
    let dec_busy = decReq[t] && !decResp[t];
    let exe_busy = exeReq[t] && !exeResp[t];
    let mem_busy = memReq[t] && !memResp[t];
  
    return fet_busy || dec_busy || exe_busy || mem_busy;
  
  endmethod
  
  method Bool isReady(TokIndex t);
  
    return (!is_load[t] && exeResp[t]) || memResp[t];
  
  endmethod
  
  method Bool isLoad(TokIndex t);
  
    return is_load[t];
  
  endmethod
  
  method Bool isStore(TokIndex t);
  
    return is_store[t];
  
  endmethod
  
endmodule
