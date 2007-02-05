import GetPut::*;
import RegFile::*;
import FIFO::*;
import Vector::*;
import ConfigReg::*;

import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_funcp_base::*;
import hasim_isa::*;

import FUNCP_FreeList::*;


//----------------------------------------------------------------------------------
// Bypass
//----------------------------------------------------------------------------------

module [HASim_Module] mkFUNCP_Regstate
    //interface:
                ()
    provisos
            (Bits#(RName,       rname_SZ),
             Bits#(PRName,      prname_SZ),
             Bits#(RegData,     data_SZ),
             Bits#(Flags,       flags_SZ),
             Bits#(Token,       token_SZ),
             Bits#(SnapshotPtr, snapshotptr_SZ),
             Add#(rname_SZ, rdiff_TMP, prname_SZ),

             Add#(snapshotptr_SZ, sdiff_TMP, token_SZ));
  
  
  BRAM_3#(PRName, Bool) prf_valid <- mkBRAM_3_Full();
  BRAM_3#(PRName, RegData) prf[valueOf(NumRegDataItems)];
  BRAM_3#(PRName, Flags) prf_flags;

  for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
  begin
    prf[index] <- mkBRAM_3_Full();
  end
  prf_flags <- mkBRAM_3_Full();

  //map table 
  function PRName logicalRegIdentity (Integer a);
    RName pra = fromInteger(a);
    RName prl = pack(REG_LAST);
    return (pra < prl) ? pra : 0;
  endfunction

  Vector#(TExp#(rname_SZ), PRName) initmap = genWith(logicalRegIdentity);
  Reg#(Vector#(TExp#(rname_SZ), PRName)) maptbl <- mkReg(initmap); // init with [0 .. REG_LAST-1, 0 .. 0]

  function PRName lookup(RName r);
     return select(maptbl._read(), r);
  endfunction 

  FreeList                       freelist <- mkFreeList();

  //rob                                   old
  BRAM_2#(PRName, Tuple3#(Token, Maybe#(RName), PRName))  rob      <- mkBRAM_2_Full();
  Reg#(PRName)                                            rob_old  <- mkReg(0);
  Reg#(PRName)                                            rob_new  <- mkReg(0);

  Reg#(Vector#(TExp#(snapshotptr_SZ), Bool))  snap_valids        <- mkReg(unpack(0));
  Reg#(Vector#(TExp#(snapshotptr_SZ), TokIndex)) snap_ids        <- mkRegU();
                             //fl_read  rob_new     map
  BRAM#(SnapshotPtr, Tuple3#(PRName, PRName, Vector#(TExp#(rname_SZ), PRName))) snaps <- mkBRAM_Full();

  Reg#(TokEpoch) epoch <- mkReg(0);
  Reg#(Bool)  busy <- mkReg(True);
  Reg#(Token) stopToken <- mkRegU();
  FIFO#(Tuple3#(Maybe#(RName), Token, Bool)) mappingQ <- mkFIFO();
  FIFO#(Tuple3#(Maybe#(RName), Token, Bool)) waitingQ <- mkFIFO();

  // FIXME just to add the segment interface
  FIFO#(Bit#(0)) segQ <- mkFIFO(); // just to synchronize, no data

  Reg#(Bool) initializing <- mkReg(True);
  Reg#(PRName) init_count <- mkReg(0);
  FIFO#(Token) flatteningQ   <- mkFIFO();

  Bool free_ss = pack(snap_valids) != ~0;

  //Connections
  Connection_Server#(Tuple3#(Maybe#(RName), Token, Bool), 
                     PRName) 
  //...
        link_mapping <- mkConnection_Server("dec_to_bypass_mapping");

  Connection_Server#(RName, PRName) 
  //...
        link_lookup1 <- mkConnection_Server("dec_to_bypass_lookup1");

  Connection_Server#(RName, PRName) 
  //...
        link_lookup2 <- mkConnection_Server("dec_to_bypass_lookup2");

  Connection_Server#(RName, PRName) 
  //...
        link_lookup3 <- mkConnection_Server("dec_to_bypass_lookup3");

  Connection_Server#(RName, PRName) 
  //...
        link_lookup4 <- mkConnection_Server("dec_to_bypass_lookup4");

  Connection_Server#(PRName, Maybe#(RegReadValue))
  //...
        link_read1 <- mkConnection_Server("exe_to_bypass_read1");

  Connection_Server#(PRName, Maybe#(RegReadValue))
  //...
        link_read2 <- mkConnection_Server("exe_to_bypass_read2");

  Connection_Server#(PRName, Maybe#(RegReadValue))
  //...
        link_read3 <- mkConnection_Server("exe_to_bypass_read3");

  // FIXME this segment read port probably needs a different data type
  Connection_Server#(PRName, Maybe#(RegReadValue))
  //...
        link_read4 <- mkConnection_Server("exe_to_bypass_read4");

  Connection_Receive#(Tuple2#(PRName, RegWriteValue)) 
  //...
        link_write1 <- mkConnection_Receive("exe_to_bypass_write1");

  Connection_Receive#(Tuple2#(PRName, RegWriteValue)) 
  //...
        link_write2 <- mkConnection_Receive("mem_to_bypass_write2");

  Connection_Receive#(Token) 
  //...
        link_freePReg <- mkConnection_Receive("lco_to_bypass_free");

  Connection_Receive#(Token) 
  //...
        link_rewindToToken <- mkConnection_Receive("fp_rewindToToken");

  (* descending_urgency = "finish_Mapping, rewindToToken, freePReg" *)
  (* descending_urgency = "unBusy, freelist.finish_free, finishFastRewind, continue_Mapping" *)

  rule initialize (initializing);
  
    prf_valid.write(init_count, True); // all registers are valid
    for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
    begin
      prf[index].write(init_count, 0); // all registers are 0
    end
    prf_flags.write(init_count, flagsClear()); // all registers flags are 0

    let new_count = init_count + 1;
    init_count <= new_count;
  
    // on overflow we're done initializing
    if (new_count == 0)
      begin
        initializing <= False;
        busy <= False;
      end

  endrule
    

  //unBusy
  
  rule unBusy (busy && !initializing);
  
    //if newest token is stopToken or we ran out unbusy
    match {.tok, .mx, .oldp} <- rob.read_resp1();
    
    $display("REGSTATE unBusy: rob.read_resp1 [%0d], valid=%d", tok.index, isJust(mx));
    if (tok == stopToken || (rob_new == rob_old + 1))
      begin
        $display("REGSTATE unBusy: done [%0d], stopToken=%0d, rob_old=%0d rob_new=%0d", tok.index, stopToken.index, rob_old, rob_new);
        busy <= False;

        // can't figure out how to make this a "function" that compiles OK
        // display_maptbl
        $write("REGSTATE maptbl");
        RName prl = pack(REG_LAST);
        for(Integer vreg = 0; fromInteger(vreg) < prl; vreg = vreg + 1)
        begin
          if (vreg % 8 == 0)
          begin
            $write("\n  ");
          end
          $write("%-6s->P%-2d, ", getRegNameString(unpack(fromInteger(vreg))), maptbl[vreg]);
        end
        $display("");

      end
    else
      begin
        $display("REGSTATE unBusy: rob.read_req1 %0d", rob_new - 1);
        rob.read_req1(rob_new - 1);
      end
       
   //back up maptable
   if (isJust(mx))
     begin
       $display("REGSTATE unBusy: [%0d] maptbl update %0s -> P%0d", tok.index, getRegNameString(unpack(unJust(mx))), oldp);
       maptbl <= update(maptbl, unJust(mx), oldp);
     end

   //back up (freelist)
   freelist.back();
 
   //backup (rob)
   rob_new <= rob_new - 1;
   
  endrule


  //begin_Mapping

  rule begin_Mapping (!busy);
    
    //{Maybe#(RName), Token, Bool} 
    match {.mx, .tok, .ss} <- link_mapping.getReq();
    RName rname = unJust(mx);
    // pseudo registers do not allow remapping
    if (isJust(mx) &&& (rname >= pack(REG_LAST)))
    begin
      mx = Invalid;
    end
    mappingQ.enq(tuple3(mx, tok, ss));
  endrule

  //continue_Mapping
  
  rule continue_Mapping (!busy &&& mappingQ.first() matches {.mx, .tok, .ss} &&& tok.info.epoch == epoch);
    $display("REGSTATE mapping request: [%0d] %0s valid=%0d, snapshot=%0d freelist=%0d",
      tok.index, getRegNameString(unpack(unJust(mx))), isJust(mx), ss, freelist.current());
    //take off freelist
    freelist.forward_req();
    mappingQ.deq();
    waitingQ.enq(mappingQ.first());
  endrule

  //finish_Mapping

  rule finish_Mapping (!busy);
    
    match {.mx, .tok, .ss} = waitingQ.first();
    waitingQ.deq();

    PRName oldPReg; 
    PRName newPReg;
  
    PRName nPReg <- freelist.forward_resp();
  
    if (isJust(mx))
      begin
        oldPReg = select(maptbl, unJust(mx));
        newPReg = nPReg;
      end
    else
      begin
        oldPReg = nPReg;
        newPReg = ?;
       end

    // update map
    Vector#(TExp#(rname_SZ), PRName) new_map = maptbl;
    if (isJust(mx))
      begin
        new_map = update(new_map, unJust(mx), newPReg);
        maptbl <= new_map;
      end

    // write rob
    let rob_updated = rob_new + 1;
    rob_new <= rob_updated;
    rob.write(rob_new, tuple3(tok, mx, newPReg));
    $display("REGSTATE rob alloc %0d: [%0d]", rob_new, tok.index);

    // make snapshot if needed
    Maybe#(SnapshotPtr) midx = Nothing;

  //INDEX sequence
    Bit#(snapshotptr_SZ) ti_bits = truncate(pack(tok));
    SnapshotPtr ti = unpack(ti_bits);
    if(!select(snap_valids, ti))
      midx = Just(ti);
//r2r: disable snapshots for now
midx = Nothing;

    //for(Integer i = 0; i < valueOf(TExp#(ssz)); i = i + 1) // 
    //  if(!snap_valids[i])
    //    midx = Just(fromInteger(i));
  //END INDEX sequence


    //snapshot only when it makes sense

    midx = (ss) ? midx : Nothing;

    if (isJust(midx))
      begin
        let idx = unJust(midx);
        snap_valids     <= update(snap_valids, idx, True);
        snap_ids        <= update(snap_ids   , idx, tok.index);
        snaps.write(idx, tuple3(freelist.current(), rob_updated, new_map));
        $display("REGSTATE made snapshot %0d: [%0d]", idx, tok.index);
        $display("REGSTATE snapshot %0d: freelist=%0d rob=%0d", idx, freelist.current(), rob_updated);
      end
    else
      begin
        if (ss)
          $display("REGSTATE no snapshot (%0d valid=%0d): [%0d]", ti, snap_valids[ti], tok.index);
      end

    // display_maptbl
    // can't figure out how to make this a "function" that compiles OK
    $write("REGSTATE maptbl");
    RName prl = pack(REG_LAST);
    for(Integer vreg = 0; fromInteger(vreg) < prl; vreg = vreg + 1)
    begin
      if (vreg % 8 == 0)
      begin
        $write("\n  ");
      end
      $write("%-6s->P%-2d, ", getRegNameString(unpack(fromInteger(vreg))), new_map[vreg]);
    end
    $display("");

    freelist.setOldPReg(tok, oldPReg);
    
    $display("REGSTATE mapping request: [%0d] %0s valid=%0d, snapshot=%0d => P%0d, old P%0d",
      tok.index, getRegNameString(unpack(unJust(mx))), isJust(mx), ss, newPReg, oldPReg);

    // return value
    link_mapping.makeResp(newPReg);
  endrule

  //lookup: used by Decode
  rule lookup1 (True);
    
    let rnm <- link_lookup1.getReq();
    let preg = lookup(rnm);
    link_lookup1.makeResp(preg);
    $display("REGSTATE Lookup1: %0s -> P%0d", getRegNameString(unpack(rnm)), preg);
    
  endrule
  
  rule lookup2 (True);
    
    let rnm <- link_lookup2.getReq();
    let preg = lookup(rnm);
    link_lookup2.makeResp(preg);
    $display("REGSTATE Lookup2: %0s -> P%0d", getRegNameString(unpack(rnm)), preg);
    
  endrule

  rule lookup3 (True);
    
    let rnm <- link_lookup3.getReq();
    let preg = lookup(rnm);
    link_lookup3.makeResp(preg);
    $display("REGSTATE Lookup3: %0s -> P%0d", getRegNameString(unpack(rnm)), preg);
    
  endrule

  rule lookup4 (True);
    
    let rnm <- link_lookup4.getReq();
    let preg = lookup(rnm);
    link_lookup4.makeResp(preg);
    $display("REGSTATE Lookup4: %0s -> P%0d", getRegNameString(unpack(rnm)), preg);
    
  endrule

  //read: used by Execute

  rule read_req1 (True);
  
    let prnm <- link_read1.getReq();

    prf_valid.read_req1(prnm);
    for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
    begin
      prf[index].read_req1(prnm);
    end
    prf_flags.read_req1(prnm);
  
  endrule
  
  rule read_resp1 (True);

    let data_valid <- prf_valid.read_resp1();
    RegReadValueData data = newVector();

    for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
    begin
      data[index] <- prf[index].read_resp1();
    end
    let flags <- prf_flags.read_resp1();

    let regReadValue = RegReadValue {data: data, flags: flags};
    Maybe#(RegReadValue) response = (data_valid ? Valid regReadValue : Invalid);
    link_read1.makeResp(response);

  endrule

  rule read_req2 (True);
  
    let prnm <- link_read2.getReq();

    prf_valid.read_req2(prnm);
    for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
    begin
      prf[index].read_req2(prnm);
    end
    prf_flags.read_req2(prnm);
  
  endrule
  
  rule read_resp2 (True);

    let data_valid <- prf_valid.read_resp2();
    RegReadValueData data = newVector();

    for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
    begin
      data[index] <- prf[index].read_resp2();
    end
    let flags <- prf_flags.read_resp2();

    let regReadValue = RegReadValue {data: data, flags: flags};
    Maybe#(RegReadValue) response = (data_valid ? Valid regReadValue : Invalid);
    link_read2.makeResp(response);

  endrule

  rule read_req3 (True);
  
    let prnm <- link_read3.getReq();

    prf_valid.read_req3(prnm);
    for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
    begin
      prf[index].read_req3(prnm);
    end
    prf_flags.read_req3(prnm);
  
  endrule
  
  rule read_resp3 (True);

    let data_valid <- prf_valid.read_resp3();
    RegReadValueData data = newVector();

    for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
    begin
      data[index] <- prf[index].read_resp3();
    end
    let flags <- prf_flags.read_resp3();

    let regReadValue = RegReadValue {data: data, flags: flags};
    Maybe#(RegReadValue) response = (data_valid ? Valid regReadValue : Invalid);
    link_read3.makeResp(response);

  endrule

  rule read_req4 (True);
  
    let prnm <- link_read4.getReq();
    segQ.enq(0);
  
  endrule
  
  rule read_resp4 (True);

    segQ.deq(); // just to synchronize, no data

    RegReadValueData data = replicate(0);
    let regReadValue = RegReadValue {data: data, flags: flagsClear()};
    Maybe#(RegReadValue) response = Valid regReadValue;
    link_read4.makeResp(response);

  endrule

  //write{1,2}
  //1 used by Execute
  //2 used by Mem

  (* descending_urgency = "initialize, write2, write" *)
  rule write (True);
  
    match {.prnm, .value} <- link_write1.receive();
    let data = value.data;
    let flags = value.flags;

    prf_valid.write(prnm, True);
    for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
    begin
      if (isJust(data[index]))
      begin
        prf[index].write(prnm, unJust(data[index]));
      end
    end
    prf_flags.write(prnm, flags);

  endrule
  
  rule write2 (True);
  
    match {.prnm, .value} <- link_write2.receive();
    let data = value.data;
    let flags = value.flags;

    prf_valid.write(prnm, True);
    for (Integer index = 0; index < valueOf(NumRegDataItems); index = index + 1)
    begin
      if (isJust(data[index]))
      begin
        prf[index].write(prnm, unJust(data[index]));
      end
    end
    prf_flags.write(prnm, flags);
  
  endrule

  //freePReg (used by Local Commit)
  
  rule freePReg (!busy);
  
    let tok <- link_freePReg.receive();
    
    freelist.free(tok);

    $display("REGSTATE rob free %0d: [%0d]", rob_old, tok.index);
    rob_old <= rob_old + 1;

    function f(s,t) = s && (t != tok.index);

    let newvals = zipWith(f, snap_valids, snap_ids);

    snap_valids <= newvals;

    // can't figure out how to make this a "function" that compiles OK
    // display_snapshot_kill
    for(Integer idx = 0; idx < valueOf(TExp#(snapshotptr_SZ)); idx = idx + 1)
    begin
      if (snap_valids[idx] && snap_ids[idx] == tok.index)
      begin
        $display("REGSTATE killing snapshot %d for token %d", idx, tok.index);
      end
    end
  
  endrule
  
  rule rewindToToken (!busy);
    
    let tok <- link_rewindToToken.receive();
    
    epoch <= epoch + 1;
    
    //NO!!!!!
     
    //see if token is snapshotted
  //INDEX function 
    Maybe#(SnapshotPtr) midx = Nothing;

    Bit#(snapshotptr_SZ) idx_bits = truncate(pack(tok));
    SnapshotPtr idx = unpack(idx_bits);
    
    $display("REGSTATE restore to snapshot %0d: [%0d]", idx, tok.index);

    if (select(snap_valids, idx))
       midx = Just(idx);

    //for(Integer i = 0; i < valueOf(TExp#(ssz)); i = i + 1) // 
    //  if(!snap_valids[i] && (snap_ids[i] == tok))
    //    midx = Just(fromInteger(i));
  //END INDEX function

    case (midx) matches
      tagged Just .i:
        begin
          $display("REGSTATE restore to snapshot %0d: [%0d]", i, tok.index);
          snaps.read_req(i);
        end
      tagged Nothing:   //if not write busy and record token
        begin
          $display("REGSTATE restore to snapshot: [%0d] slow rewind", tok.index);
          busy <= True;
          $display("REGSTATE rob.read_req1 %0d", rob_new);
          rob.read_req1(rob_new);
          stopToken <= tok;
        end
    endcase

    //flatten dead snaps
    rob.read_req2(rob_old);
    flatteningQ.enq(tok);
  
  endrule
  
  rule flattenDeadSnaps (True);

    let tok = flatteningQ.first();
    flatteningQ.deq();
    
    match {.oldTok, .*, .*} <- rob.read_resp2();

    function Bool flatten(Bool x, TokIndex t) = x && (tok.index-oldTok.index > t - oldTok.index); //valid and older than tok stay
     
    snap_valids <= zipWith(flatten, snap_valids, snap_ids);

    // can't figure out how to make this a "function" that compiles OK
    // display_snapshot_flatten
    for(Integer idx = 0; idx < valueOf(TExp#(snapshotptr_SZ)); idx = idx + 1)
    begin
      let id = snap_ids[idx];
      if (snap_valids[idx] && tok.index-oldTok.index <= id - oldTok.index)
      begin
        $display("REGSTATE flattening snapshot %d for token %d", idx, tok.index);
      end
    end
    
  endrule
  
  rule finishFastRewind (True);
  
    match {.flt, .robt, .mapt} <- snaps.read_resp();

    $display("REGSTATE rewind to snapshot: freelist=%0d rob=%0d", flt, robt);

    // can't figure out how to make this a "function" that compiles OK
    // display_maptbl
    $write("REGSTATE maptbl");
    RName prl = pack(REG_LAST);
    for(Integer vreg = 0; fromInteger(vreg) < prl; vreg = vreg + 1)
    begin
      if (vreg % 8 == 0)
      begin
        $write("\n  ");
      end
      $write("%-6s->P%-2d, ", getRegNameString(unpack(fromInteger(vreg))), mapt[vreg]);
    end
    $display("");

    maptbl  <= mapt;
    freelist.backTo(flt);
    rob_new <= robt;
    
  endrule
  
endmodule
