import GetPut::*;
import RegFile::*;
import FIFO::*;
import Vector::*;
import ConfigReg::*;

import fpga_components::*;
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
	     Bits#(Value,       value_SZ),
	     Bits#(Token,       token_SZ),
             Bits#(SnapshotPtr, snapshotptr_SZ),
	     Add#(rname_SZ, rdiff_TMP, prname_SZ),
	     Add#(snapshotptr_SZ, sdiff_TMP, token_SZ));
  
  
  BRAM_2#(PRName, Maybe#(Value)) prf <- mkBRAM_2_Full();

  //map table 
  
  Vector#(TExp#(rname_SZ), PRName) initmap = replicate(0); //map(fromInteger, genVector);
  Reg#(Vector#(TExp#(rname_SZ), PRName)) maptbl <- mkReg(initmap); // init with [0 .. max]

  function PRName lookup(RName r);
     return (r == 0) ? 0 : select(maptbl._read(), r);
  endfunction 

  FreeList                       freelist <- mkFreeList();

  //rob                                   old
  BRAM_2#(PRName, Tuple3#(Token, Maybe#(RName), PRName))  rob      <- mkBRAM_2_Full();
  Reg#(PRName)   					  rob_old  <- mkReg(0);
  Reg#(PRName) 						  rob_new  <- mkReg(0);

  Reg#(Vector#(TExp#(snapshotptr_SZ), Bool))  snap_valids        <- mkReg(unpack(0));
  Reg#(Vector#(TExp#(snapshotptr_SZ), TokIndex)) snap_ids        <- mkRegU();
                             //fl_read  rob_new     map
  BRAM#(SnapshotPtr, Tuple3#(PRName, PRName, Vector#(TExp#(rname_SZ), PRName))) snaps <- mkBRAM_Full();


  Reg#(TokEpoch) epoch <- mkReg(minBound);
  Reg#(TokEpoch) dead_epoch <- mkReg(maxBound);
  Reg#(Bool)  busy <- mkReg(True);
  Reg#(Token) stopToken <- mkRegU();
  FIFO#(Tuple5#(Maybe#(RName), Token, Bool, RName, RName)) mappingQ <- mkFIFO();

  Reg#(Bool) initializing <- mkReg(True);
  FIFO#(Token) flatteningQ   <- mkFIFO();

  Bool free_ss = pack(snap_valids) != ~0;

  //Connections
  Connection_Server#(Tuple5#(Maybe#(RName), Token, Bool, RName, RName), 
                     Tuple3#(PRName, PRName, PRName)) 
  //...
        link_mapping <- mkConnection_Server("dec_to_bypass_mapping");

  Connection_Server#(PRName, Maybe#(Value)) 
  //...
        link_read1 <- mkConnection_Server("exe_to_bypass_read1");

  Connection_Server#(PRName, Maybe#(Value)) 
  //...
        link_read2 <- mkConnection_Server("exe_to_bypass_read2");

  Connection_Receive#(Tuple2#(PRName, Value)) 
  //...
        link_write1 <- mkConnection_Receive("exe_to_bypass_write1");

  Connection_Receive#(Tuple2#(PRName, Value)) 
  //...
        link_write2 <- mkConnection_Receive("mem_to_bypass_write2");

  Connection_Receive#(Token) 
  //...
        link_freePReg <- mkConnection_Receive("lco_to_bypass_free");

  Connection_Receive#(Token) 
  //...
        link_rewindToToken <- mkConnection_Receive("fp_rewindToToken");

  rule initialize (initializing);
  
    prf.write(0, tagged Valid 0); //R0 is hard-coded to 0.
    busy <= False;
    initializing <= False;
  
  endrule
    

  //unBusy
  
  rule unBusy (busy && !initializing);
  
    //if newest token is stopToken or we ran out unbusy
    match {.tok, .mx, .oldp} <- rob.read_resp1();
    $display("RegState: Rolling back [%d], %b(R%d), PR%d", tok.index, isJust(mx), unJust(mx), oldp);
    
    if (tok.index == stopToken.index || (rob_new == rob_old + 1))
       busy <= False;
    else
    begin
       //back up maptable
       if (isJust(mx))
         maptbl <= update(maptbl, unJust(mx), oldp);

       //back up (freelist)
       freelist.back();
 
       //backup (rob)
       rob_new <= rob_new - 1;
       
       //Get ready for next cycle
       rob.read_req1(rob_new - 1);
    end
   
  endrule


  rule begin_Mapping (!busy);
     
    match {.mx, .tok, .ss, .r1, .r2} <- link_mapping.getReq();
    
    if (tok.info.epoch == dead_epoch) //It's a dead request so return junk
    begin
      $display("REGSTATE begin_Mapping JUNK: %0b(%0d), %0d, %0d", isJust(mx), unJust(mx), tok.index, ss);
      link_mapping.makeResp(?);
    end
    else
    begin
    
      $display("REGSTATE begin_Mapping: %0b(%0d), %0d, %0d", isJust(mx), unJust(mx), tok.index, ss);

      freelist.forward_req();

      mappingQ.enq(tuple5(mx, tok, ss, r1, r2));
     
    end    
  endrule

  //finish_Mapping

  rule finish_Mapping (!busy);
    
    match {.mx, .tok, .ss, .r1, .r2} = mappingQ.first();
    mappingQ.deq();

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
    if (isJust(mx))
      begin
        $display("New Mapping: R%0d/PR%0d", unJust(mx), newPReg);
	Vector#(TExp#(rname_SZ), PRName) new_map = maptbl;
	new_map = update(new_map, unJust(mx), newPReg);
	maptbl <= new_map;
	prf.write(newPReg, tagged Invalid);
      end

    // write rob
    rob_new <= rob_new + 1;
    rob.write(rob_new, tuple3(tok, mx, oldPReg)); //newPReg));
    $display("RegState: Updating ROB [%d], %b(R%d), PR%d", tok.index, isJust(mx), unJust(mx), oldPReg);

    // make snapshot if needed
    Maybe#(SnapshotPtr) midx = Nothing;

  //INDEX sequence
    Bit#(snapshotptr_SZ) ti_bits = truncate(pack(tok.index));
    SnapshotPtr ti = unpack(ti_bits);
    if(!select(snap_valids, ti))
      midx = Just(ti);

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
        snaps.write(idx, tuple3(freelist.current(), rob_new, maptbl));
      end
    
    freelist.setOldPReg(tok, oldPReg);
    
    //Do lookups
    PRName pr1 = lookup(r1);
    PRName pr2 = lookup(r2);
    
    $display("Lookup1: R%0d/PR%0d", r1, pr1);
    $display("Lookup2: R%0d/PR%0d", r2, pr2);
    
    // return value
    link_mapping.makeResp(tuple3(newPReg, pr1, pr2));
  endrule

  //read{1, 2} used by Execute

  rule read_req1 (True);
  
    let prnm <- link_read1.getReq();
    prf.read_req1(prnm);
  
  endrule
  
  rule read_resp1 (True);

    let res <- prf.read_resp1();
    link_read1.makeResp(res);
  
  endrule

  rule read_req2 (True);
  
    let prnm <- link_read2.getReq();
    prf.read_req2(prnm);
  
  endrule
  
  rule read_resp2 (True);

    let res <- prf.read_resp2();
    link_read2.makeResp(res);
  
  endrule
/*
  //read{3, 4} used by Mem

  rule read_req3 (True);
  
    let prnm <- link_read3.getReq();
    prf.read_req3(prnm);
  
  endrule
  
  rule read_resp3 (True);

    let res <- prf.read_resp3();
    link_read3.makeResp(res);
  
  endrule

  rule read_req4 (True);
  
    let prnm <- link_read4.getReq();
    prf.read_req4(prnm);
  
  endrule
  
  rule read_resp4 (True);

    let res <- prf.read_resp4();
    link_read4.makeResp(res);
  
  endrule
*/
  //write{1,2}
  //1 used by Execute
  //2 used by Mem

  (* descending_urgency = "write2, write" *)
  rule write (True);
  
    match {.prnm, .val} <- link_write1.receive();
    
    if (prnm == 0)
    begin
      $display("ERROR: WRITE1 to PR0 <= 0x%0h", val);
      $finish(1);
    end
    
    prf.write(prnm, tagged Valid val);
    
  endrule
  
  rule write2 (True);
  
    match {.prnm, .val} <- link_write2.receive();
    
    if (prnm == 0)
    begin
      $display("ERROR: WRITE2 to PR0 <= 0x%0h", val);
      $finish(1);
    end
    
    prf.write(prnm, tagged Valid val);
  
  endrule

  //freePReg (used by Local Commit)
  
  rule freePReg (!busy);
  
    let tok <- link_freePReg.receive();
    
    freelist.free(tok);

    rob_old <= rob_old + 1;

    function f(s,t) = s && (t != tok.index);

    let newvals = zipWith(f, snap_valids, snap_ids);

    snap_valids <= newvals;
  
  endrule
  
  rule rewindToToken (!busy);
    
    
    let tok <- link_rewindToToken.receive();
    $display("REGSTATE rewindToToken: %0d", tok.index);
    
    dead_epoch <= epoch;
    epoch <= epoch + 1;
    
    //NO!!!!!
     
    //see if token is snapshotted
  //INDEX function 
    Maybe#(SnapshotPtr) midx = Nothing;

    Bit#(snapshotptr_SZ) idx_bits = truncate(pack(tok.index));
    SnapshotPtr idx = unpack(idx_bits);
    
    if (select(snap_valids, idx))
       midx = Just(idx);

    //for(Integer i = 0; i < valueOf(TExp#(ssz)); i = i + 1) // 
    //  if(!snap_valids[i] && (snap_ids[i] == tok))
    //    midx = Just(fromInteger(i));
  //END INDEX function

    case (midx) matches
      tagged Just .i:
        begin
	  snaps.read_req(i);
        end
      tagged Nothing:   //if not write busy and record token
        begin
          busy <= True;
	  rob.read_req1(rob_new - 1);
	  rob_new <= rob_new - 1;
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
    
  endrule
  
  rule finishFastRewind (True);
  
    match {.flt, .robt, .mapt} <- snaps.read_resp();

    maptbl  <= mapt;
    freelist.backTo(flt);
    rob_new <= robt;
    
  endrule
  
endmodule

