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
	     Bits#(Value,       value_SZ),
	     Bits#(Token,       token_SZ),
             Bits#(SnapshotPtr, snapshotptr_SZ),
	     Add#(rname_SZ, rdiff_TMP, prname_SZ),
	     Add#(snapshotptr_SZ, sdiff_TMP, token_SZ));
  
  
  BRAM_2#(PRName, Maybe#(Value)) prf <- mkBRAM_2_Full();

  //map table 
  
  Vector#(TExp#(rname_SZ), PRName) initmap = map(fromInteger, genVector);
  Reg#(Vector#(TExp#(rname_SZ), PRName)) maptbl <- mkReg(initmap); // init with [0 .. max]

  function PRName lookup(RName r);
     return select(maptbl._read(), r);
  endfunction 

  FreeList                       freelist <- mkFreeList();

  //rob                                   old
  RegFile#(PRName, Tuple3#(Token, Maybe#(RName), PRName)) rob      <- mkRegFileFull();
  Reg#(PRName)   					  rob_old  <- mkReg(0);
  Reg#(PRName) 						  rob_new  <- mkReg(0);

  Reg#(Vector#(TExp#(snapshotptr_SZ), Bool))  snap_valids        <- mkReg(unpack(0));
  Reg#(Vector#(TExp#(snapshotptr_SZ), TokIndex)) snap_ids           <- mkRegU();
  RegFile#(SnapshotPtr, PRName)               snap_flreadptrs    <- mkRegFileFull();
  RegFile#(SnapshotPtr, PRName)               snap_robnewptrs    <- mkRegFileFull(); 
  RegFile#(SnapshotPtr, Vector#(TExp#(rname_SZ), PRName))  snaps <- mkRegFileFull();


  Reg#(TokEpoch) epoch <- mkReg(0);
  Reg#(Bool)  busy <- mkReg(True);
  Reg#(Token) stopToken <- mkRegU();
  FIFO#(Tuple3#(Maybe#(RName), Token, Bool)) mappingQ <- mkFIFO();
  FIFO#(Tuple3#(Maybe#(RName), Token, Bool)) waitingQ <- mkFIFO();

  Reg#(Bool) initializing <- mkReg(True);

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
  
    prf.write(0, Valid 0); //R0 is hard-coded to 0.
    busy <= False;
    initializing <= False;
  
  endrule
    

  //unBusy
  
  rule unBusy (busy && !initializing);
  
    //if newest token is stopToken or we ran out unbusy
    match {.tok, .mx, .oldp} = rob.sub(rob_new);
    
    if (tok == stopToken || (rob_new == rob_old + 1))
       busy <= False;
       
   //back up maptable
   if (isJust(mx))
     maptbl <= update(maptbl, unJust(mx), oldp);

   //back up (freelist)
   freelist.back();
 
   //backup (rob)
   rob_new <= rob_new - 1;
   
  endrule



  //begin_Mapping

  rule begin_Mapping (!busy);
    
    //{Maybe#(RName), Token, Bool} 
    let tup <- link_mapping.getReq();
    mappingQ.enq(tup);
  endrule

  //continue_Mapping
  
  rule continue_Mapping (!busy &&& mappingQ.first() matches {.mx, .tok, .ss} &&& tok.info.epoch == epoch);
    $display("REGSTATE begin_Mapping: %0d, %0d, %0d", mx, tok, ss);
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
    if (isJust(mx))
      begin
	Vector#(TExp#(rname_SZ), PRName) new_map = maptbl;
	new_map = update(new_map, unJust(mx), newPReg);
	maptbl <= new_map;
      end

    // write rob
    rob_new <= rob_new + 1;
    rob.upd(rob_new, tuple3(tok, mx, newPReg));

    // make snapshot if needed
    Maybe#(SnapshotPtr) midx = Nothing;

  //INDEX sequence
    Bit#(snapshotptr_SZ) ti_bits = truncate(pack(tok));
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
	snap_flreadptrs.upd(idx, freelist.current());
	snap_robnewptrs.upd(idx, rob_new);
                  snaps.upd(idx, maptbl);
      end
    
    freelist.setOldPReg(tok, oldPReg);
    
    // return value
    link_mapping.makeResp(newPReg);
  endrule

  //lookup{1, 2} used by Decode
  rule lookup1 (True);
    
    let rnm <- link_lookup1.getReq();
    $display("REGSTATE Lookup1: %0d", rnm);
    link_lookup1.makeResp(lookup(rnm));
    
  endrule
  
  rule lookup2 (True);
    
    let rnm <- link_lookup2.getReq();
    $display("REGSTATE Lookup2: %0d", rnm);
    link_lookup2.makeResp(lookup(rnm));
    
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
    prf.write(prnm, Valid val);
  endrule
  
  rule write2 (True);
  
    match {.prnm, .val} <- link_write2.receive();
    prf.write(prnm, Valid val);
  
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
    $display("REGSTATE rewindToToken: %0d", tok);
    
    epoch <= epoch + 1;
    
    //NO!!!!!
     
    //see if token is snapshotted
  //INDEX function 
    Maybe#(SnapshotPtr) midx = Nothing;

    Bit#(snapshotptr_SZ) idx_bits = truncate(pack(tok));
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
          maptbl  <=           snaps.sub(i);
          freelist.backTo(snap_flreadptrs.sub(i));
          rob_new <= snap_robnewptrs.sub(i);
        end
      tagged Nothing:   //if not write busy and record token
        begin
          busy <= True;
          stopToken <= tok;
        end
    endcase

    //flatten dead snaps
    match {.oldTok, .*, .*} = rob.sub(rob_old);

    function Bool flatten(Bool x, TokIndex t) = x && (tok.index-oldTok.index > t - oldTok.index); //valid and older than tok stay
     
    snap_valids <= zipWith(flatten, snap_valids, snap_ids);
    
  endrule
    
endmodule

