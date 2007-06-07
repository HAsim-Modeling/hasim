
import Vector::*;
import RegFile::*;

import hasim_common::*;

import hasim_funcp_memstate_ifc::*;
import hasim_funcp_memstate::*;
import hasim_funcp_tokstate::*;

module [HASim_Module] mkFUNCP 
  provisos
          (Bits#(TokIndex, idx_SZ),
	   Bits#(RName, rname_SZ))
  //interface:
              ();

  let mem_state <- mkFUNCP_MemState();
  FUNCP_TokState tok_state <- mkFUNCP_TokState();
  
  //In-Flight instruction info

  BRAM#(TokIndex, Addr)     tok_addr    <- mkBRAM();
  BRAM_3#(TokIndex, Inst)   tok_inst    <- mkBRAM_3();
  BRAM#(TokIndex, TokIndex) tok_writer1 <- mkBRAM();
  BRAM#(TokIndex, TokIndex) tok_writer2 <- mkBRAM();
  BRAM_3#(TokIndex, Value)  tok_value   <- mkBRAM_3();
  
  //Map table
  
  Vector#(TExp#(rname_SZ), PRName) initmap = newVector();
  
  for (Integer x  = 0; x < valueof(TExp#(rname_SZ)); x = x + 1)
  begin
    initmap[x] = fromInteger(x);
  end
  
  Reg#(Vector#(TExp#(rname_SZ), TokIndex)) maptable <- mkReg(initmap);
  Reg#(Vector#(TExp#(rname_SZ), Maybe#(TokIndex))) old_writer <- mkReg(replicate(Invalid));
  
  //Snapshots
  
  Reg#(Vector#(TExp#(snapshotptr_SZ), Bool))  snap_valids        <- mkReg(unpack(0));
  Reg#(Vector#(TExp#(snapshotptr_SZ), TokIndex)) snap_ids        <- mkRegU();

  BRAM#(SnapshotPtr, Vector#(TExp#(rname_SZ), TokIndex))         snaps_maptable   <- mkBRAM_Full();
  BRAM#(SnapshotPtr, Vector#(TExp#(rname_SZ), Maybe#(TokIndex))) snaps_old_writer <- mkBRAM_Full();

  //Misc

  Reg#(TokIndex) num_in_flight <- mkReg(0);
  
  Reg#(Bool)     rewinding <- mkReg#(False);
  Reg#(TokIndex) rewindTok <- mkRegU();

  let can_issue = !num_in_flight[valueOf(idx_SZ)] //Can only have half the tokens in flight at once
  
  Reg#(TokIndex) youngest_tok <- mkReg(0);
  Reg#(TokIndex) oldest_tok <- mkReg(0);
  
  //Links
  Connection_Server#(Bit#(8), Token)
  //...
  link_from_tp <- mkConnection_Server("fp_tok");
  
  Connection_Server#(Tuple3#(Token, void, Addr), 
                     Tuple3#(Token, PackedInst, Tuple2#(Addr, PackedInst)))
  //... 
  link_fet <- mkConnection_Server("fp_fet_stage");

  Connection_Server#(Tuple3#(Token, Tuple2#(Addr, PackedInst), void), 
                     Tuple3#(Token, DepInfo, Tuple2#(Addr, DecodedInst))) 
  //...
  link_dec <- mkConnection_Server("fp_dec_stage");
  
  Connection_Server#(Tuple3#(Token, void, void),
                     Tuple3#(Token, void, ExecedInst)) 
  //...
  link_exe <- mkConnection_Server("fp_exe_stage");
  
  Connection_Server#(Tuple3#(Token, ExecedInst, void),
                     Tuple3#(Token, void, InstWBInfo)) 
  //...
  link_mem <- mkConnection_Server("fp_mem_stage");

  Connection_Server#(Tuple3#(Token, InstWBInfo, void),
                     Tuple3#(Token, void, InstWBInfo)) 
  //...
  link_lco <- mkConnection_Server("fp_lco_stage");
  
  Connection_Server#(Tuple3#(Token, InstWBInfo, void),
                     Tuple3#(Token, void, void)) 
  //...
  link_gco <- mkConnection_Server("fp_gco_stage");
    
  rule newInFlight1 (ready);
   
    let x <- link_from_tp.getReq();

    //But work-around
    if (x == 17)
      tokQ.enq(?);
  
  endrule
  
  rule newInFlight2 (ready && can_issue);

    let stall = tok_state.isBusy(youngest_tok);

    if (!stall)
    begin
    
      tok_state.allocate(youngest_tok);
      
      youngest_tok <= youngest_tok + 1;
      
      num_in_flight <= num_in_flight + 1;
      
      tokQ.deq();
      
      link_from_tp.makeResp(youngest_tok);
      
    end

  endrule
  
  rule fetch1 (ready);
  
    Tuple2#(Token, Addr) tup <- link_fet.getReq();
    match {.t, .a} = tup;
  
    tok_state.fet_start(t);
    
    link_to_imem.makeReq(a);
    
    tok_addr.write(t, a);
    
    fetQ.enq(t);
  
  endrule
  
  rule fetch2 (ready);
  
    PackedInst resp <- link_to_imem.getResp();
    
    let tok = fetQ.first();
    fetQ.deq();
    
    tok_inst.write(tok, resp);
    
    tok_state.fet_finish(tok);
    
    link_fet.makeResp(tuple2(tok, resp));
    
  endrule
  
  rule decode1 (ready);
  
    Tuple2#(Token, void) tup <- link_dec.getReq();
    
    match {.t, .*} = tup;
  
    tok_state.dec_start(t);
    
    tok_inst.read_req(t);
      
    decQ.enq(t);
  
  endrule
  
  rule decode2 (ready);
  
    let tok = deqQ.first();
    decQ.deq();
  
    let inst = tok_inst.read_resp();
    
    let w1 = maptable[getSrc1(inst)];
    let w2 = maptable[getsrc2(inst)];
    
    tok_writer1.write(tok, w1);
    tok_writer2.write(tok, w2);
   
    let newmap = case (getDest(inst)) matches
      tagged Valid d: return update(maptable, tok, d)
      tagged Invalid: return maptable;
    endcase
    
    if (isLoad(inst))
      tok_state.is_a_load(tok);
    
    if (isStore(inst))
      tok_state.is_a_store(tok);
      
    if (isBranch(inst)) //Make a snapshot
    begin
      snap_valids[tok] <= True;
      snap_ids[snap_next] <= tok;
      snap_maptable[snap_next] <= newmap;
      snap_oldwriters[snap_next] <= old_writers;
      snap_next <= snap_next + 1;
    end
    
    tok_state.dec_finish(tok);
    
    link_dec.makeResp(tuple3(tok, w1, w2));
    
  endrule
  
  
  rule execute1 (ready);
  
    match {.tok, .*} <- link_exe.getReq();    
  
    tok_state.exe_start(tok);
    
    tok_writer1.read_req(tok);
    tok_writer2.read_req(tok);
  
    exeQ.enq(tok);
  
  endrule
  
  rule execute2 (ready);
  
    let tok = exeQ.first();
  
    let w1 <- tok_writer1.read_resp();
    let w2 <- tok_writer2.read_resp();
        
    if (tok_state.isReady(w1_st) && tok_state.isReady(w2_st))
    begin
    
      exeQ.deq();
  
      tok_value.read_req1(w1);
      tok_value.read_req2(w2);
      tok_addr.read_req(tok);
      tok_inst.read_req2(tok);
      
      exe2Q.enq(tok);
      
    end
    else
    begin
    
      tok_writer1.read_req(tok);
      tok_writer2.read_req(tok);
      
    end
  
  endrule
  
  rule execute3 (ready);
    
    let tok = exe2Q.first();
    exe2Q.deq();
    
    let v1 <- tok_value.read_resp1();
    let v2 <- tok_value.read_resp2();
    let addr <- tok_addr.read_resp();
    let inst <- tok_inst.read_resp2();
    
    datapath.execute_req(inst, addr, v1, v2);
    
    exe3Q.enq(tok);
    
  endrule
  
  rule execute4 (ready);
    
    let tok = exe3.first();
    exe3.deq();
    
    match {res, wbval} <- datapath.execute_resp();
    
    tok_value.write(wbval);
    tok_state.exeResp.upd(tok, True);
    link_exe.makeResp(tuple2(tok, res));
  
  endrule
  
  rule mem1 (ready);
  
    match {.t, .*} <- link_mem.getReq();
    
    tok_state.mem_start(t);
    
    tok_value.read_req3(t);
    
    memQ.enq(t);
    
  endrule
  
  rule mem2 (ready);
  
    let tok = memQ.first();
    memQ.deq();
  
    let addr <- tok_value.read_resp3();
    
    if (tok_state.isLoad(tok))
    begin
        link_to_dmem.makeReq(Ld {addr: a, token: tok});
	mem2Q.enq(tok);
    end
    else if (tok_state.isStore(tok))
    begin
        link_to_dmem.makeReq(St {val: v, addr: a, token: tok});
	mem2Q.enq(tok);
    end
    else
    begin
      tok_state.mem_finish(tok);
    end
    
  endrule
  
  rule mem3 (ready);
  
    let tok = mem2Q.first();
    mem2Q.deq();
    
    let resp <- link_to_dmem.getResp();
  
    case (resp) matches
      tagged LdResp .val: tok_value.write(tok, val);
      tagged StResp .*  : noAction;
    endcase
    
    tok_state.mem_finish(tok);
    
    link_mem.makeResp(tuple2(tok, ?));
  endrule
  
  rule lco (ready);
  
    match {.t, .*} <- link_lco.getReq();
    
    case (old_writers[t]) matches
      tagged Valid .old_writer:
      begin
    
        tok_state.dealloc(old_writer);
        old_writers[r] <= t;
	num_in_flight <= num_in_flight - 1;
      end
      tagged Invalid:
        noAction;
	
    endcase
    
    tok_state.finalize(t);
    link_lco.makeResp(t);
    
  endrule
  
  rule gco (ready);
  
    match {.t, .inf, .*} <- link_gco.getReq();
    
    link_mem_commit.send(tok);
    
    link_gco.makeResp(t);
    
  endrule
  
  rule rewind1 (ready);
  
    let tok <- link_rewind.receive();
    
    rewinding <= True;
    rewindTok <= tok;
        
    if (snap_valids[tok]) //We have a snapshot
    begin
    
    
      SnapshotPtr idx = snaps_next; //Find youngest snapshot of this token
      for (Integer x = 0; x < valueof(snapshotPTR_SZ); x = x + 1)
      begin
        
        idx = (snap_ids[snaps_next + x] == tok) ? snaps_next + x : idx;
	
      end
            
      snaps_maptable.read_req(idx);
      snaps_old_writer.read_req(idx);
    end
    
  endrule
  
  rule rewind2 (rewinding);
  
      snp_map <- snap_maptable.read_resp();
      snp_old <- snap_old_writers.read_resp();
  
      maptable <= snp_map;
      old_writers <= snp_old;

      youngest_tok <= rewindTok + 1;
      rewinding <= False;
      
  endrule
  
  rule rewind_slow1 (rewinding);
    
    tok_inst.read_req3(youngest_tok);
    youngest_tok <= youngest_tok - 1;
    
    if ((youngest_tok - 1) == rewindTok)
      rewinding <= False;
  
  endrule
  
  rule rewind_slow (True);
  
    let inst = tok_inst.read_resp3();
  
    let r = getDest(inst);
    
    case (old_writer[r]) matches
      tagged Valid .oldw:
      begin
        maptable[r] <= oldwriter;
        old_writers[r] <= Invalid;
      end
      tagged Invalid:
      begin
        $display("ERROR: Funcp Exception: Rewinding maptable to Invalid state.");
	$finish(1);
      end
    endcase
    
  endrule
  
endmodule
