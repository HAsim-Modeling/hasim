
import FIFO::*;
import Vector::*;
import RegFile::*;

import hasim_common::*;
import fpga_components::*;

import hasim_funcp_memstate_ifc::*;
import hasim_funcp_memstate::*;
import hasim_funcp_tokstate::*;
import hasim_funcp_freelist::*;

import hasim_isa::*;
import hasim_isa_datapath::*;


module [HASim_Module] mkFUNCP 
  //interface:
              ()
  provisos
          (Bits#(TokIndex, idx_SZ),
	   Bits#(RName, rname_SZ),
	   Bits#(SnapshotPtr, snapshotptr_SZ));

  //A fake register to hold our debugging file descriptor
  let debug_log <- mkReg(InvalidFile);
  Reg#(Bit#(32)) cc <- mkReg(0);

  function Action funcp_debug(Action a);
  action
  
    $fwrite(debug_log, "[%d]: ", cc);
    a;
    $fwrite(debug_log, "\n");
  
  endaction
  endfunction

  let mem_state <- mkFUNCP_Memstate();
  FUNCP_TokState tok_state <- mkFUNCP_TokState();
  
  //In-Flight instruction info

  BRAM#(TokIndex, Addr)     tok_addr         <- mkBRAM_Full();
  BRAM_2#(TokIndex, Inst)   tok_inst         <- mkBRAM_2_Full();
  BRAM_2#(TokIndex, PRName) tok_dest         <- mkBRAM_2_Full(); //A nice convenience
  BRAM#(TokIndex, Maybe#(PRName)) tok_writer1     <- mkBRAM_Full();
  BRAM#(TokIndex, Maybe#(PRName)) tok_writer2     <- mkBRAM_Full();
  BRAM#(TokIndex, Addr)      tok_memaddr     <- mkBRAM_Full();
  BRAM_3#(PRName, Maybe#(Value))     prf             <- mkBRAM_3_Full();

  //Map table
  
  Vector#(TExp#(rname_SZ), PRName) initmap = newVector();
  
  for (Integer x  = 0; x < valueof(TExp#(rname_SZ)); x = x + 1)
  begin
    initmap[x] = fromInteger(x);
  end
  
  Reg#(Vector#(TExp#(rname_SZ), PRName)) maptable   <- mkReg(initmap);
  
  //Freelist
  
  FreeList freelist <- mkFreeList(debug_log, cc);
  
  //Snapshots
  
  Reg#(Vector#(TExp#(idx_SZ), Bool))             snap_valids     <- mkReg(replicate(False));
  Reg#(Vector#(TExp#(snapshotptr_SZ), TokIndex)) snap_ids        <- mkRegU();
  Reg#(SnapshotPtr)                              snap_next       <- mkReg(0);

  BRAM#(SnapshotPtr, Vector#(TExp#(rname_SZ), PRName))         snaps    <- mkBRAM_Full();
  BRAM#(SnapshotPtr, PRName)                                   snaps_fl <- mkBRAM_Full();

  //Misc

  RName highestReg = maxBound;
  
  Reg#(Bool)     rewinding <- mkReg(False);
  Reg#(Bool)     fast_rewind <- mkReg(False);
  Reg#(Bool)     initializing <- mkReg(True);
  let ready = !rewinding && !initializing;
  Reg#(TokIndex) rewindTok <- mkRegU();
  Reg#(TokIndex) rewindCur <- mkRegU();
  
  //Queues
  
  FIFO#(Token) fetQ <- mkFIFO();
  FIFO#(Token) decQ <- mkFIFO();
  FIFO#(Token) exeQ <- mkFIFO();
  FIFO#(Tuple3#(Token, Maybe#(PRName), Maybe#(PRName))) exe2Q <- mkFIFO();
  FIFO#(Token) exe3Q <- mkFIFO();
  FIFO#(Token) memQ <- mkFIFO();
  FIFO#(Tuple2#(Token, PRName)) mem2Q <- mkFIFO();
  FIFO#(Tuple2#(Token, Addr)) storeQ <- mkFIFO();
  FIFO#(TokIndex) rewindQ <- mkFIFO();
  
  //Datapath
  let datapath <- mkISA_Datapath();
  
  //Links

  Connection_Server#(Bit#(8), Token)
  //...
  link_tok <- mkConnection_Server("fp_tok");
  
  Connection_Server#(Tuple2#(Token, Addr),
                     Tuple2#(Token, Inst))
  //...
  link_fet <- mkConnection_Server("fp_fet");
  
  Connection_Server#(Tuple2#(Token, void),
                     Tuple2#(Token, DepInfo))
  //...
  link_dec <- mkConnection_Server("fp_dec");
  
  Connection_Server#(Tuple2#(Token, void),
                     Tuple2#(Token, InstResult))
  //...
  link_exe <- mkConnection_Server("fp_exe");
  
  Connection_Server#(Tuple2#(Token, void),
                     Tuple2#(Token, void))
  //...
  link_mem <- mkConnection_Server("fp_mem");
  
  Connection_Server#(Tuple2#(Token, void),
                     Tuple2#(Token, void))
  //...
  link_lco <- mkConnection_Server("fp_lco");
  
  Connection_Server#(Tuple2#(Token, void),
                     Tuple2#(Token, void))
  //...
  link_gco <- mkConnection_Server("fp_gco");  
  
  Connection_Receive#(Token)
  //...
  link_rewind <- mkConnection_Receive("fp_rewindToToken");

  Connection_Client#(MemReq, MemResp) 
  //...
  link_to_dmem <- mkConnection_Client("mem_dmem");

  Connection_Client#(Addr, PackedInst) 
  //...
  link_to_imem <- mkConnection_Client("mem_imem");

  Connection_Receive#(Token)
  //...
  link_tok_kill <- mkConnection_Receive("fp_tok_kill");

  Connection_Receive#(Token)
  //...
  link_fet_kill <- mkConnection_Receive("fp_fet_kill");

  Connection_Receive#(Token)
  //...
  link_dec_kill <- mkConnection_Receive("fp_dec_kill");

  Connection_Receive#(Token)
  //...
  link_exe_kill <- mkConnection_Receive("fp_exe_kill");

  Connection_Receive#(Token)
  //...
  link_mem_kill <- mkConnection_Receive("fp_mem_kill");

  Connection_Receive#(Token)
  //...
  link_lco_kill <- mkConnection_Receive("fp_lco_kill");

  Connection_Receive#(Token)
  //...
  link_gco_kill <- mkConnection_Receive("fp_gco_kill");

  Connection_Send#(Token) link_mem_commit <- mkConnection_Send("mem_commit");
  
  Connection_Client#(Tuple4#(Inst, Addr, Value, Value), 
                     Tuple3#(InstResult, Addr, Maybe#(Value))) link_datapath <- mkConnection_Client("isa_datapath");
    
  //Dump obsolete kills
  
  
  rule tok_kills (ready);
    let foo <- link_tok_kill.receive();
  endrule
  
  rule fet_kills (ready);
    let foo <- link_fet_kill.receive();
  endrule
  
  rule dec_kills (ready);
    let foo <- link_dec_kill.receive();
  endrule
  
  rule exe_kills (ready);
    let foo <- link_exe_kill.receive();
  endrule
  
  rule mem_kills (ready);
    let foo <- link_mem_kill.receive();
  endrule
  
  rule lco_kills (ready);
    let foo <- link_lco_kill.receive();
  endrule

  rule gco_kills (ready);
    let foo <- link_gco_kill.receive();
  endrule
  
  //open the debug log
  rule initialize (initializing);
  
    let fd <- $fopen("hasim_funcp.out", "w");

    if (fd == InvalidFile)
    begin
      $display("Error opening FUNCP logfile hasim_funcp.out");
      $finish(1);
    end

    debug_log <= fd;

    initializing <= False;
  
  endrule
  
  rule currentCC (True);
  
    cc <= cc + 1;
  
  endrule
 
  
  (* descending_urgency= "gco, lco, mem3, mem3_store, mem2, mem1, execute4, execute3, execute2, execute1, decode_junk, decode2, decode1, fetch2, fetch1, newInFlight, rewind_slow2, rewind_slow1, rewind_fast, rewind1" *)
 
  rule newInFlight (ready);
   
    let x <- link_tok.getReq();

    //Bug work-around
    if (x == 17)
    begin
      let t <- tok_state.allocate();
      let inf = FUNCP_TokInfo {epoch: 0, scratchpad: 0};
      let newtok = Token {index: t, timep_info: ?, funcp_info: inf};
      link_tok.makeResp(newtok);
      funcp_debug($fwrite(debug_log, "TokGen: Allocating Token %0d", t));
    end

  endrule
  
  
  rule fetch1 (ready);
  
    match {.tok, .addr} <- link_fet.getReq();
    
    funcp_debug($fwrite(debug_log, "Token %0d: Fetch: Start (Address: 0x%h)", tok.index, addr));
  
    tok_state.fet_start(tok.index);
    
    link_to_imem.makeReq(addr);
    
    tok_addr.write(tok.index, addr);
    
    fetQ.enq(tok);
  
  endrule
  
  rule fetch2 (ready);
  
    PackedInst resp <- link_to_imem.getResp();
    
    let inst = bitsToInst(resp);
    
    let tok = fetQ.first();
    fetQ.deq();
    
    tok_inst.write(tok.index, inst);
    
    tok_state.fet_finish(tok.index);
    
    funcp_debug($fwrite(debug_log, "Token %0d: Fetch: End (Inst: 0x%h)", tok.index, resp));
    
    link_fet.makeResp(tuple2(tok, inst));
    
  endrule
  
  rule decode1 (ready);
  
    Tuple2#(Token, void) tup <- link_dec.getReq();
    
    match {.tok, .*} = tup;
    funcp_debug($fwrite(debug_log, "Token %0d: Decode: Start", tok.index));
  
    tok_state.dec_start(tok.index);
    
    tok_inst.read_req1(tok.index);
    freelist.forward_req();        //everyone gets a PR, which makes freeing easier
    
    decQ.enq(tok);
  
  endrule
  
  rule decode2 (ready && tok_state.isAllocated(decQ.first().index));
  
    let tok = decQ.first();
    decQ.deq();
  
    let inst <- tok_inst.read_resp1();
    let pdest <- freelist.forward_resp();
    
    let s1 = getSrc1(inst);
    let s2 = getSrc2(inst);
    
    let w1 = case (s1) matches
        tagged Invalid:   return tagged Invalid;
        tagged Valid .r1: return tagged Valid select(maptable, r1);
      endcase;
    
    let w2 = case (s2) matches
        tagged Invalid: return tagged Invalid;
        tagged Valid .r2: return tagged Valid select(maptable, r2);
      endcase;
    
    let dst = getDest(inst);
        
    tok_writer1.write(tok.index, w1);
    tok_writer2.write(tok.index, w2);
    tok_dest.write(tok.index, pdest);
   
    if (isLoad(inst))
      tok_state.is_a_load(tok.index);
    
    if (isStore(inst))
      tok_state.is_a_store(tok.index);
    
    let newmap = case (dst) matches
	tagged Valid .d: return update(maptable, d, pdest);
	tagged Invalid:  return maptable;
      endcase;
         
    case (dst) matches
      tagged Invalid: noAction;
      tagged Valid .d: 
      begin
	prf.write(pdest, tagged Invalid);
      end
    endcase
    
    let opr = case (dst) matches
        tagged Invalid: pdest; //Free the dummy when you free this token
        tagged Valid .d: select(maptable,d); //Free the old writer
      endcase;
    freelist.setOldPReg(tok, opr);
    
    if (isBranch(inst)) //Make a snapshot
    begin
      funcp_debug($fwrite(debug_log, "Token %0d: Decode: Branch Detected. Making Snapshot (Number %0d).", tok.index, snap_next));
      snap_valids[tok.index] <= True;
      snap_ids[snap_next] <= tok.index;
      snaps.write(snap_next, newmap);
      snaps_fl.write(snap_next, freelist.current());
      snap_next <= snap_next + 1;
    end
    
    maptable <= newmap;
    
    tok_state.dec_finish(tok.index);
 
    let map_d = case (dst) matches
	          tagged Invalid: tagged Invalid;
		  tagged Valid .d: tagged Valid tuple2(d, pdest);
		endcase;
 
    let map_s1 = case (s1) matches
	           tagged Invalid: tagged Invalid;
		   tagged Valid .r1: tagged Valid tuple2(r1, validValue(w1));
		 endcase;

    let map_s2 = case (s2) matches
	           tagged Invalid: tagged Invalid;
		   tagged Valid .r2: tagged Valid tuple2(r2, validValue(w2));
		 endcase;

    let deps = DepInfo 
      { 
      
        dep_dest: map_d,
	dep_src1: map_s1,
	dep_src2: map_s2
      };
    
    case (dst) matches
      tagged Invalid:  funcp_debug($fwrite(debug_log, "Token %0d: Decode: End (No Dest. PR%0d)", tok.index, pdest));
      tagged Valid .d: funcp_debug($fwrite(debug_log, "Token %0d: Decode: End (Dest:R%0d/PR%0d, formerly %0d)", tok.index, d, pdest, select(maptable, d)));
    endcase
    
    case (s1) matches
      tagged Invalid:   funcp_debug($fwrite(debug_log, "Token %0d: Decode: End (No Source 1)", tok.index));
      tagged Valid .r1: funcp_debug($fwrite(debug_log, "Token %0d: Decode: End (Source 1: R%0d/PR%0d)", tok.index, r1, validValue(w1)));
    endcase

    case (s2) matches
      tagged Invalid:   funcp_debug($fwrite(debug_log, "Token %0d: Decode: End (No Source 2)", tok.index));
      tagged Valid .r2: funcp_debug($fwrite(debug_log, "Token %0d: Decode: End (Source 2: R%0d/PR%0d)", tok.index, r2, validValue(w2)));
    endcase

    link_dec.makeResp(tuple2(tok, deps));
    
  endrule
  
  rule decode_junk (ready && !tok_state.isAllocated(decQ.first().index));
  
    let tok = decQ.first();
    decQ.deq();

    let inst <- tok_inst.read_resp1();
    let pdest <- freelist.forward_resp();
    freelist.back();
    
    let s1 = getSrc1(inst);
    let s2 = getSrc2(inst);
    
    let w1 = case (s1) matches
        tagged Invalid:   return tagged Invalid;
        tagged Valid .r1: return tagged Valid select(maptable, r1);
      endcase;
    
    let w2 = case (s2) matches
        tagged Invalid: return tagged Invalid;
        tagged Valid .r2: return tagged Valid select(maptable, r2);
      endcase;
    
    tok_state.dec_finish(tok.index);
    
    let map_s1 = case (s1) matches
	           tagged Invalid: tagged Invalid;
		   tagged Valid .s1: tagged Valid tuple2(s1, validValue(w1));
		 endcase;

    let map_s2 = case (s2) matches
	           tagged Invalid: tagged Invalid;
		   tagged Valid .s2: tagged Valid tuple2(s2, validValue(w2));
		 endcase;

    let deps = DepInfo 
      { 
      
        dep_dest: tagged Invalid,
	dep_src1: map_s1,
	dep_src2: map_s2
      };
      
    funcp_debug($fwrite(debug_log, "Token %0d: Decode: End JUNK (Dest ignored)", tok.index));
    
    case (s1) matches
      tagged Invalid:   funcp_debug($fwrite(debug_log, "Token %0d: Decode: End JUNK (No Source 1)", tok.index));
      tagged Valid .r1: funcp_debug($fwrite(debug_log, "Token %0d: Decode: End JUNK (Source 1: R%0d/PR%0d)", tok.index, r1, validValue(w1)));
    endcase

    case (s2) matches
      tagged Invalid:   funcp_debug($fwrite(debug_log, "Token %0d: Decode: End JUNK (No Source 2)", tok.index));
      tagged Valid .r2: funcp_debug($fwrite(debug_log, "Token %0d: Decode: End JUNK (Source 2: R%0d/PR%0d)", tok.index, r2, validValue(w2)));
    endcase
      
    link_dec.makeResp(tuple2(tok, deps));
    
  endrule
  
  rule execute1 (ready);
  
    match {.tok, .*} <- link_exe.getReq();    
    funcp_debug($fwrite(debug_log, "Token %0d: Execute: Start", tok.index));
  
    tok_state.exe_start(tok.index);
    
    tok_writer1.read_req(tok.index);
    tok_writer2.read_req(tok.index);
  
    exeQ.enq(tok);
  
  endrule
  
  rule execute2 (ready);
  
    let tok = exeQ.first();
    exeQ.deq();
    funcp_debug($fwrite(debug_log, "Token %0d: Execute: Reg Read", tok.index));
    
    let w1 <- tok_writer1.read_resp();
    let w2 <- tok_writer2.read_resp();
    
    //Always do the requests to pretty up the logic
    prf.read_req1(validValue(w1));
    prf.read_req2(validValue(w2));
 
    tok_addr.read_req(tok.index);
    tok_inst.read_req2(tok.index);

    exe2Q.enq(tuple3(tok, w1, w2));
      
  endrule
  
  rule execute3 (ready);
  
    match {.tok, .w1, .w2} = exe2Q.first();

    let v1 <- prf.read_resp1();
    let v2 <- prf.read_resp2();
    let addr <- tok_addr.read_resp();
    let inst <- tok_inst.read_resp2();
    
    let rdy1 = isValid(w1) ? isValid(v1) : True;
    let rdy2 = isValid(w2) ? isValid(v2) : True;
    
    let isJunk = !tok_state.isAllocated(tok.index);
    
    if ((rdy1 && rdy2) || isJunk) //let junk proceed
    begin
      exe2Q.deq();

      if (!isJunk)
        funcp_debug($fwrite(debug_log, "Token %0d: Execute: Sending to Datapath (V1:0x%h, V2:0x%h)", tok.index, validValue(v1), validValue(v2)));
      else
        funcp_debug($fwrite(debug_log, "Token %0d: Execute: Sending to Datapath JUNK (V1:0x%h, V2:0x%h)", tok.index, validValue(v1), validValue(v2)));
	
      link_datapath.makeReq(tuple4(inst, addr, validValue(v1), validValue(v2)));
      tok_dest.read_req1(tok.index);
      exe3Q.enq(tok);
    end
    else
    begin
    
      if (!rdy1 && !rdy2)
        funcp_debug($fwrite(debug_log, "Token %0d: Execute: Stalling on both sources.", tok.index));  
      else if (!rdy1)
        funcp_debug($fwrite(debug_log, "Token %0d: Execute: Stalling on source 1.", tok.index));  
      else
        funcp_debug($fwrite(debug_log, "Token %0d: Execute: Stalling on source 2.", tok.index));    
      
      prf.read_req1(validValue(w1));
      prf.read_req2(validValue(w2));
      tok_addr.read_req(tok.index);
      tok_inst.read_req2(tok.index);
      
    end
    
  endrule
  
  
  rule execute4 (ready);
    
    let tok = exe3Q.first();
    exe3Q.deq();
    
    match {.res, .eaddr, .wbval} <- link_datapath.getResp();
    let dst <- tok_dest.read_resp1();
    
    prf.write(dst, wbval);
    
    tok_memaddr.write(tok.index, eaddr);
    
    tok_state.exe_finish(tok.index);
    
    if (isValid(wbval))
      funcp_debug($fwrite(debug_log, "Token %0d: Execute: Finish (PR%0d <= 0x%h).", tok.index, dst, validValue(wbval)));
    else
      funcp_debug($fwrite(debug_log, "Token %0d: Execute: Finish (No writeback).", tok.index));
    
    link_exe.makeResp(tuple2(tok, res));
  
  endrule
  
  rule mem1 (ready);
  
    match {.tok, .*} <- link_mem.getReq();
    
    funcp_debug($fwrite(debug_log, "Token %0d: DMem: Start", tok.index)); 
    
    tok_state.mem_start(tok.index);
    
    tok_memaddr.read_req(tok.index);
    tok_dest.read_req2(tok.index);
    
    memQ.enq(tok);
    
  endrule
  
  rule mem2 (ready);
  
    let tok = memQ.first();
    memQ.deq();
  
    let addr <- tok_memaddr.read_resp();
    let dst <- tok_dest.read_resp2();
    
    if (tok_state.isLoad(tok.index))
    begin
        funcp_debug($fwrite(debug_log, "Token %0d: DMem: Requesting Load (Addr: 0x%h)", tok.index, addr)); 
        link_to_dmem.makeReq(Ld {addr: addr, token: tok});
	mem2Q.enq(tuple2(tok, dst));
    end
    else if (tok_state.isStore(tok.index))
    begin
        funcp_debug($fwrite(debug_log, "Token %0d: DMem: Retreiving Store Value (PR%0d)", tok.index, dst)); 
        prf.read_req3(dst);
	storeQ.enq(tuple2(tok, addr));
    end
    else
    begin
      funcp_debug($fwrite(debug_log, "Token %0d: DMem: Finish (trivial)", tok.index)); 
      tok_state.mem_finish(tok.index);
      link_mem.makeResp(tuple2(tok, ?));
    end
    
  endrule
  
  rule mem3_store (ready);
  
    match {.tok, .addr} = storeQ.first();
    storeQ.deq();
    let mval <- prf.read_resp3();
    
    case (mval) matches
      tagged Invalid:
      begin
        $display("FUNCP: ERROR: Doing store for Token %0d which has not been executed.", tok.index);
	$finish(1);
      end
      tagged Valid .val:
      begin
        funcp_debug($fwrite(debug_log, "Token %0d: DMem: Requesting Store (Addr: 0x%h <= 0x%h)", tok.index, addr, val)); 
        link_to_dmem.makeReq(St {val: val, addr: addr, token: tok});
      end
    endcase
  
    mem2Q.enq(tuple2(tok, ?));
    
  endrule
  
  rule mem3 (ready);
  
    match {.tok, .dest} = mem2Q.first();
    mem2Q.deq();
    
    let resp <- link_to_dmem.getResp();
  
    case (resp) matches
      tagged StResp .*  : noAction;
      tagged LdResp .val:
      begin
        funcp_debug($fwrite(debug_log, "Token %0d: DMem: Load Response (PR%0d <= 0x%h", tok.index, dest, val)); 
        prf.write(dest, tagged Valid val);
      end
    endcase
    
    tok_state.mem_finish(tok.index);
    
    funcp_debug($fwrite(debug_log, "Token %0d: DMem: Finish", tok.index)); 
    
    link_mem.makeResp(tuple2(tok, ?));
  endrule
  
  rule lco (ready);
  
    match {.tok, .*} <- link_lco.getReq();
    
    freelist.free(tok);

    funcp_debug($fwrite(debug_log, "Token %0d: LCO: Finishing Token", tok.index)); 

    link_lco.makeResp(tuple2(tok, ?));
    tok_state.deallocate(tok.index);
	
  endrule

  
  rule gco (ready);
  
    match {.tok, .*} <- link_gco.getReq();
    
    funcp_debug($fwrite(debug_log, "Token %d: GCO: Finishing Token", tok.index)); 

    if (tok_state.isStore(tok.index))
    begin
      link_mem_commit.send(tok);
    end
    
    link_gco.makeResp(tuple2(tok, ?));
    
  endrule
  
  rule rewind1 (ready);
  
    let tok <- link_rewind.receive();
    
    funcp_debug($fwrite(debug_log, "Rewind: Starting Rewind to Token %0d (Youngest: %0d)", tok.index, tok_state.youngest())); 
    
    tok_state.rewindTo(tok.index);
    
    Bool found = False;
    if (snap_valids[tok.index]) //We might have a snapshot
    begin
    
      funcp_debug($fwrite(debug_log, "Potential Fast Rewind"));  
      SnapshotPtr idx = snap_next; //Find youngest snapshot of this token
      for (Integer x = 0; x < valueof(TExp#(snapshotptr_SZ)); x = x + 1)
      begin
	
	let cur = snap_next + fromInteger(x);
	match {.new_idx, .new_found} = (snap_ids[cur] == tok.index) ? tuple2(cur, True) : tuple2(idx, found);
        found = new_found;
	idx = new_idx;
	
      end
      
      if (found)
      begin   
        funcp_debug($fwrite(debug_log, "Fast Rewind confirmed with Snapshot %0d", idx));  
        snaps.read_req(idx);
	snaps_fl.read_req(idx);
      end
      
    end
    
    if (!found)
    begin
    
        funcp_debug($fwrite(debug_log, "Initiating slow Rewind (Oldest: %0d)", tok_state.oldest()));  
    end
    
    rewinding <= True;
    fast_rewind <= found;
    rewindTok <= tok_state.youngest();
    rewindCur <= tok_state.oldest(); //Start at the oldest and go forward
    
  endrule
  
  rule rewind_fast (rewinding && fast_rewind);
  
      let snp_map <- snaps.read_resp();
      let snp_fl  <- snaps_fl.read_resp();
  
      maptable <= snp_map;
      freelist.backTo(snp_fl);

      rewinding <= False;
      funcp_debug($fwrite(debug_log, "Fast Rewind finished."));  
      
  endrule
  
  //Slow rewind. Walk the tokens in age order
  //and reconstruct the maptable
  
  rule rewind_slow1 (rewinding && !fast_rewind);
    
    if (tok_state.isAllocated(rewindCur)) //Don't remap killed tokens
    begin
        funcp_debug($fwrite(debug_log, "Slow Rewind: Lookup up Token %0d", rewindCur));  
        tok_dest.read_req2(rewindCur);
        tok_inst.read_req2(rewindCur);
	rewindQ.enq(rewindCur);
    end
    
    rewindCur <= rewindCur + 1;
    
    if (rewindCur == rewindTok) //Must take into account the last instruction
    begin
      rewinding <= False;
      funcp_debug($fwrite(debug_log, "Slow Rewind: No more tokens to lookup."));  
    end
  
  endrule

  rule rewind_slow2 (True);
  
    let t = rewindQ.first();
    rewindQ.deq();
    
     
    freelist.back();
    let dst <- tok_dest.read_resp2();
    let inst <- tok_inst.read_resp2();
    
    case (getDest(inst)) matches
      tagged Invalid: funcp_debug($fwrite(debug_log, "Slow Rewind: Token %0d had no dest", t));
      tagged Valid .d:
      begin
          funcp_debug($fwrite(debug_log, "Slow Rewind: Token %0d: Remapping (R%0d/PR%0d)", t, d, dst));
	  maptable[d] <= dst;
      end
    endcase
    
  endrule
 
endmodule
