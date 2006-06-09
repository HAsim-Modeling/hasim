import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import Mem::*;
import FunctionalPartitionBase::*;
import BypassUnit::*;
import Debug::*;

import TOY_Datatypes::*;

`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"

// ToyMIPS is an extremely simple ISA designed to work as a proof of concept.
    

//-------------------------------------------------------------------------//
// Fetch Unit                                                              //
//-------------------------------------------------------------------------//

//mkTOY_Fetch :: IMem Port -> FP_Unit

`define MODULE_NAME "mkTOY_Fetch"
module [HASim_Module] mkTOY_Fetch ();

  FIFO#(Tuple2#(TOY_Token, TOY_Addr)) waitingQ <- mkFIFO();

  //Ports
  
  Connection_Server#(Tuple3#(TOY_Token, void, TOY_Addr), 
                     Tuple3#(TOY_Token, TOY_Inst, Tuple2#(TOY_Addr, TOY_Inst)))
  //... 
  link_fet <- mkConnection_Server("link_fet");
	 
  Connection_Client#(TOY_Addr, TOY_Inst) 
  //...
  link_to_imem <- mkConnection_Client("fet_to_imem");
  
  //handleReq
  
  //Just pass the request on to the IMem

  rule handleFetch (True);
  
    debug_rule("handleFetch");
    
    Tuple3#(TOY_Token, void, TOY_Addr) tup <- link_fet.getReq();
    match {.t, .*, .a} = tup;
    
    link_to_imem.makeReq(a);
    waitingQ.enq(tuple2(t, a));
    
  endrule

  //getMemResp
  
  //Just pass the response back from the IMem

  rule getMemResp (True);
  
    debug_rule("getMemResp");
    
    TOY_Inst resp <- link_to_imem.getResp();
    
    match {.tok, .addr} = waitingQ.first();
    waitingQ.deq();
    
    link_fet.makeResp(tuple3(tok, resp, tuple2(addr, resp)));
  endrule

endmodule
`undef MODULE_NAME


//-------------------------------------------------------------------------//
// Decode Stage                                                            //
//-------------------------------------------------------------------------//

// Also lookup physical register from BypassUnit

// mkTOY_Decode :: BypassUnit -> FP_Unit

`define MODULE_NAME "mkTOY_Decode"
module [HASim_Module] mkTOY_Decode#(BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass) ();

  //Ports
  Connection_Server#(Tuple3#(TOY_Token, Tuple2#(TOY_Addr, TOY_Inst), void), 
                     Tuple3#(TOY_Token, TOY_DepInfo, Tuple2#(TOY_Addr, TOY_DecodedInst))) 
  //...
  link_dec <- mkConnection_Server("link_dec");
  
  FIFO#(Tuple3#(TOY_Token, TOY_DepInfo, Tuple2#(TOY_Addr, TOY_DecodedInst))) 
  //...
  respQ <- mkFIFO();
  
  //handleDecode
  
  //Handles the actual decoding and register allocation
  
  rule handleDecode (True);
  
    debug_rule("handleDecode");
    
    Tuple3#(TOY_Token, Tuple2#(TOY_Addr, TOY_Inst), void) 
    //...
    tup <- link_dec.getReq();
    
    match {.t, {.a, .inst}, .*} = tup;
    
    TOY_DepInfo depinfo = ?;
    TOY_DecodedInst decinst = ?;

    //Get the architectural sources
    match {.ara, .arb} = case (inst) matches
      tagged IAdd {dest: .rd, src1: .ra, src2: .rb}:
          return tuple2(ra, rb);
      tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
          return tuple2(ra, rb);
      tagged IBz {cond: .c , addr:  .addr}:
          return tuple2(c, addr);
      tagged ILoad {dest: .rd, idx: .ri, offset: .off}:
	  return tuple2(ri, ?);
      tagged ILoadImm {dest: .rd, imm: .i}:
          return tuple2(?, ?);
      tagged IStore {src: .rsrc, idx: .ri, offset: .off}:
          return tuple2(rsrc, ri);
      tagged ITerminate:
          return tuple2(?,?);
      endcase;

    //Translate into physical registers
    let pra = bypass.lookup1(ara);
    let prb = bypass.lookup2(arb);
    
    //Actually do the decode
    case (inst) matches
      tagged IAdd {dest: .rd, src1: .ra, src2: .rb}:
        begin
	  debug_case("inst", "IAdd");
	  
          let rtup <- bypass.makeMapping(Just(rd), t, False);
          match {.prd, .oprd} = rtup;
          decinst = DAdd {pdest: prd, opdest: oprd, op1: pra, op2: prb};
          depinfo = TOY_DepInfo {dep_dest: Just(tuple2(rd, prd)), dep_src1: Just(tuple2(ra, pra)), dep_src2: Just(tuple2(rb,prb))};
	  
          debug(2, $display("DEC: [%d]: IAdd R%d := R%d + R%d", t, rd, ra, rb));
        end
      tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
        begin
	  debug_case("inst", "ISub");
	  
          let rtup <- bypass.makeMapping(Just(rd), t, False);
          match {.prd, .oprd} = rtup;
          decinst = DSub {pdest: prd, opdest: oprd, op1: pra, op2: prb};
          depinfo = TOY_DepInfo {dep_dest: Just(tuple2(rd, prd)), dep_src1: Just(tuple2(ra, pra)), dep_src2: Just(tuple2(rb,prb))};

          debug(2, $display("DEC: [%d]: ISub R%d := R%d - R%d", t, rd, ra, rb));
        end
      tagged IBz {cond: .c , addr:  .addr}:
        begin
	  debug_case("inst", "IBz");
	  
          let rtup <- bypass.makeMapping(Nothing, t, True);// likely rewind candidate
          match {.prd, .oprd} = rtup;
          decinst = DBz {opdest: oprd, cond: pra, addr: prb};
          depinfo = TOY_DepInfo {dep_dest: Nothing, dep_src1: Just(tuple2(c,pra)), dep_src2: Just(tuple2(addr,prb))};

          debug(2, $display("DEC: [%d]: IBz (R%d == 0)? pc := (R%d)", t, c, addr));
        end
      tagged ILoad {dest: .rd, idx: .ri, offset: .off}:
        begin
	  debug_case("inst", "ILoad");
	  
          let rtup <- bypass.makeMapping(Just(rd), t, False);
          match {.prd, .oprd} = rtup;
          decinst = DLoad {pdest: prd, opdest: oprd, idx: pra, offset: zeroExtend(off)};
          depinfo = TOY_DepInfo {dep_dest: Just(tuple2(rd,prd)), dep_src1: Just(tuple2(ri,pra)), dep_src2: Nothing};

          debug(2, $display("DEC: [%d]: ILoad R%d := (R%d + %h)", t, rd, ri, off));
        end
      tagged ILoadImm {dest: .rd, imm: .i}:
        begin
	  debug_case("inst", "ILoadImm");
	  
          let rtup <- bypass.makeMapping(Just(rd), t, False);
          match {.prd, .oprd} = rtup;
          decinst = DLoadImm {pdest: prd, opdest: oprd, value: signExtend(i)};
          depinfo = TOY_DepInfo {dep_dest: Just(tuple2(rd,prd)), dep_src1: Nothing, dep_src2: Nothing};

          debug(2, $display("DEC: [%d]: ILoadImm R%d := %d", t, rd, i));
        end
      tagged IStore {src: .rsrc, idx: .ri, offset: .off}:
        begin
	  debug_case("inst", "IStore");
	  
	  let rtup <- bypass.makeMapping(Nothing, t, False);
          match {.prd, .oprd} = rtup;
          decinst = DStore{value: pra, opdest: oprd, idx: prb, offset: zeroExtend(off)};
          depinfo = TOY_DepInfo {dep_dest: Nothing, dep_src1: Just(tuple2(ri,prb)), dep_src2: Just(tuple2(rsrc,pra))};
	  
          debug(2, $display("DEC: [%d]: IStore (R%d + %h) := R%d", t, ri, off, rsrc));
        end
      tagged ITerminate:
        begin
	  debug_case("inst", "ITerminate");
	  
	  let rtup <- bypass.makeMapping(Nothing, t, False); // only to simplify logic
          match {.prd, .oprd} = rtup;
          decinst = DTerminate;
          depinfo = TOY_DepInfo {dep_dest: Nothing, dep_src1: Nothing, dep_src2: Nothing};

          debug(2, $display("DEC: [%d]: ITerminate"));
        end
    endcase
        
    debug(2, $display("DEC: Physical Sources: (PR%d, PR%d)", pra, prb));
    
    link_dec.makeResp(tuple3(t, depinfo, tuple2(a, decinst)));
    
  endrule
  
endmodule
`undef MODULE_NAME  


//-------------------------------------------------------------------------//
// Execute Unit                                                            //
//-------------------------------------------------------------------------//

// Also reads physical register file

// mkTOY_Execute :: BypassUnit -> FP_Unit

`define MODULE_NAME "mkTOY_Execute"
module [HASim_Module] mkTOY_Execute#(BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass) ();
  
  //Ports
  Connection_Server#(Tuple3#(TOY_Token, Tuple2#(TOY_Addr, TOY_DecodedInst), void),
                     Tuple3#(TOY_Token, TOY_InstResult, TOY_ExecedInst)) 
  //...
  link_exe <- mkConnection_Server("link_exe");
  
  //State elements
  FIFO#(Tuple3#(TOY_Token, Tuple2#(TOY_Addr, TOY_DecodedInst), void)) 
  //...
  waitingQ <- mkFIFO();
  
  //handleExec
  
  //We can't always exec right away, since our operands may not be available.
   
  rule handleExec (True);
  
    debug_rule("handleExec");

    let tup <- link_exe.getReq();
    waitingQ.enq(tup);

  endrule
  
  //execute

  rule execute (True);
  
    debug_rule("execute");

    match {.t, {.addr, .dec}, .*} = waitingQ.first();

    TOY_ExecedInst ei = ?;
    Maybe#(TOY_Addr) branchResult = Nothing;
    TOY_PRName va = ?;
    TOY_PRName vb = ?;

    //Get the registers which hold the values
    case (dec) matches
          tagged DAdd {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
            begin
              va = ra;
              vb = rb;
            end
          tagged DSub {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
            begin
              va = ra;
              vb = rb;
            end       
          tagged DBz {opdest: .oprd, cond: .c, addr: .a}:
            begin
              va = c;
              vb = a;
            end
     endcase

     //Try to get the values from the Bypass unit
     Maybe#(TOY_Value) mva = bypass.read1(va);
     Maybe#(TOY_Value) mvb = bypass.read2(vb);


     debug(2, $display("EXE: [%d] read1 PR%d -> %s %d", va,
                        (isJust(mva) ? "Just" : "Nothing"), unJust(mva)));

     debug(2, $display("EXE: [%d] read2 PR%d -> %s %d", vb,
                        (isJust(mvb) ? "Just" : "Nothing"), unJust(mvb)));

     //Actually do the execute
     case (dec) matches
       tagged DAdd {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
       begin
       
	 debug_case("dec", "DAdd");
	 
         if (isJust(mva) && isJust(mvb))
         begin
	 
	   debug_then("isJust(mva) && isJust(mvb)");
	   
	   let result = unJust(mva) + unJust(mvb);
	   
           bypass.write1(prd, result);
           link_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
           waitingQ.deq();

	   debug(2, $display("EXE: [%d] DAdd (old PR%d) PR%d <= 0x%h", t, oprd, prd, result));
	   
         end
       end
       tagged DSub {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
       begin
       
	 debug_case("dec", "DSub");
	 
         if (isJust(mva) && isJust(mvb))
         begin
	 
	   debug_then("isJust(mva) && isJust(mvb)");
	   
	   let result = unJust(mva) - unJust(mvb);
	   
           bypass.write1(prd, result);
           link_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
           waitingQ.deq();
	   
	   debug(2, $display("EXE: [%d] DAdd (old PR%d) PR%d <= 0x%h", t, oprd, prd, result));
	   
         end
       end
       tagged DBz {opdest: .oprd, cond: .c, addr: .a}:
       begin
       
	 debug_case("dec", "DBz");
	 
	 case (mva) matches
	   tagged Valid .cval:
	   begin
	   
	     debug_case("mva", "Valid");
	     
	     if (cval != 0)
	     begin // XXX extra cleverness needed
	     
	       debug_then("cval != 0");
	       
	       link_exe.makeResp(tuple3(t, RBranchNotTaken, ENop {opdest: oprd}));
	       waitingQ.deq();

	       debug(2, $display("EXE: [%d] DBz Not Taken (cval == %d) (old PR%d)", t, cval, oprd));
	       
	     end
	     else   // condition must be zero
	     begin
	     
	       debug_else("cval != 0");
	       
	       case (mvb) matches
		 tagged Valid .dest:
		 begin
		 
		   debug_case("mvb", "Valid");
		   
                   link_exe.makeResp(tuple3(t, RBranchTaken truncate(dest), ENop{opdest: oprd}));
                   waitingQ.deq();
        	 end
		 default:
		   debug_case_default("mvb");
	       endcase
	       
	     end
	   end
	   default:
	     debug_case_default("mva");
	 endcase
       end
       tagged DLoad {pdest: .prd, opdest: .oprd, idx: .idx, offset: .o}: // XXX do offset calc here?
       begin
         
	 debug_case("dec", "DLoad");
	 
         link_exe.makeResp(tuple3(t, RNop, ELoad {idx: idx, offset: o, pdest:prd, opdest: oprd}));
         waitingQ.deq();
	 
	 debug(2, $display("EXE: [%d] DLoad (old PR%d) PR%d := (PR%d + 0x%h)", t, oprd, prd, idx, o));
       end
       tagged DLoadImm {pdest: .prd, opdest: .oprd, value: .val}:
       begin

	 debug_case("dec", "DLoadImm");

         bypass.write1(prd, signExtend(val));
         link_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
         waitingQ.deq();
	 
	 debug(2, $display("EXE: [%d] DLoadImm (old PR%d) PR%d := 0x%h", t, oprd, prd, val));
       end
       tagged DStore {value: .v, opdest: .oprd, idx: .idx, offset: .o}:// XXX do offset calc here?
       begin

	 debug_case("dec", "DLoadImm");

         link_exe.makeResp(tuple3(t, RNop, EStore {idx: idx, offset: o, val: v, opdest: oprd}));
         waitingQ.deq();
	 
	 debug(2, $display("EXE: [%d] DStore (old PR%d) (PR%d + 0x%h) := PR%d", t, oprd, idx, o, v));
       end
       tagged DTerminate:
       begin

	 debug_case("dec", "DTerminate");
	 
         link_exe.makeResp(tuple3(t, RTerminate, ETerminate));
         waitingQ.deq();

	 debug(2, $display("EXE: [%d] DTerminate", t)); 
       end
    endcase
  endrule

endmodule
`undef MODULE_NAME  


//-------------------------------------------------------------------------//
// Memory Unit                                                             //
//-------------------------------------------------------------------------//

// mkTOY_Mem :: BypassUnit -> Memory -> FP_Unit


`define MODULE_NAME "mkTOY_Mem"
module [HASim_Module] mkTOY_Mem#(BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass) ();

  
  //Links
  Connection_Server#(Tuple3#(TOY_Token, TOY_ExecedInst, void),
                     Tuple3#(TOY_Token, void, TOY_ExecedInst)) 
  //...
  link_mem <- mkConnection_Server("link_mem");
	  
  Connection_Client#(MemReq#(TOY_Token, TOY_Addr, TOY_Value), 
                     MemResp#(TOY_Value)) 
  //...
  link_to_dmem <- mkConnection_Client("link_to_dmem");

  
  FIFO#(Tuple2#(TOY_Token, TOY_ExecedInst)) 
  //...
  waitingQ <- mkFIFO();

  //doReq

  rule doReq (True);
       
    debug_rule("doReq");
  
    match {.t, .i, .*} <- link_mem.getReq();

    TOY_PRName va = ?;
    TOY_PRName vb = ?;

    //Get the address info. XXX why doesn't this happen in exe?
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        va = idx;
      tagged EStore{opdest: .oprd, val: .v, idx: .idx, offset: .o}:
        begin
          va = idx;
          vb = v;
        end
    endcase

    
    let mva = bypass.read3(va);
    let mvb = bypass.read4(vb);	 

    debug(2, $display("MEM: read1 PR%d -> %s PR%d", va,
                       isJust(mva) ? "Just" : "Nothing", unJust(mva)));


    debug(2, $display("MEM: read2 PR%d -> %s PR%d", vb,
                       isJust(mvb) ? "Just" : "Nothing", unJust(mvb)));

    waitingQ.enq(tuple2(t, i));
    
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
      begin
        debug_case("i", "ELoad");
       
        if (isJust(mva))
        begin
	  debug_then("isJust(mva)");
	  
	  TOY_Addr a = truncate(unJust(mva)) + zeroExtend(o);
          link_to_dmem.makeReq(Ld {addr: a, token: t});
	  
          debug(2, $display("MEM: [%d] Load Request: 0x%h", t, a));
        end
      end
      tagged EStore{opdest: .oprd, val: .v, idx: .idx, offset: .o}:
      begin
        debug_case("i", "EStore");
	
        let addr = unJust(mva) + zeroExtend(o);
	
        if (isJust(mva) && isJust(mvb))
        begin
	  debug_then("(isJust(mva) && isJust(mvb))");
          link_to_dmem.makeReq(St {val: unJust(mvb), addr: truncate(addr), token: t});
	  
          debug(2, $display("MEM: [%d] Store Request: 0x%h := %d", t, addr, unJust(mvb)));
        end
      end
      default: // push
        debug_case_default("i");
    endcase
  endrule

  //getResp

  rule getResp(True);
       
    debug_rule("getResp");
  
    match {.tok, .i} = waitingQ.first();
    
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        begin
	
	  debug_case("i", "ELoad");
	  
          let resp <- link_to_dmem.getResp();
	  
          TOY_Value v = case (resp) matches
                      tagged LdResp .val: return val;
                      tagged StResp .*  : return 0; // impossible
                    endcase;
		    
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok,?,EWB{pdest: prd, opdest: oprd}));
          bypass.write2(prd, v);
	  
	  
        end
      tagged EStore {opdest: .oprd, val: .*, idx: .*, offset: .*}:
        begin
	
	  debug_case("i", "EStore");
	  
          let resp <- link_to_dmem.getResp();
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok, ?, ENop {opdest: oprd}));
	  
          debug(2, $display("MEM: [%d] StResp", tok));
        end
      default:
        begin
	
	  debug_case_default("i");
	  
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok, ?, i));
	  
          debug(2, $display("MEM: [%d] Non-Memory op", tok));
        end
    endcase
  endrule
  
endmodule
`undef MODULE_NAME  


//-------------------------------------------------------------------------//
// Local Commit Unit                                                       //
//-------------------------------------------------------------------------//

//mkTOY_LocalCommit :: BypassUnit -> FP_Unit

`define MODULE_NAME "mkTOY_LocalCommit"
module [HASim_Module] mkTOY_LocalCommit#(BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass) ();
  
   
  Connection_Server#(Tuple3#(TOY_Token, TOY_ExecedInst, void),
                     Tuple3#(TOY_Token, void, TOY_ExecedInst)) 
  //...
  link_lco <- mkConnection_Server("link_lco");
  
  rule handleLCO (True);
  
    match {.t, .ei, .*} <- link_lco.getReq();
    
    TOY_PRName p = case (ei) matches
                 tagged ENop    .x: return(x.opdest);
		 tagged EWB     .x: return(x.opdest);
		 tagged ELoad   .x: return(x.opdest);
		 tagged EStore  .x: return(x.opdest);
                 tagged ETerminate: return(?);
	       endcase;

    bypass.freePReg(t, p);

    link_lco.makeResp(tuple3(t, ?, ?));

  endrule
  
endmodule
`undef MODULE_NAME  


//-------------------------------------------------------------------------//
// Global Commit Unit                                                      //
//-------------------------------------------------------------------------//

//mkToy_GlobalCommit :: Memory -> FP_Unit

`define MODULE_NAME "mkTOY_GlobalCommit"
module [HASim_Module] mkTOY_GlobalCommit ();

  Connection_Send#(TOY_Token) link_mem_commit <- mkConnection_Send("link_mem_commit");
  
  Connection_Server#(Tuple3#(TOY_Token, TOY_ExecedInst, void),
                     Tuple3#(TOY_Token, void, void)) 
  //...
  link_gco <- mkConnection_Server("link_gco");
  
  rule handleGCO (True);
  
    match {.tok, .*, .*} <- link_gco.getReq();
    
    link_mem_commit.send(tok);
    
    link_gco.makeResp(tuple3(tok, ?, ?));
  
  endrule
  
  
endmodule
`undef MODULE_NAME  

//-------------------------------------------------------------------------//
// Toy Functional Partition                                                //
//-------------------------------------------------------------------------//

module [HASim_Module] mkTOY_TOK_Stage
    //interface:
                (FP_Stage_Con#(TOY_Tick,  //Tick type
		               TOY_Token, //Token type
			       void,	  //Type from previous stage
			       void,	  //Request Type
			       void,	  //Response Type
			       void));    //Type to next stage
		 
  Reg#(TOY_Token) r_first <- mkReg(minBound);
  Reg#(TOY_Token) r_free <- mkReg(minBound);
  
  //Links
  Connection_Server#(Tuple3#(TOY_Token, TOY_Tick, void),
                     Tuple2#(TOY_Token, void))
  //...
  link_from_tp <- mkConnection_Server("tok_server");
  
  Connection_Receive#(Tuple2#(TOY_Token, void))
  //...
  link_from_prev <- mkConnection_Receive("gco_to_tok");
  
  Connection_Send#(Tuple2#(TOY_Token, void))
  //...
  link_to_next <- mkConnection_Send("tok_to_fet");
  
  Connection_Receive#(TOY_Token)
  //...
  link_killToken <- mkConnection_Receive("link_killToken");


  //handleReq
  
  rule handleReq (True);
  
    match {.*, .*, .tick} <- link_from_tp.getReq();

    //allocate a new token
    r_free <= r_free + 1;

    link_from_tp.makeResp(tuple2(r_free, ?));
    link_to_next.send(tuple2(r_free, ?));
    
  
  endrule
  
 
  //recycle
 
  rule recycle (True);

    match {.t, .*} <- link_from_prev.receive();

    //complete token t

    if (r_first != t) 
      $display("TGen ERROR: tokens completing out of order");

    r_first <= r_first + 1;

  endrule
   
  //killToken
  
  rule killToken (True);
    
    let tok <- link_killToken.receive();
    
    //free tok and all tokens after it
    r_free <= tok;
    
  endrule
  
endmodule

//mkTOY_FP :: Memory -> FunctionalPartition

module [HASim_Module] mkTOY_FET_Stage 
    //interface:
                (FP_Stage_Con#(TOY_Tick, 
	                       TOY_Token, 
			       void, 
			       TOY_Addr, 
			       TOY_Inst, 
			       Tuple2#(TOY_Addr, TOY_Inst)));

  let s <- mkFP_Stage_Con("FET", 
                          "link_fet",
			  "fet_server",
			  "tok_to_fet",
			  "fet_to_dec", 
			  8);
  
  return s;

endmodule

module [HASim_Module] mkTOY_DEC_Stage 
    //interface:
                (FP_Stage_Con#(TOY_Tick, 
	                       TOY_Token, 
			       Tuple2#(TOY_Addr, TOY_Inst), 
			       void, 
			       TOY_DepInfo, 
			       Tuple2#(TOY_Addr, TOY_DecodedInst)));

  let s <- mkFP_Stage_Con("DEC", 
                          "link_dec",
			  "dec_server",
			  "fet_to_dec",
			  "dec_to_exe", 
			  8);
  
  return s;

endmodule

module [HASim_Module] mkTOY_EXE_Stage 
    //interface:
                (FP_Stage_Con#(TOY_Tick, 
	                       TOY_Token, 
			       Tuple2#(TOY_Addr, TOY_DecodedInst), 
			       void, 
			       TOY_InstResult, 
			       TOY_ExecedInst));

  let s <- mkFP_Stage_Con("EXE", 
                           "link_exe",
			   "exe_server",
			   "dec_to_exe",
			   "exe_to_mem", 
			   8);
  
  return s;

endmodule

module [HASim_Module] mkTOY_MEM_Stage 
    //interface:
                (FP_Stage_Con#(TOY_Tick, 
	                       TOY_Token, 
			       TOY_ExecedInst, 
			       void, 
			       void, 
			       TOY_ExecedInst));

  let s <- mkFP_Stage_Con("MEM", 
                          "link_mem",
			  "mem_server",
			  "exe_to_mem",
			  "mem_to_lco", 
			  8);

  return s;

endmodule

module [HASim_Module] mkTOY_LCO_Stage 
    //interface:
                (FP_Stage_Con#(TOY_Tick, 
	                       TOY_Token, 
			       TOY_ExecedInst, 
			       void, 
			       void, 
			       TOY_ExecedInst));

  let s <- mkFP_Stage_Con("LCO", 
                          "link_lco",
			  "lco_server",
			  "mem_to_lco",
			  "lco_to_gco", 
			  8);
  
  return s;

endmodule

module [HASim_Module] mkTOY_GCO_Stage 
    //interface:
                (FP_Stage_Con#(TOY_Tick, 
	                       TOY_Token, 
			       TOY_ExecedInst, 
			       void, 
			       void, 
			       void));

  let s <- mkFP_Stage_Con("GCO", 
                          "link_gco",
			  "gco_server",
			  "lco_to_gco",
			  "gco_to_tok", 
			  8);
  
  return s;

endmodule

module [HASim_Module] mkTOY_FP (); 

  BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass <- mkBypassUnit();
  
  Empty fet <- mkTOY_Fetch();
  Empty dec <- mkTOY_Decode(bypass);
  Empty exe <- mkTOY_Execute(bypass);
  Empty mem <- mkTOY_Mem(bypass);
  Empty lco <- mkTOY_LocalCommit(bypass);
  Empty gco <- mkTOY_GlobalCommit();

  
  FP_Stage_Con#(TOY_Tick, TOY_Token,
		void, void, 
		void, void)
  //...
  tok_stage <- mkTOY_TOK_Stage();
		  
  let fet_stage <- mkTOY_FET_Stage();
  let dec_stage <- mkTOY_DEC_Stage();
  let exe_stage <- mkTOY_EXE_Stage();
  let mem_stage <- mkTOY_MEM_Stage();
  let lco_stage <- mkTOY_LCO_Stage();
  FP_Stage_Con#(TOY_Tick, TOY_Token, 
		TOY_ExecedInst, void, 
		void, void) 
  //...
  gco_stage <- mkTOY_GCO_Stage();
  
endmodule
