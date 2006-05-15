import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import Ports::*;
import FunctionalPartition::*;
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
module [Module] mkTOY_Fetch#(Port_Client#(TOY_Addr, TOY_Inst) port_to_imem) 

       //interface:
                   (FP_Unit#(TOY_Token,                     // token type
		             void,                          // type from prev stage
			     TOY_Addr,                      // request type
			     TOY_Inst,                      // response type
			     Tuple2#(TOY_Addr, TOY_Inst))); // type to next stage
  //State elements
  FIFO#(Tuple2#(TOY_Token, TOY_Addr)) waitingQ <- mkFIFO();

  //Ports
  let port_fet <- mkPort_Server("port_fet");

  //handleReq
  
  //Just pass the request on to the IMem

  rule handleFetch (True);
  
    debug_rule("handleFetch");
    
    Tuple3#(TOY_Token, void, TOY_Addr) tup <- port_fet.getReq();
    match {.t, .*, .a} = tup;
    
    port_to_imem.makeReq(a);
    waitingQ.enq(tuple2(t, a));
    
  endrule

  //getMemResp
  
  //Just pass the response back from the IMem

  rule getMemResp (True);
  
    debug_rule("getMemResp");
    
    TOY_Inst resp <- port_to_imem.getResp();
    
    match {.tok, .addr} = waitingQ.first();
    waitingQ.deq();
    
    port_fet.makeResp(tuple3(tok, resp, tuple2(addr, resp)));
  endrule

  //Interface to FPStage
  return port_fet.server;

endmodule
`undef MODULE_NAME


//-------------------------------------------------------------------------//
// Decode Stage                                                            //
//-------------------------------------------------------------------------//

// Also lookup physical register from BypassUnit

// mkTOY_Decode :: BypassUnit -> FP_Unit

`define MODULE_NAME "mkTOY_Decode"
module [Module] mkTOY_Decode#(BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass)
     //interface:
                (FP_Unit#(TOY_Token,                            //token type
		          Tuple2#(TOY_Addr, TOY_Inst),          //type from prev stage (fetch)
			  void,                                 //request type
			  TOY_DepInfo,                          //response type
			  Tuple2#(TOY_Addr, TOY_DecodedInst))); //type to next stage (exec)
  
  //Ports
  let port_dec <- mkPort_Server("port_dec");
  
  FIFO#(Tuple3#(TOY_Token, TOY_DepInfo, Tuple2#(TOY_Addr, TOY_DecodedInst))) respQ <- mkFIFO();
  
  //handleDecode
  
  //Handles the actual decoding and register allocation
  
  rule handleDecode (True);
  
    debug_rule("handleDecode");
    
    Tuple3#(TOY_Token, Tuple2#(TOY_Addr, TOY_Inst), void) tup <- port_dec.getReq();
    
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
    
    port_dec.makeResp(tuple3(t, depinfo, tuple2(a, decinst)));
    
  endrule
  
  //Interface to FP_Stage
  return port_dec.server;
  
endmodule
`undef MODULE_NAME  


//-------------------------------------------------------------------------//
// Execute Unit                                                            //
//-------------------------------------------------------------------------//

// Also reads physical register file

// mkTOY_Execute :: BypassUnit -> FP_Unit

`define MODULE_NAME "mkTOY_Execute"
module [Module] mkTOY_Execute#(BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass)
    //interface:
                (FP_Unit#(TOY_Token,                          //token type
		       Tuple2#(TOY_Addr, TOY_DecodedInst), //type from prev stage (decode)
		       void,                               //request type
		       TOY_InstResult,                     //response type
		       TOY_ExecedInst));                   //type to next stage (mem)

  //Ports
  let port_exe <- mkPort_Server("port_exe");
  
  //State elements
  FIFO#(Tuple3#(TOY_Token, Tuple2#(TOY_Addr, TOY_DecodedInst), void)) waitingQ <- mkFIFO();
  
  //handleExec
  
  //We can't always exec right away, since our operands may not be available.
   
  rule handleExec (True);
  
    debug_rule("handleExec");

    let tup <- port_exe.getReq();
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
           port_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
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
           port_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
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
	       
	       port_exe.makeResp(tuple3(t, RBranchNotTaken, ENop {opdest: oprd}));
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
		   
                   port_exe.makeResp(tuple3(t, RBranchTaken truncate(dest), ENop{opdest: oprd}));
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
	 
         port_exe.makeResp(tuple3(t, RNop, ELoad {idx: idx, offset: o, pdest:prd, opdest: oprd}));
         waitingQ.deq();
	 
	 debug(2, $display("EXE: [%d] DLoad (old PR%d) PR%d := (PR%d + 0x%h)", t, oprd, prd, idx, o));
       end
       tagged DLoadImm {pdest: .prd, opdest: .oprd, value: .val}:
       begin

	 debug_case("dec", "DLoadImm");

         bypass.write1(prd, signExtend(val));
         port_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
         waitingQ.deq();
	 
	 debug(2, $display("EXE: [%d] DLoadImm (old PR%d) PR%d := 0x%h", t, oprd, prd, val));
       end
       tagged DStore {value: .v, opdest: .oprd, idx: .idx, offset: .o}:// XXX do offset calc here?
       begin

	 debug_case("dec", "DLoadImm");

         port_exe.makeResp(tuple3(t, RNop, EStore {idx: idx, offset: o, val: v, opdest: oprd}));
         waitingQ.deq();
	 
	 debug(2, $display("EXE: [%d] DStore (old PR%d) (PR%d + 0x%h) := PR%d", t, oprd, idx, o, v));
       end
       tagged DTerminate:
       begin

	 debug_case("dec", "DTerminate");
	 
         port_exe.makeResp(tuple3(t, RTerminate, ETerminate));
         waitingQ.deq();

	 debug(2, $display("EXE: [%d] DTerminate", t)); 
       end
    endcase
  endrule

  //Interface to FP_Stage
  return port_exe.server;
  
endmodule
`undef MODULE_NAME  


//-------------------------------------------------------------------------//
// Memory Unit                                                             //
//-------------------------------------------------------------------------//

// mkTOY_Mem :: BypassUnit -> Memory -> FP_Unit


`define MODULE_NAME "mkTOY_Mem"
module [Module] mkTOY_Mem#(BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass,
	                   Port_Client#(MemReq#(TOY_Addr, TOY_Token, TOY_Value), MemResp#(TOY_Value)) port_to_dmem)
    //interface:
	        (FP_Unit#(TOY_Token,        //token type
		          TOY_ExecedInst,   //type from prev stage (exec)
			  void,             //request type
			  void,             //response type
			  TOY_ExecedInst)); //type to next stage (lcommit)

  
  let port_mem <- mkPort_Server("port_mem");
  
  FIFO#(Tuple2#(TOY_Token, TOY_ExecedInst)) waitingQ <- mkFIFO();

  //doReq

  rule doReq (True);
       
    debug_rule("doReq");
  
    match {.t, .i, .*} <- port_mem.getReq();

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
          port_to_dmem.makeReq(Ld {addr: a, token: t});
	  
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
          port_to_dmem.makeReq(St {val: unJust(mvb), addr: truncate(addr), token: t});
	  
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
	  
          let resp <- port_to_dmem.getResp();
	  
          TOY_Value v = case (resp) matches
                      tagged LdResp .val: return val;
                      tagged StResp .*  : return 0; // impossible
                    endcase;
		    
          waitingQ.deq();
          port_mem.makeResp(tuple3(tok,?,EWB{pdest: prd, opdest: oprd}));
          bypass.write2(prd, v);
	  
	  
        end
      tagged EStore {opdest: .oprd, val: .*, idx: .*, offset: .*}:
        begin
	
	  debug_case("i", "EStore");
	  
          let resp <- port_to_dmem.getResp();
          waitingQ.deq();
          port_mem.makeResp(tuple3(tok, ?, ENop {opdest: oprd}));
	  
          debug(2, $display("MEM: [%d] StResp", tok));
        end
      default:
        begin
	
	  debug_case_default("i");
	  
          waitingQ.deq();
          port_mem.makeResp(tuple3(tok, ?, i));
	  
          debug(2, $display("MEM: [%d] Non-Memory op", tok));
        end
    endcase
  endrule

  //Interface to FP_Stage
  return port_mem.server;
  
endmodule
`undef MODULE_NAME  


//-------------------------------------------------------------------------//
// Local Commit Unit                                                       //
//-------------------------------------------------------------------------//

//mkTOY_LocalCommit :: BypassUnit -> FP_Unit

`define MODULE_NAME "mkTOY_LocalCommit"
module [Module] mkTOY_LocalCommit#(BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass)
    //interface:
                (FP_Unit#(TOY_Token,      //token type
		          TOY_ExecedInst, //type from prev stage (mem)
			  void,           //request type
			  void,           //response type
			  void));         //type to next stage (gcommit)
  
  let port_lco <- mkPort_Server("port_lco");
  
  rule handleLCO (True);
  
    match {.t, .ei, .*} <- port_lco.getReq();
    
    TOY_PRName p = case (ei) matches
                 tagged ENop    .x: return(x.opdest);
		 tagged EWB     .x: return(x.opdest);
		 tagged ELoad   .x: return(x.opdest);
		 tagged EStore  .x: return(x.opdest);
                 tagged ETerminate: return(?);
	       endcase;

    bypass.freePReg(t, p);

    port_lco.makeResp(tuple3(t, ?, ?));

  endrule
  
  //Interface to FP_Stage
  return port_lco.server;
  
endmodule
`undef MODULE_NAME  


//-------------------------------------------------------------------------//
// Global Commit Unit                                                      //
//-------------------------------------------------------------------------//

//mkToy_GlobalCommit :: Memory -> FP_Unit

`define MODULE_NAME "mkTOY_GlobalCommit"
module [Module] mkTOY_GlobalCommit#(Port_Send#(TOY_Token) port_to_mem_commit)
    //interface:
                (FP_Unit#(TOY_Token, //token type
		          void,      //type from prev stage (lcommit)
			  void,      //request type
			  void,      //response type
			  void));    //type for next stage (tokgen)

  
  let port_gco <- mkPort_Server("port_gco");
  
  rule handleGCO (True);
  
    match {.tok, .*, .*} <- port_gco.getReq();
    
    port_to_mem_commit.send(tok);
    
    port_gco.makeResp(tuple3(tok, ?, ?));
  
  endrule
  
  //Interface to FP_Stage
  return port_gco.server;
  
endmodule
`undef MODULE_NAME  

//-------------------------------------------------------------------------//
// Toy Functional Partition                                                //
//-------------------------------------------------------------------------//

//mkTOY_FP :: Memory -> FunctionalPartition

(* synthesize *)
module [Module] mkTOY_FP
    //interface:						       
  		(FunctionalPartition#(TOY_Tick,             //tick type
		                      TOY_Token,            //token type
		                      TOY_Addr,             //address type
				      TOY_Inst,             //instruction type
				      TOY_Value,            //value type
		                      void, void,           //tokenReq, tokenResp,
  				      TOY_Addr, TOY_Inst,   //fetchReq, fetchResp
  				      void, TOY_DepInfo,    //decodeReq, decodeResp
  				      void, TOY_InstResult, //execReq, execResp
  				      void, void,           //memReq, memResp
  				      void, void,           //lcommitReq, lcommitResp
  				      void, void));         //gcommitReq, gcommitResp  

  let bypass <- mkBypassUnit();
  
  let port_to_imem           <- mkPort_Client("mem_imem");
  let port_to_dmem           <- mkPort_Client("mem_dmem");
  let port_to_mem_commit     <- mkPort_Send("mem_commit");
  let port_to_mem_killRange  <- mkPort_Send("mem_killRange");
  
  let tok <- mkFP_TokGen();  //TokGen is from library
  let fet <- mkTOY_Fetch(port_to_imem);
  let dec <- mkTOY_Decode(bypass);
  let exe <- mkTOY_Execute(bypass);
  let mem <- mkTOY_Mem(bypass, port_to_dmem);
  let lco <- mkTOY_LocalCommit(bypass);
  let gco <- mkTOY_GlobalCommit(port_to_mem_commit);
  
  let fp <- mkFunctionalPartition(tok, fet, 
                                  dec, exe, 
				  mem, lco, 
				  gco, 
				  bypass, 
				  port_to_imem,
				  port_to_dmem,
				  port_to_mem_commit,
				  port_to_mem_killRange,
				  8);

  return fp;

endmodule
