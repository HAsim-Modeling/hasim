import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import Ports::*;
import TOY_Datatypes::*;
import FunctionalPartition::*;
import ValueVector::*;
import Mem::*;
import BypassUnit::*;
import BypassFIFO::*;

// ToyMIPS is an extremely simple ISA designed to work as a proof of concept.
    

//-------------------------------------------------------------------------//
// Fetch Unit                                                              //
//-------------------------------------------------------------------------//

//mkTOY_Fetch :: IMem Port -> FP_Unit

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

  rule handleReq (True);
    
    Tuple3#(TOY_Token, void, TOY_Addr) tup <- port_fet.getReq();
    match {.t, .*, .a} = tup;
    
    port_to_imem.makeReq(a);
    waitingQ.enq(tuple2(t,a));
    
  endrule

  //getMemResp
  
  //Just pass the response back from the IMem

  rule getMemResp (True);
  
    TOY_Inst resp <- port_to_imem.getResp();
    
    match {.tok, .addr} = waitingQ.first();
    waitingQ.deq();
    
    port_fet.makeResp(tuple3(tok, resp, tuple2(addr, resp)));
  endrule

  //Interface to FPStage
  return port_fet.server;

endmodule  


//-------------------------------------------------------------------------//
// Decode Stage                                                            //
//-------------------------------------------------------------------------//

// Also lookup physical register from BypassUnit

// mkTOY_Decode :: BypassUnit -> FP_Unit

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
      tagged ILoad {dest: .rd, idx: .ri, offset: .ro}:
	  return tuple2(ri, ?);
      tagged ILoadImm {dest: .rd, imm: .i}:
          return tuple2(?, ?);
      tagged IStore {src: .rsrc, idx: .ri, offset: .ro}:
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
          let rtup <- bypass.makeMapping(Just(rd), t, False);
          match {.prd, .oprd} = rtup;
          decinst = DAdd {pdest: prd, opdest: oprd, op1: pra, op2: prb};
          depinfo = TOY_DepInfo {dep_dest: Just(tuple2(rd, prd)), dep_src1: Just(tuple2(ra, pra)), dep_src2: Just(tuple2(rb,prb))};
        end
      tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
        begin
          let rtup <- bypass.makeMapping(Just(rd), t, False);
          match {.prd, .oprd} = rtup;
          decinst = DSub {pdest: prd, opdest: oprd, op1: pra, op2: prb};
          depinfo = TOY_DepInfo {dep_dest: Just(tuple2(rd, prd)), dep_src1: Just(tuple2(ra, pra)), dep_src2: Just(tuple2(rb,prb))};
        end
      tagged IBz {cond: .c , addr:  .addr}:
        begin
          let rtup <- bypass.makeMapping(Nothing, t, True);// likely rewind candidate
          match {.prd, .oprd} = rtup;
          decinst = DBz {opdest: oprd, cond: pra, addr: prb};
          depinfo = TOY_DepInfo {dep_dest: Nothing, dep_src1: Just(tuple2(c,pra)), dep_src2: Just(tuple2(addr,prb))};
        end
      tagged ILoad {dest: .rd, idx: .ri, offset: .ro}:
        begin
          let rtup <- bypass.makeMapping(Just(rd), t, False);
          match {.prd, .oprd} = rtup;
          decinst = DLoad{pdest: prd, opdest: oprd, idx: pra, offset: zeroExtend(ro)};
          depinfo = TOY_DepInfo {dep_dest: Just(tuple2(rd,prd)), dep_src1: Just(tuple2(ri,pra)), dep_src2: Nothing};
        end
      tagged ILoadImm {dest: .rd, imm: .i}:
        begin
          let rtup <- bypass.makeMapping(Just(rd), t, False);
          match {.prd, .oprd} = rtup;
          decinst = DLoadImm {pdest: prd, opdest: oprd, value: signExtend(i)};
          depinfo = TOY_DepInfo {dep_dest: Just(tuple2(rd,prd)), dep_src1: Nothing, dep_src2: Nothing};
        end
      tagged IStore {src: .rsrc, idx: .ri, offset: .ro}:
        begin
	  let rtup <- bypass.makeMapping(Nothing, t, False);
          match {.prd, .oprd} = rtup;
          decinst = DStore{value: pra, opdest: oprd, idx: prb, offset: zeroExtend(ro)};
          depinfo = TOY_DepInfo {dep_dest: Nothing, dep_src1: Just(tuple2(ri,prb)), dep_src2: Just(tuple2(rsrc,pra))};
        end
      tagged ITerminate:
        begin
	  let rtup <- bypass.makeMapping(Nothing, t, False); // only to simplify logic
          match {.prd, .oprd} = rtup;
          decinst = DTerminate;
          depinfo = TOY_DepInfo {dep_dest: Nothing, dep_src1: Nothing, dep_src2: Nothing};
        end
    endcase

    $display("Decode Packed Instruction: %h", pack(inst));
    $display("Decode Read1[%d] -> [%d]", ara, pra);

    $display("Decode Read2[%d] -> [%d]", arb, prb);

    
    port_dec.makeResp(tuple3(t, depinfo, tuple2(a, decinst)));
    
  endrule
  
  //Interface to FP_Stage
  return port_dec.server;
  
endmodule  


//-------------------------------------------------------------------------//
// Execute Unit                                                            //
//-------------------------------------------------------------------------//

// Also reads physical register file

// mkTOY_Execute :: BypassUnit -> FP_Unit

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
  
    let tup <- port_exe.getReq();
    waitingQ.enq(tup);
    
  endrule
  
  //execute

  rule execute (True);
   
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

 
    $display("Execute Read1[%d] -> %s[%d]", va,
                                            (isJust(mva) ? "Just":"Nothing"),
                                            unJust(mva));

    $display("Execute Read2[%d] -> %s[%d]", vb,
                                            (isJust(mvb) ? "Just":"Nothing"),
                                            unJust(mvb));

    //Actually do the execute
    case (dec) matches
         tagged DAdd {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
             if (isJust(mva) && isJust(mvb))
               begin
                 $display("Executing Add %d: (old %d)[%d] <= 0x%h", t, oprd, prd, unJust(mva) + unJust(mvb));
                 bypass.write1(prd,unJust(mva) + unJust(mvb));
                 port_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
                 waitingQ.deq();
               end
         tagged DSub {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
             if (isJust(mva) && isJust(mvb))
               begin
                 $display("Executing Sub %d: (old %d)[%d] <= 0x%h", t, oprd, prd, unJust(mva) - unJust(mvb));
                 bypass.write1(prd,unJust(mva) - unJust(mvb));
                 port_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
                 waitingQ.deq();
               end
         tagged DBz {opdest: .oprd, cond: .c, addr: .a}:
	   case (mva) matches
	     tagged Valid .cval:
	       if (cval != 0)
	       begin // XXX extra cleverness needed
		 port_exe.makeResp(tuple3(t, RBranchNotTaken , ENop {opdest: oprd}));
		 waitingQ.deq();
	       end
	       else case (mvb) matches // condition must be zero
	         tagged Valid .dest:
		 begin
                   port_exe.makeResp(tuple3(t, RBranchTaken truncate(dest), ENop{opdest: oprd}));
                   waitingQ.deq();
                 end
	         default:
		   noAction;
		 endcase
	     default:
	       noAction;
	   endcase
         tagged DLoad {pdest: .prd, opdest: .oprd, idx: .idx, offset: .o}: // XXX do offset calc
           begin
             $display("Executing Load %d: (old %d)[%d]", t, oprd, prd);
             port_exe.makeResp(tuple3(t, RNop, ELoad {idx: idx, offset: o, pdest:prd, opdest: oprd}));
             waitingQ.deq();
           end
         tagged DLoadImm {pdest: .prd, opdest: .oprd, value: .val}:
           begin
             bypass.write1(prd, signExtend(val));
             port_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
             waitingQ.deq();
           end
         tagged DStore {value: .v, opdest: .oprd, idx: .idx, offset: .o}:// XXX do offset calc
           begin
             $display("Executing Store %d: (old %d)", t, oprd);
             port_exe.makeResp(tuple3(t, RNop, EStore {idx: idx, offset: o, val: v, opdest: oprd}));
             waitingQ.deq();
           end
         tagged DTerminate:
           begin
             $display("Executing Terminate %d: ", t);
             port_exe.makeResp(tuple3(t, RTerminate, ETerminate));
             waitingQ.deq();
           end
    endcase
  endrule

  //Interface to FP_Stage
  return port_exe.server;
  
endmodule  


//-------------------------------------------------------------------------//
// Memory Unit                                                             //
//-------------------------------------------------------------------------//

// mkTOY_Mem :: BypassUnit -> Memory -> FP_Unit

module [Module] mkTOY_Mem#(BypassUnit#(TOY_RName, TOY_PRName, TOY_Value, TOY_Token, TOY_SnapshotPtr) bypass,
	                   Port_Client#(MemReq#(TOY_Addr, TOY_Token, TOY_Value), MemResp#(TOY_Value)) port_to_dmem)
    //interface:
	        (FP_Unit#(TOY_Token,        //token type
		          TOY_ExecedInst,   //type from prev stage (exec)
			  void,             //request type
			  void,             //response type
			  TOY_ExecedInst)); //type to next stage (lcommit)

  
  let port_mem <- mkPort_Server("port_mem");
  
  FIFO#(Tuple2#(TOY_Token, TOY_ExecedInst))       waitingQ <- mkFIFO();

  //doReq

  rule doReq (True);
  
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
    $display("MEM Read1[%d] -> %s[%d]", va,
                                        (isJust(mva) ? "Just":"Nothing"),
                                        unJust(mva));

    $display("MEM Read2[%d] -> %s[%d]", vb,
                                        (isJust(mvb) ? "Just":"Nothing"),
                                        unJust(mvb));

    waitingQ.enq(tuple2(t, i));
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        if (isJust(mva))
           begin
             port_to_dmem.makeReq(Ld {addr: truncate(unJust(mva)) + zeroExtend(o), token: t});
           end
      tagged EStore{opdest: .oprd, val: .v, idx: .idx, offset: .o}:
        begin
          let addr = unJust(mva) + zeroExtend(o);
          if (isJust(mva) && isJust(mvb))
             begin
               port_to_dmem.makeReq(St {val: unJust(mvb), addr: truncate(addr), token: t});
             end
        end
      default: // push
        noAction;
    endcase
  endrule

  //getResp

  rule getResp(True);
  
    match {.tok, .i} = waitingQ.first();
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        begin
          let resp <- port_to_dmem.getResp();
	  
          TOY_Value v = case (resp) matches
                      tagged LdResp .val: return val;
                      tagged StResp .*  : return 0; // impossible
                    endcase;
          waitingQ.deq();
          $display("MEM LdResp from Mem: %d Writing [%d] <= %h", tok, prd, v);
          port_mem.makeResp(tuple3(tok,?,EWB{pdest: prd, opdest: oprd}));
          bypass.write2(prd, v);
        end
      tagged EStore {opdest: .oprd, val: .*, idx: .*, offset: .*}:
        begin
          $display("MEM StResp from Mem: %d", tok);
          let resp <- port_to_dmem.getResp();
          waitingQ.deq();
          port_mem.makeResp(tuple3(tok, ?, ENop {opdest: oprd}));
        end
      default:
        begin
          $display("MEM Non-Mem Op: %d", tok);
          waitingQ.deq();
          port_mem.makeResp(tuple3(tok, ?, i));
        end
    endcase
  endrule

  //Interface to FP_Stage
  return port_mem.server;
  
endmodule  


//-------------------------------------------------------------------------//
// Local Commit Unit                                                       //
//-------------------------------------------------------------------------//

//mkTOY_LocalCommit :: BypassUnit -> FP_Unit

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


//-------------------------------------------------------------------------//
// Global Commit Unit                                                      //
//-------------------------------------------------------------------------//

//mkToy_GlobalCommit :: Memory -> FP_Unit

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
