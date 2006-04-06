import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import ToyMIPS::*;
import FunctionalPartition::*;
import TOY_ValueVector::*;
import Mem::*;
import BypassUnit::*;
import BypassFIFO::*;

// ToyMIPS is an extremely simple ISA designed to work as a proof of concept.
    

//-------------------------------------------------------------------------//
// Fetch Unit                                                              //
//-------------------------------------------------------------------------//

//mkTOY_Fetch :: IMem -> FP_Unit

module [Module] mkTOY_Fetch#(Server#(TOY_Addr, TOY_Inst) imem) 

       //interface:
                   (FP_Unit#(TOY_Token,                     // token type
		             void,                          // type from prev stage
			     TOY_Addr,                      // request type
			     TOY_Inst,                      // response type
			     Tuple2#(TOY_Addr, TOY_Inst))); // type to next stage
  
  FIFO#(Tuple2#(TOY_Token, TOY_Addr))           reqQ  <- mkFIFO();
  FIFO#(Tuple3#(TOY_Token, TOY_Addr, TOY_Inst)) respQ <- mkFIFO();

  //getMemResp

  rule getMemResp (True);
  
    TOY_Inst i <- imem.response.get();
    
    match {.tok, .addr} = reqQ.first();
    reqQ.deq();
    
    respQ.enq(tuple3(tok, addr, i));
  
  endrule

  //From FP_Stage

  interface Put request;

    method Action put(Tuple3#(TOY_Token, void, TOY_Addr) tup);
    
      match {.t, .*, .a} = tup;
      
      imem.request.put(a);
      
      freqs.enq(tuple2(t,a));
      
    endmethod

  endinterface

  //To FP_Stage

  interface Get response;

    method ActionValue#(Tuple3#(TOY_Token, TOY_Inst, Tuple2#(TOY_Addr, TOY_Inst))) get();
    
      fresps.deq();
      
      match {.tok,.addr,.resp} = fresps.first();
      
      return tuple3(tok, resp, tuple2(addr, resp)); 
      
    endmethod

  endinterface

endmodule  


//-------------------------------------------------------------------------//
// Decode Stage                                                            //
//-------------------------------------------------------------------------//

// Also lookup physical register from BypassUnit

// mkTOY_Decode :: BypassUnit -> FP_Unit

module [Module] mkTOY_Decode#(BypassUnit#(RName, PRName, TOY_Value, TOY_Token) bypass)
     //interface:
                (FP_Unit#(TOY_Token,                            //token type
		          Tuple2#(TOY_Addr, TOY_Inst),          //type from prev stage (fetch)
			  void,                                 //request type
			  TOY_DepInfo,                          //response type
			  Tuple2#(TOY_Addr, TOY_DecodedInst))); //type to next stage (exec)
                 
  FIFO#(Tuple3#(TOY_Token, TOY_DepInfo, Tuple2#(TOY_Addr, TOY_DecodedInst))) respQ <- mkFIFO();
  
  //From FP_Stage
  
  interface Put request;

    method Action put(Tuple3#(TOY_Token, Tuple2#(TOY_Addr, TOY_Inst), void) tup);
    
      match {.t, {.a, .inst}, .*} = tup;

      TOY_DepInfo depinfo = ?;
      TOY_DecodedInst decinst = ?;

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
      
      let pra = bypass.lookup1(ara);
      let prb = bypass.lookup2(arb);
      
      case (inst) matches
	tagged IAdd {dest: .rd, src1: .ra, src2: .rb}:
          begin
            let rtup <- bypass.makeMapping(Just(rd), t, False);
            match {.prd, .oprd} = rtup;
            decinst = DAdd {pdest: prd, opdest: oprd, op1: pra, op2: prb};
            depinfo = TOY_DepInfo {dest: Just(tuple2(rd, prd)), src1: Just(tuple2(ra, pra)), src2: Just(tuple2(rb,prb))};
          end
	tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
          begin
            let rtup <- b.makeMapping(Just(rd), t, False);
            match {.prd, .oprd} = rtup;
            decinst = DSub {pdest: prd, opdest: oprd, op1: pra, op2: prb};
            depinfo = TOY_DepInfo {dest: Just(tuple2(rd, prd)), src1: Just(tuple2(ra, pra)), src2: Just(tuple2(rb,prb))};
          end
	tagged IBz {cond: .c , addr:  .addr}:
          begin
            let rtup <- b.makeMapping(Nothing, t, True);// likely rewind candidate
            match {.prd, .oprd} = rtup;
            decinst = DBz {opdest: oprd, cond: pra, addr: prb};
            depinfo = TOY_DepInfo {dest: Nothing, src1: Just(tuple2(c,pra)), src2: Just(tuple2(addr,prb))};
          end
	tagged ILoad {dest: .rd, idx: .ri, offset: .ro}:
          begin
            let rtup <- b.makeMapping(Just(rd), t, False);
            match {.prd, .oprd} = rtup;
            decinst = DLoad{pdest: prd, opdest: oprd, idx: pra, offset: zeroExtend(ro)};
            depinfo = TOY_DepInfo {dest: Just(tuple2(rd,prd)), src1: Just(tuple2(ri,pra)), src2: Nothing};
          end
	tagged ILoadImm {dest: .rd, imm: .i}:
          begin
            let rtup <- b.makeMapping(Just(rd), t, False);
            match {.prd, .oprd} = rtup;
            decinst = DLoadImm {pdest: prd, opdest: oprd, TOY_Value: signExtend(i)};
            depinfo = TOY_DepInfo {dest: Just(tuple2(rd,prd)), src1: Nothing, src2: Nothing};
          end
	tagged IStore {src: .rsrc, idx: .ri, offset: .ro}:
          begin
	    let rtup <- b.makeMapping(Nothing, t, False);
            match {.prd, .oprd} = rtup;
            decinst = DStore{TOY_Value: pra, opdest: oprd, idx: prb, offset: zeroExtend(ro)};
            depinfo = TOY_DepInfo {dest: Nothing, src1: Just(tuple2(ri,prb)), src2: Just(tuple2(rsrc,pra))};
          end
        tagged ITerminate:
          begin
	    let rtup <- b.makeMapping(Nothing, t, False); // only to simplify logic
            match {.prd, .oprd} = rtup;
            decinst = DTerminate;
            depinfo = TOY_DepInfo {dest: Nothing, src1: Nothing, src2: Nothing};
          end
      endcase

      respQ.enq(tuple3(t, decinst, tuple2(a, decinst)));
    endmethod
  
  endinterface
  
  //To FP_Stage
 
  interface Get response;

    method ActionValue#(Tuple3#(TOY_Token, TOY_DepInfo, Tuple2#(TOY_Addr, TOY_DecodedInst))) get();
    
      respQ.deq();
      return respQ.first(); 
      
    endmethod
  
  endinterface
endmodule  


//-------------------------------------------------------------------------//
// Execute Unit                                                            //
//-------------------------------------------------------------------------//

// Also reads physical register file

// mkTOY_Execute :: BypassUnit -> FP_Unit

module [Module] mkTOY_Execute#(BypassUnit#(RName, PRName, TOY_Value, TOY_Token) bypass)
    //interface:
                (Unit#(TOY_Token,                          //token type
		       Tuple2#(TOY_Addr, TOY_DecodedInst), //type from prev stage (decode)
		       void,                               //request type
		       TOY_InstResult,                     //response type
		       TOY_ExecedInst));                   //type to next stage (mem)

  FIFO#(Tuple3#(TOY_Token, TOY_Addr, TOY_DecodedInst))      reqQ    <- mkFIFO();
  FIFO#(Tuple3#(TOY_Token, TOY_InstResult, TOY_ExecedInst)) respQ   <- mkFIFO();

  //execute

  rule execute (True);

   TOY_ExecedInst ei = ?;
   Maybe#(TOY_Addr) branchResult = Nothing;

   match {.t, .addr, .dec} = reqQ.first();

   PRName va = ?;
   PRName vb = ?;

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

    Maybe#(TOY_Value) mva = bypass.read1(va);
    Maybe#(TOY_Value) mvb = bypass.read2(vb);

 
    $display("Execute Read1[%d] -> %s[%d]", va,
                                            (isJust(mva) ? "Just":"Nothing"),
                                            unJust(mva));

    $display("Execute Read2[%d] -> %s[%d]", vb,
                                            (isJust(mvb) ? "Just":"Nothing"),
                                            unJust(mvb));


    case (dec) matches
         tagged DAdd {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
             if (isJust(mva) && isJust(mvb))
               begin
                 $display("Executing Add %d: (old %d)[%d] <= 0x%h", t, oprd, prd, unJust(mva) + unJust(mvb));
                 bypass.write1(prd,unJust(mva) + unJust(mvb));
                 respQ.enq(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
                 reqQ.deq();
               end
         tagged DSub {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
             if (isJust(mva) && isJust(mvb))
               begin
                 $display("Executing Sub %d: (old %d)[%d] <= 0x%h", t, oprd, prd, unJust(mva) - unJust(mvb));
                 bypass.write1(prd,unJust(mva) - unJust(mvb));
                 respQ.enq(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
                 reqQ.deq();
               end
         tagged DBz {opdest: .oprd, cond: .c, addr: .a}:
	   case (mva) matches
	     tagged Valid .cval:
	       if (cval != 0)
	       begin // XXX extra cleverness needed
		 respQ.enq(tuple3(t, RBranchNotTaken , ENop {opdest: oprd}));
		 reqQ.deq();
	       end
	       else case (mvb) matches // condition must be zero
	         tagged Valid .dest:
		 begin
                   respQ.enq(tuple3(t, RBranchTaken truncate(dest), ENop{opdest: oprd}));
                   reqQ.deq();
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
             respQ.enq(tuple3(t, RNop, ELoad {idx: idx, offset: o, pdest:prd, opdest: oprd}));
             reqQ.deq();
           end
         tagged DLoadImm {pdest: .prd, opdest: .oprd, TOY_Value: .val}:
           begin
             bypass.write1(prd, signExtend(val));
             respQ.enq(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
             reqQ.deq();
           end
         tagged DStore {TOY_Value: .v, opdest: .oprd, idx: .idx, offset: .o}:// XXX do offset calc
           begin
             $display("Executing Store %d: (old %d)", t, oprd);
             respQ.enq(tuple3(t, RNop, EStore {idx: idx, offset: o, val: v, opdest: oprd}));
             reqQ.deq();
           end
         tagged DTerminate:
           begin
             $display("Executing Terminate %d: ", t);
             respQ.enq(tuple3(t, RTerminate, ETerminate));
             reqQ.deq();
           end
    endcase
  endrule

  //From FP_Stage

  interface Put request;

    method Action put(Tuple3#(TOY_Token, Tuple2#(TOY_Addr, TOY_DecodedInst), void) tup);
      match {.t, {.addr, .dec}, .*} = tup;

      reqQ.enq(tuple3(t, addr, dec));
    endmethod

  endinterface

  //From FP_Stage

  interface Get response;

    method ActionValue#(Tuple3#(TOY_Token,TOY_InstResult,TOY_ExecedInst)) get();
      f.deq();
      return f.first(); 
    endmethod

  endinterface
  
endmodule  


//-------------------------------------------------------------------------//
// Memory Unit                                                             //
//-------------------------------------------------------------------------//

// mkTOY_Mem :: BypassUnit -> Memory -> FP_Unit

module [Module] mkTOY_Mem#(BypassUnit#(RName, PRName, TOY_Value, TOY_Token) bypass,
	                   Memory#(TOY_Addr, TOY_Inst, TOY_Value, TOY_Token) mem)
    //interface:
	        (FP_Unit#(TOY_Token,        //token type
		          TOY_ExecedInst,   //type from prev stage (exec)
			  void,             //request type
			  void,             //response type
			  TOY_ExecedInst)); //type to next stage (lcommit)

  FIFO#(Tuple2#(TOY_Token, TOY_ExecedInst))       reqQ     <- mkFIFO();
  FIFO#(Tuple3#(TOY_Token, void, TOY_ExecedInst)) respQ    <- mkFIFO();
  FIFO#(Tuple2#(TOY_Token, TOY_ExecedInst))       waitingQ <- mkFIFO();

  //doReq

  rule doReq (True);
  
    match {.t,.i} = reqQ.first();

    PRName va = ?;
    PRName vb = ?;

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

    waitingQ.enq(tuple3(t, ?, i));
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        if (isJust(mva))
           begin
             mem.dmem.request.put(Ld {addr: truncate(unJust(mva)) + zeroExtend(o), token: t});
             reqs.deq();
           end
      tagged EStore{opdest: .oprd, val: .v, idx: .idx, offset: .o}:
        begin
          let addr = unJust(mva) + zeroExtend(o);
          if (isJust(mva) && isJust(mvb))
             begin
               mem.dmem.request.put(St {val: unJust(mvb), addr: truncate(addr), token: t});
               reqs.deq();
             end
        end
      default: // push
        reqs.deq();
    endcase
  endrule

  //getResp

  rule getResp(True);
  
    match {.tok,.*,.i} = wResp.first();
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        begin
          let resp <- mem.dmem.response.get();
          TOY_Value v = case (resp) matches
                      tagged LdResp .val: return val;
                      tagged StResp .*  : return ?; // impossible
                    endcase;
          wResp.deq();
          $display("MEM LdResp from Mem: %d Writing [%d] <= %h", tok, prd, v);
          f.enq(tuple3(tok,?,EWB{pdest: prd, opdest: oprd}));
          b.write2(prd, v);
        end
      tagged EStore {opdest: .oprd, val: .*, idx: .*, offset: .*}:
        begin
          $display("MEM StResp from Mem: %d", tok);
          let resp <- mem.dmem.response.get();
          wResp.deq();
          f.enq(tuple3(tok,?,ENop{opdest: oprd}));
        end
      default:
        begin
          $display("MEM Non-Mem Op: %d", tok);
          wResp.deq();
          f.enq(wResp.first());
        end
    endcase
  endrule

  //To FP_Stage

  interface Put request;

    method Action put(Tuple3#(TOY_Token, TOY_ExecedInst, void) tup);
    
      match {.t, .i, .*} = tup;
      reqs.enq(tuple2(t,i));
      
    endmethod
  
  endinterface
  
  //From FP_Stage
  
  interface Get response;
  
    method ActionValue#(Tuple3#(TOY_Token, void, TOY_ExecedInst)) get();
    
      f.deq();
      return f.first(); 
      
    endmethod
  
  endinterface
  
endmodule  


//-------------------------------------------------------------------------//
// Local Commit Unit                                                       //
//-------------------------------------------------------------------------//

//mkTOY_LocalCommit :: BypassUnit -> FP_Unit

module [Module] mkTOY_LocalCommit#(BypassUnit#(RName, PRName, TOY_Value, TOY_Token) bypass)
    //interface:
                (FP_Unit#(TOY_Token,      //token type
		          TOY_ExecedInst, //type from prev stage (mem)
			  void,           //request type
			  void,           //response type
			  void));         //type to next stage (gcommit)

  FIFO#(TOY_Token) respQ   <- mkFIFO();
  
  //From FP_Stage
  
  interface Put request;

    method Action put(Tuple3#(TOY_Token, TOY_ExecedInst, void) tup);
    
      match {.t, .ei, .*} = tup;
      
      f.enq(t);
      
      PRName p = case (ei) matches
                   tagged ENop    .x: return(x.opdest);
		   tagged EWB     .x: return(x.opdest);
		   tagged ELoad   .x: return(x.opdest);
		   tagged EStore  .x: return(x.opdest);
                   tagged ETerminate: return(?);
		 endcase;
		 
      bypass.freePReg(t, p);
    endmethod
  
  endinterface
  
  //To FP_Stage
  
  interface Get response;
  
    method ActionValue#(Tuple3#(TOY_Token, void, void)) get();
    
      respQ.deq();
      return respQ.first(); 
      
    endmethod
  
  endinterface
  
endmodule  


//-------------------------------------------------------------------------//
// Global Commit Unit                                                      //
//-------------------------------------------------------------------------//

//mkToy_GlobalCommit :: Memory -> FP_Unit

module [Module] mkTOY_GlobalCommit#(Memory#(TOY_Addr, TOY_Inst, TOY_Value, TOY_Token) mem)
    //interface:
                (FP_Unit#(TOY_Token, //token type
		          void,      //type from prev stage (lcommit)
			  void,      //request type
			  void,      //response type
			  void));    //type for next stage (tokgen)

  FIFO#(TOY_Token) respQ <- mkFIFO();
  
  //From FP_Stage
  
  interface Put request;
                
    method Action put(Tuple3#(TOY_Token, void, void) t);
    
      respQ.enq(t);
      mem.commit(t.fst());
      
    endmethod
  
  endinterface
  
  //To FP_Stage
  
  interface Get response;

    method ActionValue#(Tuple3#(TOY_Token, void, void)) get();
    
      respQ.deq();
      return respQ.first(); 
      
    endmethod

  endinterface
  
endmodule  

//-------------------------------------------------------------------------//
// Toy Functional Partition                                                //
//-------------------------------------------------------------------------//

//mkTOY_FP :: Memory -> FunctionalPartition

module [Module] mkTOY_FP#(Memory#(TOY_Addr, TOY_Inst, TOY_Value, TOY_Token) memsystem)
    //interface:						       
  		(FunctionalPartition#(TOY_Tick, TOY_Token,  //tick type, token type
  				      TOY_Addr, TOY_Inst,   //fetchReq, fetchResp
  				      void, TOY_DepInfo,    //decodeReq, decodeResp
  				      void, TOY_InstResult, //execReq, execResp
  				      void, void,           //memReq, memResp
  				      void, void,           //lcommitReq, lcommitResp
  				      void, void));         //gcommitReq, gcommitResp  

  let bypass <- mkBypassUnit();
  
  let tok  <- mkTOY_TokGen();
  let fet  <- mkTOY_Fetch(memsystem.imem);
  let dec  <- mkTOY_Decode(b);
  let exe  <- mkTOY_Execute(b);
  let mem  <- mkTOY_Memory(b, memsystem);
  let lco  <- mkTOY_LocalCommit(b);
  let gco  <- mkTOY_GlobalCommit(memsystem);
  
  let fp <- mkFP(tok, fet, dec, exe, mem, lco, gco, 8);

  return fp;

endmodule
