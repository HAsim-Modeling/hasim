import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import Datatypes::*;
import ValueVector::*;
import Mem::*;
import BypassUnit::*;
                 
//-----------------------------------------------------------------------------------------------------------//
// General Unit (in-order unit)                                                                              //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkFP_Unit#(ValueVector#(Bool, token_T) valids,
                           ValueVector#(Bool, token_T) dones,
                           Unit#(token_T, init_T, req_T, resp_T, next_T) u,
                           Integer sz) // parameters

               (FP_Unit#(tick_T, token_T, init_T, req_T, resp_T, next_T)) //interface
        provisos
          (Bits#(token_T, tsz),Bounded#(token_T),Eq#(token_T),Literal#(token_T),
           Bits#(init_T, isz),
           Bits#(req_T, qsz),
           Bits#(resp_T,psz),
           Bits#(next_T,nsz));

   FIFO#(Tuple3#(token_T,resp_T,next_T))          unitRespQ <- mkSizedFIFO(sz);
   FIFO#(Tuple2#(token_T,next_T))                 nextQ     <- mkSizedFIFO(sz);
                 
   //SRAM tables
   RegFile#(token_T, init_T)                      tbl_init <- mkRegFileFull();

//  Vector#(256,Reg#(Bool))                         valids  <- Vector::replicateM(mkReg(False));
//  Vector#(256,Reg#(Bool))                         dones   <- Vector::replicateM(mkReg(False));

  match {.respQToken,.*,.*} = unitRespQ.first();
  Bool respValid = valids.read1(respQToken);
   
  rule getUnitResponse(True);
    Tuple3#(token_T, resp_T, next_T) tup <- u.response.get();
    match {.tok, .resp, .next} = tup;

   Bool valid = valids.read2(tok);
   
   if(valid)
      begin // don't insert it was killed
        unitRespQ.enq(tup);
        dones.write1(tok,True);
      end       
  endrule
    
  rule tossDeadResps(respValid == False);
    unitRespQ.deq();
  endrule
  
  interface Put in;
           
    method Action put(Tuple2#(token_T, init_T) tup);

     match {.tok,.iVal} = tup;

     Bool valid = valids.read3(tok);

     if(valid)
       begin        
	 $display("ERROR: reinserting allocated token %h", tok);
       end
     else
       begin
	 //Set valid to true and done to false
	 valids.write2(tok,True);
	 dones.write2(tok,False);
	 tbl_init.upd(tok, iVal);
       end
    endmethod
  endinterface

  interface Server server;
  
    interface Put request;
  
      method Action put(Tuple3#(token_T, req_T, tick_T) tup);
       match {.tok, .req, .tick} = tup;


       Bool valid = valids.read4(tok);      
       Bool done  =  dones.read1(tok);  

       init_T iVal = tbl_init.sub(tok);

	if (!valid)
	  $display("ERROR: requesting unallocated token %h", tok);
	else if (done)
	  $display("ERROR: re-requesting finished token %h", tok);            
	else // !done
	  u.request.put(tuple3(tok, iVal, req));
      endmethod

    endinterface

    //rule getUnitResponse
    //rule tossDeadResps

    interface Get response;

      method ActionValue#(Tuple2#(token_T, resp_T)) get() if (respValid);

       match {.tok, .resp, .next} = unitRespQ.first();
       unitRespQ.deq();
       nextQ.enq(tuple2(tok,next));
       return tuple2(tok,resp);
      endmethod

    endinterface
    
  endinterface
  // rule tossDeadNexts
  
  interface Get out;

    method ActionValue#(Tuple2#(token_T, next_T)) get();
     nextQ.deq();
     return nextQ.first;
    endmethod

  endinterface

  method Action                                     killToken(token_T tok);
    valids.write2(tok,False);
  endmethod

endmodule

//-----------------------------------------------------------------------------------------------------------//
// BoolVector                                                                                                //
//-----------------------------------------------------------------------------------------------------------//

(* synthesize *)
module [Module] mkBoolVector_Token(ValueVector#(Bool, Token));

  let v <- mkBoolVector();
   
  return v;
endmodule


//-----------------------------------------------------------------------------------------------------------//
// Token Generation Unit (head/tail pointer)                                                                              //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkFP_TokGen(FP_Unit#(Tick, Token, void, void, void, void));


  Reg#(Token) r_first <- mkReg(0);
  Reg#(Token) r_free <- mkReg(0);
  
  //Killing tokens can never result in free tokens being taken.
  //Therefore we never need to worry about the responses being invalid.
  FIFO#(Token) respQ <- mkFIFO();
  FIFO#(Token) nextQ <- mkFIFO();
 
  interface Put in;
           
    method Action put(Tuple2#(Token, void) tup);
      match {.t, .*} = tup;
      
      //complete token t
      
      if (r_first != t) 
        $display("ERROR: Tokens completing out of order");
     
      r_first <= r_first + 1;
      
    endmethod
  endinterface

  interface Server server;
  
    interface Put request;
  
      method Action put(Tuple3#(Token, void, Tick) tup);
      
       match {.*, .*, .tick} = tup;
       
       //allocate a new token
       respQ.enq(r_free);
       nextQ.enq(r_free);
       r_free <= r_free + 1;

      endmethod

    endinterface

    interface Get response;

      method ActionValue#(Tuple2#(Token, void)) get();

        //return allocated token
	respQ.deq();
	return tuple2(respQ.first(), ?);
      endmethod 

    endinterface
    
  endinterface
  
  interface Get out;

    method ActionValue#(Tuple2#(Token, void)) get();
      nextQ.deq();
      return tuple2(nextQ.first, ?); //This Does Not Exist.
    endmethod

  endinterface

  method Action killToken(Token tok);
    //free tok a and all tokens before it
    r_free <= tok;
  endmethod
  
endmodule
                  

//-----------------------------------------------------------------------------------------------------------//
// Fetch Unit (in-order _unit_)                                                                              //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkFetch#(Server#(Addr, Inst) imem) (Unit#(Token, void, Addr, Inst, Tuple2#(Addr,Inst)));
  
  FIFO#(Tuple2#(Token, Addr)) freqs <- mkFIFO();
  FIFO#(Tuple3#(Token, Addr, Inst)) fresps <- mkFIFO();


  rule getMemResp (True);
  
    Inst i <- imem.response.get();
    
    match {.tok, .addr} = freqs.first();
    freqs.deq();
    
    fresps.enq(tuple3(tok, addr, i));
  
  endrule

  interface Put request;

    method Action put(Tuple3#(Token, void, Addr) tup);
      match {.t, .*, .a} = tup;
      imem.request.put(a);
      freqs.enq(tuple2(t,a));
    endmethod

  endinterface

  interface Get response;

    method ActionValue#(Tuple3#(Token,Inst,Tuple2#(Addr,Inst))) get();
      fresps.deq();
      match {.tok,.addr,.resp} = fresps.first();
      return tuple3(tok, resp, tuple2(addr,resp)); 
    endmethod

  endinterface

endmodule  

module [Module] mkFP_Fetch#(Server#(Addr, Inst) imem) (FP_Unit#(Tick,Token, void, Addr, Inst, Tuple2#(Addr,Inst)));
  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 
    
  Unit#(Token, void, Addr, Inst, Tuple2#(Addr,Inst)) memunit <- mkFetch(imem);
  
  let i <- mkFP_Unit(valids, dones, memunit,2);

  return i;     
endmodule
                  

//-----------------------------------------------------------------------------------------------------------//
// Decode Unit (in-order _unit_)                                                                             //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkDecode#(BypassUnit#(RName, PRName, Value, Token) b)
           (Unit#(Token, Tuple2#(Addr,Inst), void, DepInfo, Tuple2#(Addr, DecodedInst)));
                 
  FIFO#(Tuple3#(Token,DepInfo,Tuple2#(Addr, DecodedInst)))           f   <- mkFIFO();
  
  interface Put request;

    method Action put(Tuple3#(Token, Tuple2#(Addr,Inst), void) tup);
      match {.t, {.a,.inst}, .*} = tup;

      DepInfo depInfo = ?;
      DecodedInst di = ?;

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
        endcase;
      
      let pra = b.lookup1(ara);
      let prb = b.lookup2(arb);
      
      case (inst) matches
	tagged IAdd {dest: .rd, src1: .ra, src2: .rb}:
          begin
            let rtup <- b.makeMapping(Just(rd), t, False);
            match {.prd, .oprd} = rtup;
            di =      DAdd {pdest: prd, opdest: oprd, op1: pra, op2: prb};
            depInfo = DepInfo {dest: Just(tuple2(rd, prd)), src1: Just(tuple2(ra, pra)), src2: Just(tuple2(rb,prb))};
          end
	tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
          begin
            let rtup <- b.makeMapping(Just(rd), t, False);
            match {.prd, .oprd} = rtup;
            di =      DSub {pdest: prd, opdest: oprd, op1: pra, op2: prb};
            depInfo = DepInfo {dest: Just(tuple2(rd, prd)), src1: Just(tuple2(ra, pra)), src2: Just(tuple2(rb,prb))};
          end
	tagged IBz {cond: .c , addr:  .addr}:
          begin
            let rtup <- b.makeMapping(Nothing, t, True);// likely rewind candidate
            match {.prd, .oprd} = rtup;
            di =      DBz {opdest: oprd, cond: pra, addr: prb};
            depInfo = DepInfo {dest: Nothing, src1: Just(tuple2(c,pra)), src2: Just(tuple2(addr,prb))};
          end
	tagged ILoad {dest: .rd, idx: .ri, offset: .ro}:
          begin
            let rtup <- b.makeMapping(Just(rd), t, False);
            match {.prd, .oprd} = rtup;
            di =      DLoad{pdest: prd, opdest: oprd, idx: pra, offset: zeroExtend(ro)};
            depInfo = DepInfo {dest: Just(tuple2(rd,prd)), src1: Just(tuple2(ri,pra)), src2: Nothing};
          end
	tagged ILoadImm {dest: .rd, imm: .i}:
          begin
            let rtup <- b.makeMapping(Just(rd), t, False);
            match {.prd, .oprd} = rtup;
            di =      DLoadImm {pdest: prd, opdest: oprd, value: signExtend(i)};
            depInfo = DepInfo {dest: Just(tuple2(rd,prd)), src1: Nothing, src2: Nothing};
          end
	tagged IStore {src: .rsrc, idx: .ri, offset: .ro}:
          begin
	    let rtup <- b.makeMapping(Nothing, t, False);
            match {.prd, .oprd} = rtup;
            di =      DStore{value: pra, opdest: oprd, idx: prb, offset: zeroExtend(ro)};
            depInfo = DepInfo {dest: Nothing, src1: Just(tuple2(ri,prb)), src2: Just(tuple2(rsrc,pra))};
          end
      endcase

      f.enq(tuple3(t,depInfo,tuple2(a,di)));
    endmethod
  
  endinterface
  
  interface Get response;

    method ActionValue#(Tuple3#(Token, DepInfo,Tuple2#(Addr, DecodedInst))) get();
      f.deq();
      return f.first(); 
    endmethod
  
  endinterface
endmodule  


module [Module] mkFP_Decode#(BypassUnit#(RName, PRName, Value, Token) b)(FP_Unit#(Tick, Token,   Tuple2#(Addr,Inst),
                                              void, DepInfo, Tuple2#(Addr,DecodedInst)));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token();
   
  Unit#(Token, Tuple2#(Addr,Inst), void, DepInfo, Tuple2#(Addr, DecodedInst)) dec <- mkDecode(b);
  
  let i <- mkFP_Unit(valids, dones, dec,2);
  return i;     
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Execute Unit (in-order _unit_)                                                                            //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkExecute#(BypassUnit#(RName, PRName, Value, Token) b)
	    (Unit#(Token, Tuple2#(Addr,DecodedInst),void, InstResult, ExecedInst));

  FIFO#(Tuple3#(Token,InstResult, ExecedInst)) f   <- mkFIFO();
  FIFO#(Tuple3#(Token,Addr, DecodedInst))     iq   <- mkFIFO();

  rule execute(True);

   ExecedInst ei = ?;
   Maybe#(Addr) branchResult = Nothing;

   match {.t,.addr,.dec} = iq.first();

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

    Maybe#(Value) mva = b.read1(va);
    Maybe#(Value) mvb = b.read2(vb);

    case (dec) matches
         tagged DAdd {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
             if (isJust(mva) && isJust(mvb))
               begin
                 b.write1(prd,unJust(mva) + unJust(mvb));
                 f.enq(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
                 iq.deq();
               end
         tagged DSub {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
             if (isJust(mva) && isJust(mvb))
               begin
                 b.write1(prd,unJust(mva) - unJust(mvb));
                 f.enq(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
                 iq.deq();
               end
         tagged DBz {opdest: .oprd, cond: .c, addr: .a}:
	   case (mva) matches
	     tagged Valid .cval:
	       if (cval != 0)
	       begin // XXX extra cleverness needed
		 f.enq(tuple3(t, RBranchNotTaken , ENop {opdest: oprd}));
		 iq.deq();
	       end
	       else case (mvb) matches // condition must be zero
	         tagged Valid .dest:
		 begin
                   f.enq(tuple3(t, RBranchTaken truncate(dest), ENop{opdest: oprd}));
                   iq.deq();
                 end
	         default:
		   noAction;
		 endcase
	     default:
	       noAction;
	   endcase
         tagged DLoad {pdest: .prd, opdest: .oprd, idx: .idx, offset: .o}: // XXX do offset calc
           begin
             f.enq(tuple3(t, RNop, ELoad {idx: idx, offset: o, pdest:prd, opdest: oprd}));
             iq.deq();
           end
         tagged DLoadImm {pdest: .prd, opdest: .oprd, value: .val}:
           begin
             b.write1(prd, signExtend(val));
             f.enq(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
             iq.deq();
           end
         tagged DStore {value: .v, opdest: .oprd, idx: .idx, offset: .o}:// XXX do offset calc
           begin
             f.enq(tuple3(t,RNop,EStore {idx: idx, offset: o, val: v, opdest: oprd}));
             iq.deq();
           end
    endcase
  endrule

  interface Put request;

    method Action put(Tuple3#(Token, Tuple2#(Addr, DecodedInst), void) tup);
      match {.t, {.addr, .dec}, .*} = tup;

      iq.enq(tuple3(t,addr,dec));
    endmethod

  endinterface

  interface Get response;

    method ActionValue#(Tuple3#(Token,InstResult,ExecedInst)) get();
      f.deq();
      return f.first(); 
    endmethod

  endinterface
  
endmodule  

module [Module] mkFP_Execute#(BypassUnit#(RName, PRName, Value, Token) b)
	    (FP_Unit#(Tick, Token, Tuple2#(Addr,DecodedInst),void, InstResult, ExecedInst));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 
  Unit#(Token, Tuple2#(Addr,DecodedInst),void, InstResult, ExecedInst) exe <- mkExecute(b);

  let i <- mkFP_Unit(valids, dones, exe,3);
  return i;     
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Memory Unit (in-order _unit_)                                                                             //
//-----------------------------------------------------------------------------------------------------------//

module [Module]  mkDMemory#(BypassUnit#(RName, PRName, Value, Token) b,
	                    Memory#(Addr, Inst, Value, Token) mem)
  (Unit#(Token, ExecedInst, void, void, ExecedInst));

  FIFO#(Tuple2#(Token, ExecedInst))       reqs  <- mkFIFO();
  FIFO#(Tuple3#(Token, void, ExecedInst))  f    <- mkFIFO();
  FIFO#(Tuple3#(Token, void, ExecedInst)) wResp <- mkFIFO();

  rule doReq(True);
    match {.t,.i} = reqs.first();

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

    let mva = b.read3(va);
    let mvb = b.read4(vb);

    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        if (isJust(mva))
           begin
             mem.dmem.request.put(Ld{addr: truncate(unJust(mva)) + zeroExtend(o), token: t});
             wResp.enq(tuple3(t,?,EWB{pdest: prd, opdest: oprd}));
             reqs.deq();
           end
      tagged EStore{opdest: .oprd, val: .v, idx: .idx, offset: .o}:
        begin
          let addr = unJust(mva) + zeroExtend(o);
          if (isJust(mva) && isJust(mvb))
             begin
               mem.dmem.request.put(St{val: unJust(mvb), addr: truncate(addr), token: t});
               wResp.enq(tuple3(t,?,ENop{opdest: oprd}));
               reqs.deq();
             end
        end
      default: // push
        begin
          wResp.enq(tuple3(t,?,i));
          reqs.deq();
        end
    endcase
  endrule

  rule getResp(True);
    match {.tok,.*,.i} = wResp.first();
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        begin
          let resp <- mem.dmem.response.get();
          Value v = case (resp) matches
                      tagged LdResp .v: return v;
                      tagged StResp .*: return ?; // impossible
                    endcase;
          wResp.deq();
          f.enq(wResp.first());
          b.write2(prd, v);
        end
      tagged EStore .*:
        begin
          let resp <- mem.dmem.response.get();
          wResp.deq();
          f.enq(wResp.first());
        end
      default:
        begin
          wResp.deq();
          f.enq(wResp.first());
        end
    endcase
  endrule

  interface Put request;

    method Action put(Tuple3#(Token, ExecedInst, void) tup);
      match {.t, .i, .*} = tup;
      reqs.enq(tuple2(t,i));
    endmethod
  
  endinterface
  
  interface Get response;
  
    method ActionValue#(Tuple3#(Token, void, ExecedInst)) get();
      f.deq();
      return f.first(); 
    endmethod
  
  endinterface
  
endmodule  

module [Module] mkFP_Memory#(BypassUnit#(RName, PRName, Value, Token) b, Memory#(Addr, Inst, Value, Token) mem)(FP_Unit#(Tick, Token, ExecedInst, void, void, ExecedInst));
  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token();
   
  Unit#(Token, ExecedInst, void, void, ExecedInst) memunit <- mkDMemory(b, mem);
  
  let i <- mkFP_Unit(valids, dones, memunit, 2);
  return i;     
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Local Commit Unit (in-order _unit_)                                                                             //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkLocalCommit#(BypassUnit#(RName, PRName, Value, Token) b)
                (Unit#(Token, ExecedInst, void, void,void));

  FIFO#(Tuple3#(Token, void, void)) f   <- mkFIFO();
  
  interface Put request;

    method Action put(Tuple3#(Token, ExecedInst, void) tup);
      match {.t, .ei, .*} = tup;
      f.enq(tuple3(t,?,?)); //? == unit
      PRName p = case (ei) matches
                   tagged ENop    .x: return(x.opdest);
		   tagged EWB     .x: return(x.opdest);
		   tagged ELoad   .x: return(x.opdest);
		   tagged EStore  .x: return(x.opdest);
		 endcase;
      b.freePReg(t, p);
    endmethod
  
  endinterface
  
  interface Get response;
  
    method ActionValue#(Tuple3#(Token, void, void)) get();
      f.deq();
      return f.first(); 
    endmethod
  
  endinterface
  
endmodule  

module [Module] mkFP_LocalCommit#(BypassUnit#(RName, PRName, Value, Token) b)
                   (FP_Unit#(Tick,Token, ExecedInst, void, void,void));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 

  Unit#(Token, ExecedInst, void, void,void) com <- mkLocalCommit(b);
  
  let i <- mkFP_Unit(valids, dones, com,2);
  return i;     
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Global Commit Unit (in-order _unit_)                                                                      //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkGlobalCommit#(Memory#(Addr, Inst, Value, Token) mem)(Unit#(Token, void, void, void,void));

  FIFO#(Tuple3#(Token, void, void)) f   <- mkFIFO();
  
  interface Put request;
                
    method Action put(Tuple3#(Token, void, void) t);
      f.enq(t);
      mem.commit(t.fst());
    endmethod
  
  endinterface
  
  interface Get response;

    method ActionValue#(Tuple3#(Token, void, void)) get();
      f.deq();
      return f.first(); 
    endmethod

  endinterface
  
endmodule  

module [Module] mkFP_GlobalCommit#(Memory#(Addr, Inst, Value, Token) mem)
                   (FP_Unit#(Tick,Token, void, void, void,void));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 

  Unit#(Token, void, void, void,void) com <- mkGlobalCommit(mem);
  
  let i <- mkFP_Unit(valids, dones, com,2);
  return i;     
endmodule
                   

//-----------------------------------------------------------------------------------------------------------//
// Top                                                                                                       //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkFP_Test#(Memory#(Addr, Inst, Value, Token) memsystem)

                          (FunctionalPartition#(Tick, Token,
                                        	Addr, Inst,
						void, DepInfo,
						void, InstResult,
						void, void,
						void, void,
						void, void));

  let b    <- mkBypassUnit();
  let tok  <- mkFP_TokGen();
  let fet  <- mkFP_Fetch(memsystem.imem);
  let dec  <- mkFP_Decode(b);
  let exe  <- mkFP_Execute(b);
  let mem  <- mkFP_Memory(b, memsystem);
  let lco  <- mkFP_LocalCommit(b);
  let gco  <- mkFP_GlobalCommit(memsystem);

  mkConnection(tok.out, fet.in);
  mkConnection(fet.out, dec.in);
  mkConnection(dec.out, exe.in);
  mkConnection(exe.out, mem.in);
  mkConnection(mem.out, lco.in);
  mkConnection(lco.out, gco.in);
  mkConnection(gco.out, tok.in);
  
  interface tokgen = tok.server;
  interface fetch = fet.server;
  interface decode = dec.server;
  interface execute = exe.server;
  interface memory = mem.server;
  interface local_commit = lco.server;
  interface global_commit = gco.server;
  
  method Action killToken(Token t);
    
    tok.killToken(t);
    fet.killToken(t);
    dec.killToken(t);
    exe.killToken(t);
    mem.killToken(t);
    lco.killToken(t);
    gco.killToken(t);
    b.rewindtoToken(t-1); //so we catch the branch
  endmethod

endmodule
