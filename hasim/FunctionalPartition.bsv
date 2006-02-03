import GetPut::*;
import RegFile2::*;
import FIFO::*;
import Vector::*;

import Interfaces::*;
import Primitive::*;
import ValueVector::*;

typedef UInt#(8) Token;
typedef UInt#(64) Tick;
typedef Bit#(32)  Addr;
typedef Bit#(5)   RName;
typedef Bit#(6)   PRName;
typedef Bit#(32)  Value;

typedef union tagged {
 struct {RName dest; RName src1; RName src2;}    IAdd;
 struct {RName dest; RName src1; RName src2;}    ISub;
 struct {RName cond; RName addr;}                IBz;
 struct {RName dest; RName idx; Bit#(5) offset;} ILoad;
 struct {RName dest; Bit#(10) imm;}              ILoadImm;
 struct {RName src;  RName idx; Bit#(5) offset;} IStore;
}
 Inst deriving (Eq,Bits);
                     
typedef union tagged {
  struct       {PRName pdest; PRName opdest; PRName op1; PRName op2;  } DAdd;
  struct       {PRName pdest; PRName opdest; PRName op1; PRName op2;  } DSub;
  struct       {              PRName opdest; PRName cond;PRName addr; } DBz;     
  struct       {PRName pdest; PRName opdest; PRName idx; Bit#(6) offset;} DLoad;
  struct       {PRName pdest; PRName opdest;   Bit#(12)  value;} DLoadImm;
  struct       {PRName value; PRName opdest; PRName idx; Bit#(6) offset;} DStore;
}
  DecodedInst deriving (Eq,Bits);

typedef struct {
  Maybe#(Tuple2#(RName, PRName))  dest;
  Maybe#(Tuple2#(RName, PRName))  src1;           
  Maybe#(Tuple2#(RName, PRName))  src2;
}
  DepInfo deriving(Eq,Bits);
       
typedef union tagged {
  struct {                            PRName pdest; PRName opdest;}  EWB;
  struct {                                          PRName opdest;}  ENop;
  struct { PRName idx; Bit#(6) offset;PRName pdest; PRName opdest;}  ELoad;
  struct { PRName idx; Bit#(6) offset;PRName val;   PRName opdest;}  EStore;
}
  ExecedInst deriving (Eq,Bits);

typedef union tagged {
  struct {           Addr addr; Token token;} Ld;
  struct {Value val; Addr addr; Token token;} St;
}
  MemReq deriving(Eq,Bits);

typedef union tagged {
  Value LdResp;
  void  StResp;
}
  MemResp deriving(Eq,Bits);
                 
//-----------------------------------------------------------------------------------------------------------//
// General Unit (in-order unit)                                                                              //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkFM_Unit#(ValueVector#(Bool, token_T) valids,
                  ValueVector#(Bool, token_T) dones,
                  Unit#(token_T, init_T, req_T, resp_T, next_T) u,
                   Integer sz) // parameters
                 (FM_Unit#(tick_T, token_T, init_T, req_T, resp_T, next_T)) //inf
        provisos
          (Bits#(token_T, tsz),Bounded#(token_T),Eq#(token_T),Literal#(token_T),
           Bits#(init_T, isz),
           Bits#(req_T, qsz),
           Bits#(resp_T,psz),
           Bits#(next_T,nsz));

   //token_T tminT = minBound;
   //token_T tmaxT = maxBound;
   
   //Integer                 minT = primBitToInteger(pack(tminT)) + 1;
   //Integer                 maxT = primBitToInteger(pack(tmaxT)) + 1;

   FIFO#(Tuple3#(token_T,resp_T,next_T))          unitRespQ <- mkSizedFIFO(sz);
   FIFO#(Tuple2#(token_T,next_T))                 nextQ     <- mkSizedFIFO(sz);
                 
   //SRAM tables
   RegFile#(token_T, init_T)                      tbl_init <- mkRegFileFull();

//  Vector#(256,Reg#(Bool))                         valids  <- Vector::replicateM(mkReg(False));
//  Vector#(256,Reg#(Bool))                         dones   <- Vector::replicateM(mkReg(False));

  match {.respQToken,.*,.*} = unitRespQ.first();
  Bool respValid = valids.read1(respQToken);
   
  rule getUnitResponse(True);
    Tuple3#(token_T, resp_T, next_T) tup <- u.response();
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
           
  method Action                                                   putPrevFM(Tuple2#(token_T, init_T) tup);
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

  method Action                                                   putTM(Tuple3#(token_T, req_T, tick_T) tup);
   match {.tok, .req, .tick} = tup;


   Bool valid = valids.read4(tok);      
   Bool done  =  dones.read1(tok);  

   init_T iVal = tbl_init.sub(tok);
   
    if (!valid)
      $display("ERROR: requesting unallocated token %h", tok);
    else if (done)
      $display("ERROR: re-requesting finished token %h", tok);            
    else // !done
      u.request(tok, iVal, req);
  endmethod


  //rule getUnitResponse
  //rule tossDeadResps
    
  method ActionValue#(Tuple2#(token_T, resp_T)) getTM(tick_T t) if (respValid);

   match {.tok, .resp, .next} = unitRespQ.first();
   unitRespQ.deq();
   nextQ.enq(tuple2(tok,next));
   return tuple2(tok,resp);
  endmethod

  // rule tossDeadNexts

  method ActionValue#(Tuple2#(token_T, next_T))     getNextFM();
   nextQ.deq();
   return nextQ.first;
  endmethod

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
// Fetch Unit (in-order _unit_)                                                                              //
//-----------------------------------------------------------------------------------------------------------//

(* synthesize *)
module [Module] mkIMemory(Unit#(Token, void, Addr, Inst, Tuple2#(Addr,Inst)));
  RegFile#(Addr, Inst)                 m   <- mkRegFileFull();
  FIFO#(Tuple3#(Token,Addr,Inst))           f   <- mkFIFO();
                
  method Action request(Token t, void v, Addr a);
    f.enq(tuple3(t,a,m.sub(a)));
  endmethod
  
  method ActionValue#(Tuple3#(Token,Inst,Tuple2#(Addr,Inst))) response();
    f.deq();
    match {.tok,.addr,.resp} = f.first();
    return tuple3(tok, resp, tuple2(addr,resp)); 
  endmethod
endmodule  

(* synthesize *)
module [Module] mkFM_Fetch(FM_Unit#(Tick,Token, void, Addr, Inst, Tuple2#(Addr,Inst)));
  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 
    
  Unit#(Token, void, Addr, Inst, Tuple2#(Addr,Inst)) mem <- mkIMemory();
  
  let i <- mkFM_Unit(valids, dones, mem,2);

  return i;     
endmodule
                  

//-----------------------------------------------------------------------------------------------------------//
// Decode Unit (in-order _unit_)                                                                             //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkDecode#(BypassUnit#(RName, PRName, Value, Token) b)
           (Unit#(Token, Tuple2#(Addr,Inst), void, DepInfo, Tuple2#(Addr, DecodedInst)));
                 
  FIFO#(Tuple3#(Token,DepInfo,Tuple2#(Addr, DecodedInst)))           f   <- mkFIFO();
                
  method Action request(Token t, Tuple2#(Addr,Inst) tup, void v);
    match {.a,.inst} = tup;

    DepInfo depInfo;
    DecodedInst di;

    case (inst) matches
      tagged IAdd {dest: .rd, src1: .ra, src2: .rb}:
        begin
          let pra = b.lookup1(ra);
          let prb = b.lookup2(rb);
          let rtup <- b.makeMapping(Just(rd), t);
          match {.prd, .oprd} = rtup;
          di =      DAdd {pdest: prd, opdest: oprd, op1: pra, op2: prb};
          depInfo = DepInfo {dest: Just(tuple2(rd, prd)), src1: Just(tuple2(ra, pra)), src2: Just(tuple2(rb,prb))};
        end
      tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
        begin
          let pra = b.lookup1(ra);
          let prb = b.lookup2(rb);
          let rtup <- b.makeMapping(Just(rd), t);
          match {.prd, .oprd} = rtup;
          di =      DSub {pdest: prd, opdest: oprd, op1: pra, op2: prb};
          depInfo = DepInfo {dest: Just(tuple2(rd, prd)), src1: Just(tuple2(ra, pra)), src2: Just(tuple2(rb,prb))};
        end
      tagged IBz {cond: .c , addr:  .addr}:
        begin
          let pra = b.lookup1(c);
          let prb = b.lookup2(addr);
          let rtup <- b.makeMapping(Nothing, t);
          match {.prd, .oprd} = rtup;
          di =      DBz {opdest: oprd, cond: pra, addr: prb};
          depInfo = DepInfo {dest: Nothing, src1: Just(tuple2(c,pra)), src2: Just(tuple2(addr,prb))};
        end
      tagged ILoad {dest: .rd, idx: .ri, offset: .ro}:
        begin
          let pra = b.lookup1(ri);
          let rtup <- b.makeMapping(Just(rd), t);
          match {.prd, .oprd} = rtup;
          di =      DLoad{pdest: prd, opdest: oprd, idx: pra, offset: zeroExtend(ro)};
          depInfo = DepInfo {dest: Just(tuple2(rd,prd)), src1: Just(tuple2(ri,pra)), src2: Nothing};
        end
      tagged ILoadImm {dest: .rd, imm: .i}:
        begin
          let rtup <- b.makeMapping(Just(rd), t);
          match {.prd, .oprd} = rtup;
          di =      DLoadImm {pdest: prd, opdest: oprd, value: signExtend(i)};
          depInfo = DepInfo {dest: Just(tuple2(rd,prd)), src1: Nothing, src2: Nothing};
        end
      tagged IStore {src: .rsrc, idx: .ri, offset: .ro}:
        begin
          let pra = b.lookup1(rsrc);
          let prb = b.lookup2(ri);
	  let rtup <- b.makeMapping(Nothing, t);
          match {.prd, .oprd} = rtup;
          di =      DStore{value: pra, opdest: oprd, idx: prb, offset: zeroExtend(ro)};
          depInfo = DepInfo {dest: Nothing, src1: Just(tuple2(ri,prb)), src2: Just(tuple2(rsrc,pra))};
        end
    endcase
       
    f.enq(tuple3(t,depInfo,tuple2(a,di)));
  endmethod
  
  method ActionValue#(Tuple3#(Token, DepInfo,Tuple2#(Addr, DecodedInst))) response();
    f.deq();
    return f.first(); 
  endmethod
endmodule  


module [Module] mkFM_Decode#(BypassUnit#(RName, PRName, Value, Token) b)(FM_Unit#(Tick, Token,   Tuple2#(Addr,Inst),
                                              void, DepInfo, Tuple2#(Addr,DecodedInst)));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token();
   
  Unit#(Token, Tuple2#(Addr,Inst), void, DepInfo, Tuple2#(Addr, DecodedInst)) dec <- mkDecode(b);
  
  let i <- mkFM_Unit(valids, dones, dec,2);
  return i;     
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Execute Unit (in-order _unit_)                                                                            //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkExecute#(BypassUnit#(RName, PRName, Value, Token) b)
	    (Unit#(Token, Tuple2#(Addr,DecodedInst),void, Maybe#(Addr), ExecedInst));

  FIFO#(Tuple3#(Token,Maybe#(Addr), ExecedInst)) f   <- mkFIFO();
  FIFO#(Tuple3#(Token,Addr, DecodedInst))       iq   <- mkFIFO();

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
                 f.enq(tuple3(t, Nothing, EWB {pdest: prd, opdest: oprd}));
                 iq.deq();
               end
         tagged DSub {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
             if (isJust(mva) && isJust(mvb))
               begin
                 b.write1(prd,unJust(mva) - unJust(mvb));
                 f.enq(tuple3(t, Nothing, EWB {pdest: prd, opdest: oprd}));
                 iq.deq();
               end
         tagged DBz {opdest: .oprd, cond: .c, addr: .a}:
           if (isJust(mva) && unJust(mva) != 0)
             begin
                 f.enq(tuple3(t, Nothing, ENop{opdest: oprd}));
                 iq.deq();                
               end 
             else if (isJust(mva) && isJust(mvb)) // condition must be zero
               begin
                 f.enq(tuple3(t,mvb, ENop{opdest: oprd}));
                 iq.deq();
               end
         tagged DLoad {pdest: .prd, opdest: .oprd, idx: .idx, offset: .o}:
           begin
             f.enq(tuple3(t, Nothing, ELoad {idx: idx, offset: o, pdest:prd, opdest: oprd}));
             iq.deq();
           end
         tagged DLoadImm {pdest: .prd, opdest: .oprd, value: .val}:
           begin
             b.write1(prd, signExtend(val));
             f.enq(tuple3(t, Nothing, EWB {pdest: prd, opdest: oprd}));
             iq.deq();
           end
         tagged DStore {value: .v, opdest: .oprd, idx: .idx, offset: .o}:
           begin
             f.enq(tuple3(t,Nothing,EStore {idx: idx, offset: o, val: v, opdest: oprd}));
             iq.deq();
           end
    endcase
  endrule

  method Action request(Token t, Tuple2#(Addr, DecodedInst) tup, void v);
    match {.addr, .dec} = tup;

    iq.enq(tuple3(t,addr,dec));
  endmethod

  method ActionValue#(Tuple3#(Token,Maybe#(Addr),ExecedInst)) response();
    f.deq();
    return f.first(); 
  endmethod
endmodule  

module [Module] mkFM_Execute#(BypassUnit#(RName, PRName, Value, Token) b)
	    (FM_Unit#(Tick, Token, Tuple2#(Addr,DecodedInst),void, Maybe#(Addr), ExecedInst));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 
  Unit#(Token, Tuple2#(Addr,DecodedInst),void, Maybe#(Addr), ExecedInst) exe <- mkExecute(b);

  let i <- mkFM_Unit(valids, dones, exe,3);
  return i;     
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Memory Unit (in-order _unit_)                                                                             //
//-----------------------------------------------------------------------------------------------------------//

module [Module]  mkDMemory#(BypassUnit#(RName, PRName, Value, Token) b,
	                    Memory#(MemReq, MemResp, Token) dmem)
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
             dmem.request(Ld{addr: unJust(mva) + zeroExtend(o), token: t});
             wResp.enq(tuple3(t,?,EWB{pdest: prd, opdest: oprd}));
             reqs.deq();
           end
      tagged EStore{opdest: .oprd, val: .v, idx: .idx, offset: .o}:
        begin
          let addr = unJust(mva) + zeroExtend(o);
          if (isJust(mva) && isJust(mvb))
             begin
               dmem.request(St{val: unJust(mvb), addr: addr, token: t});
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
          let resp <- dmem.response();
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
          let resp <- dmem.response();
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

  method Action request(Token t, ExecedInst i, void j);
    reqs.enq(tuple2(t,i));
  endmethod
  
  method ActionValue#(Tuple3#(Token, void, ExecedInst)) response();
    f.deq();
    return f.first(); 
  endmethod
endmodule  

module [Module] mkFM_Memory#(BypassUnit#(RName, PRName, Value, Token) b, Memory#(MemReq, MemResp,Token) dmem)(FM_Unit#(Tick, Token, ExecedInst, void, void, ExecedInst));
  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token();
   
  Unit#(Token, ExecedInst, void, void, ExecedInst) mem <- mkDMemory(b,dmem);
  
  let i <- mkFM_Unit(valids, dones, mem, 2);
  return i;     
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Local Commit Unit (in-order _unit_)                                                                             //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkLocalCommit#(BypassUnit#(RName, PRName, Value, Token) b)
                (Unit#(Token, ExecedInst, void, void,void));

  FIFO#(Tuple3#(Token, void, void)) f   <- mkFIFO();
                
  method Action request(Token t, ExecedInst ei, void j);
    f.enq(tuple3(t,?,?)); //? == unit
    PRName p = case (ei) matches
                 tagged ENop    .x: return(x.opdest);
		 tagged EWB     .x: return(x.opdest);
		 tagged ELoad   .x: return(x.opdest);
		 tagged EStore  .x: return(x.opdest);
	       endcase;
    b.freePReg(p);
  endmethod
  
  method ActionValue#(Tuple3#(Token, void, void)) response();
    f.deq();
    return f.first(); 
  endmethod
endmodule  

module [Module] mkFM_LocalCommit#(BypassUnit#(RName, PRName, Value, Token) b)
                   (FM_Unit#(Tick,Token, ExecedInst, void, void,void));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 

  Unit#(Token, ExecedInst, void, void,void) com <- mkLocalCommit(b);
  
  let i <- mkFM_Unit(valids, dones, com,2);
  return i;     
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Global Commit Unit (in-order _unit_)                                                                      //
//-----------------------------------------------------------------------------------------------------------//

module [Module] mkGlobalCommit#(Memory#(MemReq, MemResp,Token) dmem)(Unit#(Token, void, void, void,void));

  FIFO#(Tuple3#(Token, void, void)) f   <- mkFIFO();
                
  method Action request(Token t, void v, void j);
    f.enq(tuple3(t,?,?)); //? == unit
    dmem.commit(t);
  endmethod
  
  method ActionValue#(Tuple3#(Token, void, void)) response();
    f.deq();
    return f.first(); 
  endmethod
endmodule  

module [Module] mkFM_GlobalCommit#(Memory#(MemReq,MemResp,Token) dmem)
                   (FM_Unit#(Tick,Token, void, void, void,void));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 

  Unit#(Token, void, void, void,void) com <- mkGlobalCommit(dmem);
  
  let i <- mkFM_Unit(valids, dones, com,2);
  return i;     
endmodule
                   

//-----------------------------------------------------------------------------------------------------------//
// Top                                                                                                       //
//-----------------------------------------------------------------------------------------------------------//

(* synthesize *)
module [Module] mkBypassUnit(BypassUnit#(RName, PRName, Value, Token))
  provisos(
    Bits#(RName,rsz),
    Bits#(PRName,psz)
    );

/*  

  Vector#(TExp#(rsz), 


  //new old
  method ActionValue#(Tuple2#(PRName,PRName)) makeMapping(Maybe#(RName) x, Token tok); //token is the ref name
    
   return(?);
  endmethod








  method preg_T lookup1(vreg_T v);
  method preg_T lookup2(vreg_T v);

  method Maybe#(value_T) read1(preg_T i);
  method Maybe#(value_T) read2(preg_T i);

  method Action write(preg_T i, value_T v);

  method Action freePReg(preg_T x);
  method Action rewindtoToken(token_T tok);


*/


  return (?);






endmodule











(* synthesize *)
module [Module] mkDMem(Memory#(MemReq, MemResp, Token)) provisos(Bits#(PRName, psz));

  FIFO#(MemResp)        f <- mkFIFO();

  RegFile#(Addr, Value) m <- mkRegFileFull();

  Reg#(Vector#(TExp#(psz), Maybe#(Tuple3#(Token,Addr,Value)))) mtokens <- mkReg(Vector::replicate(Nothing));

  function canStore(v) = !Vector::all(isJust, v);  

  function matchAddr(a,mx);
    case (mx) matches
      tagged Just {.*,.addr,.*}: return (a == addr) ? mx : Nothing;
      tagged Nothing           : return Nothing;
    endcase
  endfunction
 
  method Action request(MemReq req) if (canStore(mtokens));

    function Value getResult(Token youngest, Addr a);
      function youngerToken(Token a, Token b) = (youngest - a) < (youngest - b);

      let mmtokens = Vector::map(matchAddr(a),mtokens);

      function pickYoungest (mta,mtb) = (!isJust(mta)) ? mtb:
	                                (!isJust(mtb)) ? Nothing:
                                        youngerToken((unJust(mta)).fst,(unJust(mtb)).fst)? mta:mtb;
      let finalChoice = Vector::fold(pickYoungest, mmtokens);

      case(finalChoice) matches
        tagged Nothing: return m.sub(a); // goto memory
        tagged Just {.*,.*,.v}: return v;
      endcase
    endfunction

     case (req) matches
       tagged Ld .ld_info:
         begin
           let v = getResult(ld_info.token, ld_info.addr);
           f.enq(LdResp v);
         end
       tagged St .st_info:
         begin
           //Response
           let v = getResult(st_info.token, st_info.addr); // use this as the "old value"
           f.enq(StResp);
           //drop in Buffer
	   
	   let num_mtokens = Vector::zip(mtokens,genVector);

           function choose (a,b) = (!isJust(a.fst)) ? a : b;
/**/ 
           Vector#(TExp#(psz), Maybe#(Tuple3#(Token,Addr,Value))) new_mtokens = newVector();

           Integer i = 0;
           Bool done = False;
           for(i = 0; i < valueOf(TExp#(psz)); i = i + 1)
             begin
               new_mtokens[i] = (done || (isJust(mtokens[i])) ?
                                  mtokens[i]:
                                  Just(tuple3(st_info.token, st_info.addr, st_info.val)));
                  
               if (!isJust(mtokens[i]))
                 done = True;
             end
/**/

           
/*
            match {.*, .index} = Vector::fold(choose,num_mtokens);

           let new_mtokens = Vector::update((mtokens._read),index,
	                                    Just(tuple3(st_info.token, st_info.addr, st_info.val)));
*/

            mtokens <= new_mtokens;

 
         end
     endcase
  endmethod

  method ActionValue#(MemResp) response();
    f.deq();
    return f.first();
  endmethod

  method Action      commit(Token token);
    function matchToken(t,mx)  =
      case (mx) matches
	tagged Just {.tok,.*,.*}: return (t == tok);
        tagged Nothing        : return False;
      endcase;	

    function ff(ma, mb) = matchToken(token,ma) ? ma: mb;
    
    let mresult = fold(ff, mtokens); // the value

    case (mresult) matches
      tagged Just {.*,.addr,.val}:
        m.upd(addr, val);
      tagged Nothing:
        noAction;
    endcase

    function flattenToken(mx) = case (mx) matches
                                   tagged Just {.tok,.*,.*}: return (token == tok) ? Nothing : mx;
                                   tagged Nothing        : return Nothing;
				endcase;

    mtokens <= Vector::map(flattenToken,mtokens);

  endmethod


  method Action        kill(Token lb, Token ub); 

    function flattenToken(mx) = case (mx) matches
                                   tagged Just {.tok,.*,.*}: return (ub-lb > (tok - lb)) ? Nothing: mx;
                                   tagged Nothing        :   return Nothing;
				endcase;

     mtokens <= Vector::map(flattenToken, mtokens);

  endmethod


endmodule

(* synthesize *)
module [Module] mkFM_Test(Empty);

  let b    <- mkBypassUnit();
  let dmem <- mkDMem();
  let fet  <- mkFM_Fetch();
  let dec  <- mkFM_Decode(b);
  let exe  <- mkFM_Execute(b);
  let mem  <- mkFM_Memory(b, dmem);
  let lco  <- mkFM_LocalCommit(b);
  let gco  <- mkFM_GlobalCommit(dmem);

  rule fet_dec(True);
     let x <- fet.getNextFM();
     dec.putPrevFM(x);
  endrule
    
  rule dec_exe(True);
     let x <- dec.getNextFM();
     exe.putPrevFM(x);
  endrule 

  rule exe_mem(True);
     let x <- exe.getNextFM();
     mem.putPrevFM(x);
  endrule 

  rule mem_lco(True);
     let x <- mem.getNextFM();
     lco.putPrevFM(x);
  endrule

  rule lco_gco(True);
     let x <- lco.getNextFM();
     gco.putPrevFM(x);
  endrule
endmodule