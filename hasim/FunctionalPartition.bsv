import GetPut::*;
import RegFile::*;
import FIFO::*;
import RWire::*;
//import Monad::*;
//import List::*;
import Vector::*;

import Interfaces::*;
import Primitive::*;
import ValueVector::*;

typedef UInt#(8) Token;
typedef UInt#(64) Tick;
typedef Bit#(32)  Addr;
typedef Bit#(5)   RName;
typedef Bit#(32)  Value;

typedef union tagged {
 struct {RName dest; RName src1; RName src2;}    IAdd;
 struct {RName dest; RName src1; RName src2;}    ISub;
 struct {RName cond; RName addr;}                IBz;
 struct {RName dest; RName idx; Bit#(5) offset;} ILoad;
 struct {RName dest; Bit#(10) imm;}              ILoadImm;
 struct {RName src;  RName idx; Bit#(5) offset;} IStore;
 RName                                           ITerminate;		     //Terminate and return value in RName
}
 Inst deriving (Eq,Bits);
		     
typedef union tagged {
 struct {RName dest;  Value op1; Value op2;} DAdd;
 struct {RName dest;  Value op1; Value op2;} DSub;
 struct {Value cond;  Addr addr;} DBz;
 struct {RName dest;  Value idx; Value offset;} DLoad;
 struct {RName dest;  Value value;} DLoadImm;
 struct {Value value; Value idx; Value offset;} DStore;
 Value DTerminate;
}
  DecodedInst deriving (Eq,Bits);

typedef struct {
  Maybe#(RName) dest;
  Maybe#(RName) src1;		
  Maybe#(RName) src2;
}
  DepInfo deriving(Eq,Bits);

	
typedef union tagged {
 struct {RName dest;  Value val;}   EWB;
 void                               ENop;
 struct {RName dest;  Value addr;}  ELoad;
 struct {Value val;   Value addr;}  EStore;
}
  ExecedInst deriving (Eq,Bits);

//-----------------------------------------------------------------------------------------------------------//
// General Unit (in-order unit)                                                                              //
//-----------------------------------------------------------------------------------------------------------//


   
		 
//-----------------------------------------------------------------------------------------------------------//
// General Unit (in-order unit)                                                                              //
//-----------------------------------------------------------------------------------------------------------//

module mkFM_Unit#(ValueVector#(Bool, token_T) valids,
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

  token_T tminT = minBound;
  token_T tmaxT = maxBound;
   
  Integer minT = primBitToInteger(pack(tminT)) + 1;
  Integer maxT = primBitToInteger(pack(tmaxT)) + 1;

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
// RegisterFile                                                                                              //
//-----------------------------------------------------------------------------------------------------------//

typedef RegFile#(RName, Value) RegisterFile;

(* synthesize *)
module mkRegisterFile(RegisterFile);
  let r <- mkRegFileFull();
   
  return r;
endmodule

//-----------------------------------------------------------------------------------------------------------//
// BoolVector                                                                                                //
//-----------------------------------------------------------------------------------------------------------//

(* synthesize *)
module mkBoolVector_Token(ValueVector#(Bool, Token));

  let v <- mkBoolVector();
   
  return v;
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Fetch Unit (in-order _unit_)                                                                              //
//-----------------------------------------------------------------------------------------------------------//

(* synthesize *)
module mkIMemory(Unit#(Token, void, Addr, Inst, Tuple2#(Addr,Inst)));
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

/* synthesize */
module mkFM_Fetch(FM_Unit#(Tick,Token, void, Addr, Inst, Tuple2#(Addr,Inst)));
  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 
    
  Unit#(Token, void, Addr, Inst, Tuple2#(Addr,Inst)) mem <- mkIMemory();
  
  let i <- mkFM_Unit(valids, dones, mem,2);

  return i;	
endmodule
		  

//-----------------------------------------------------------------------------------------------------------//
// Decode Unit (in-order _unit_)                                                                             //
//-----------------------------------------------------------------------------------------------------------//

module mkDecode#(RegisterFile rf)
           (Unit#(Token, Tuple2#(Addr,Inst), void, DepInfo, Tuple2#(Addr, DecodedInst)));

  function DecodedInst decodeInst(Inst inst) =
    case (inst) matches
      tagged IAdd {dest: .rd, src1: .ra, src2: .rb}:
	return DAdd {dest: rd, op1: rf.sub(ra), op2: rf.sub(rb)};
      tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
	return DSub {dest: rd, op1: rf.sub(ra), op2: rf.sub(rb)};
      tagged IBz {cond: .c , addr:  .a}:
	return DBz {cond: rf.sub(c) , addr: rf.sub(a)};
      tagged ILoad {dest: .rd, idx: .ri, offset: .ro}:
	return DLoad{dest: rd, idx: rf.sub(ri), offset: zeroExtend(ro)};
      tagged ILoadImm {dest: .rd, imm: .i}:
	return DLoadImm {dest: rd, value: signExtend(i)};
      tagged IStore {src: .rsrc, idx: .ri, offset: .ro}:
	return DStore{value: rf.sub(rsrc), idx: rf.sub(ri), offset: zeroExtend(ro)};
      tagged ITerminate .rsrc:
	return DTerminate rf.sub(rsrc);
    endcase;

  function DepInfo getDepInfo(Inst inst) =
    case (inst) matches
      tagged IAdd {dest: .rd, src1: .ra, src2: .rb}:
	return DepInfo {dest: Just(rd), src1: Just(ra), src2: Just(rb)};
      tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
	return DepInfo {dest: Just(rd), src1: Just(ra), src2: Just(rb)};
      tagged IBz {cond: .c , addr:  .a}:
        return  DepInfo {dest: Nothing, src1: Just(c), src2: Just(a)};
      tagged ILoad {dest: .rd, idx: .ri, offset: .ro}:
	return DepInfo {dest: Just(rd), src1: Just(ri), src2: Nothing}; 
      tagged ILoadImm {dest: .rd, imm: .i}:
	return  DepInfo {dest: Just(rd), src1: Nothing, src2: Nothing};
      tagged IStore {src: .rsrc, idx: .ri, offset: .ro}:
	return  DepInfo {dest: Nothing, src1: Just(ri), src2: Just(rsrc)}; // better sim hw
      tagged ITerminate .rsrc:
	return  DepInfo {dest: Nothing, src1: Just(rsrc), src2: Nothing};
    endcase; // case(inst)
		 
  FIFO#(Tuple3#(Token,DepInfo,Tuple2#(Addr, DecodedInst)))           f   <- mkFIFO();
		
  method Action request(Token t, Tuple2#(Addr,Inst) tup, void v);
    match {.a,.i} = tup;
	
    f.enq(tuple3(t,getDepInfo(i),tuple2(a,decodeInst(i))));
  endmethod
  
  method ActionValue#(Tuple3#(Token, DepInfo,Tuple2#(Addr, DecodedInst))) response();
    f.deq();
    return f.first(); 
  endmethod
endmodule  


module mkFM_Decode#(RegisterFile rf)(FM_Unit#(Tick, Token,   Tuple2#(Addr,Inst),
                                              void, DepInfo, Tuple2#(Addr,DecodedInst)));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token();
   
  Unit#(Token, Tuple2#(Addr,Inst), void, DepInfo, Tuple2#(Addr, DecodedInst)) dec <- mkDecode(rf);
  
  let i <- mkFM_Unit(valids, dones, dec,2);
  return i;	
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Execute Unit (in-order _unit_)                                                                            //
//-----------------------------------------------------------------------------------------------------------//

(* synthesize *)
module mkExecute(Unit#(Token, Tuple2#(Addr,DecodedInst),void, Maybe#(Addr), ExecedInst));
  FIFO#(Tuple3#(Token,Maybe#(Addr), ExecedInst)) f   <- mkFIFO();

  method Action request(Token t, Tuple2#(Addr, DecodedInst) tup, void v);
     match {.addr, .dec} = tup;

     ExecedInst ei =  
       case (dec) matches
         tagged DAdd {dest: .rd, op1: .ra, op2: .rb}:
           return EWB {dest: rd, val: (ra + rb)};
         tagged DSub {dest: .rd, op1: .ra, op2: .rb}:
      	   return EWB {dest: rd, val: (ra - rb)};
         tagged DBz  {cond: .c , addr: .a}:
           return ENop;
         tagged DLoad {dest: .rd, idx: .idx, offset: .o}:
           return ELoad {dest: rd, addr: idx + o};
         tagged DLoadImm {dest: .rd, value: .val}:
	   return EWB {dest: rd, val: val};
         tagged DStore {value: .v, idx: .idx, offset: .o}:
	   return EStore {val: v, addr: idx+ o};
         tagged DTerminate .v:
           return ENop;
       endcase;

    Maybe#(Addr) branchResult =
       case (dec) matches
	 tagged DBz {cond: .c, addr: .a} &&& (c == 0):
           return Just(a);
	 default:
           return Nothing;
       endcase;

    f.enq(tuple3(t,branchResult, ei));
  endmethod

  method ActionValue#(Tuple3#(Token,Maybe#(Addr),ExecedInst)) response();
    f.deq();
    return f.first(); 
  endmethod
endmodule  

(* synthesize *)
module mkFM_Execute(FM_Unit#(Tick, Token, Tuple2#(Addr,DecodedInst),void, Maybe#(Addr), ExecedInst));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 
  Unit#(Token, Tuple2#(Addr,DecodedInst),void, Maybe#(Addr), ExecedInst) exe <- mkExecute();

  let i <- mkFM_Unit(valids, dones, exe,2);
  return i;	
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Memory Unit (in-order _unit_)                                                                             //
//-----------------------------------------------------------------------------------------------------------//

(* synthesize *)
module mkDMemory(Unit#(Token, ExecedInst, void, void, ExecedInst));
  RegFile#(Addr, Value)                    m   <- mkRegFileFull();
  FIFO#(Tuple3#(Token, void, ExecedInst))  f   <- mkFIFO();
		
  method Action request(Token t, ExecedInst i, void j);
    case (i) matches
      tagged ELoad {dest: .rd, addr: .a}:
	f.enq(tuple3(t,?,EWB{dest:rd, val: m.sub(a)}));
      tagged EStore{val: .v,   addr: .a}:
        begin
          f.enq(tuple3(t,?,ENop));
    	  m.upd(a, v);
	end
     default:
       f.enq(tuple3(t,?,i));
    endcase
  endmethod
  
  method ActionValue#(Tuple3#(Token, void, ExecedInst)) response();
    f.deq();
    return f.first(); 
  endmethod
endmodule  

(* synthesize *)
module mkFM_Memory(FM_Unit#(Tick,Token, ExecedInst, void, void, ExecedInst));
  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token();
   
  Unit#(Token, ExecedInst, void, void, ExecedInst) mem <- mkDMemory();
  
  let i <- mkFM_Unit(valids, dones, mem, 2);
  return i;	
endmodule

//-----------------------------------------------------------------------------------------------------------//
// Commit Unit (in-order _unit_)                                                                             //
//-----------------------------------------------------------------------------------------------------------//

module mkCommit#(RegisterFile rf)
                (Unit#(Token, ExecedInst, void, void,void));

  FIFO#(Tuple3#(Token, void, void)) f   <- mkFIFO();
		
  method Action request(Token t, ExecedInst ei, void j);
    f.enq(tuple3(t,?,?)); //? == unit

    case(ei) matches
      tagged EWB {dest: .rd, val: .v}:
        rf.upd(rd,v);
      default:
        noAction;
    endcase
  endmethod
  
  method ActionValue#(Tuple3#(Token, void, void)) response();
    f.deq();
    return f.first(); 
  endmethod
endmodule  

module mkFM_Commit#(RegisterFile rf)
                   (FM_Unit#(Tick,Token, ExecedInst, void, void,void));

  let valids <- mkBoolVector_Token();
  let dones  <- mkBoolVector_Token(); 

  Unit#(Token, ExecedInst, void, void,void) com <- mkCommit(rf);
  
  let i <- mkFM_Unit(valids, dones, com,2);
  return i;	
endmodule
		    






