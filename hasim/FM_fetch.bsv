import GetPut::*;
import RegFile::*;
import FIFO::*;
import RWire::*;

import Interfaces::*;


typedef UInt#(64) Token;
typedef UInt#(64) Tick;
typedef Bit#(32)  Addr;
typedef Bit#(5) RName;
typedef Bit#(32) Value;

interface Memory#(type addr_T, type data_T);
  method Action request(addr_T a);
  method ActionValue#(data_T) response();
endinterface



module [Module] mkFM_fetch(FM_Unit#(Tick, void, Token, Addr, Inst, Inst));

  Memory#(Addr, Inst)  mem <- ?; //mkMemory(); //XXX
  FIFO#(Token)       tfifo <- mkSizedFIFO(10); //parameterize
  FIFO#(Token)      t2fifo <- mkSizedFIFO(10); //parameterize
			   
  FIFO#(Tick)      cfifo <- mkSizedFIFO(10);
  FIFO#(Tick)     c2fifo <- mkSizedFIFO(10);

  FIFO#(Inst)        ififo <- mkSizedFIFO(10);
  FIFO#(Inst)       i2fifo <- mkSizedFIFO(10);
			   
  rule getResp(True);
    let i <- mem.response();
    ififo.enq(i);
    i2fifo.enq(i);
  endrule
			   
  method Action                                     putPrevFM(Tuple2#(Token, void) x);
    match {.tok, .junk} = x;
    tfifo.enq(tok);
    t2fifo.enq(tok);							      
  endmethod

  method ActionValue#(Token)                        putTM(Tuple2#(Addr, Tick) x);
    match {.a, .clk} = x;

    cfifo.enq(clk);
    mem.request(a);
    return ?;
  endmethod
							      
  method ActionValue#(Tuple2#(Token, Inst))     getTM(Tick t);

    c2fifo.enq(t);							      

    ififo.deq();			
    tfifo.deq();
		      
    return(tuple2(tfifo.first(), ififo.first()));
  endmethod 							      
							      
  method ActionValue#(Tuple2#(Token, Inst))     getNextFM();
    cfifo.deq();
    c2fifo.deq();
   
    t2fifo.deq();
    i2fifo.deq();
    return (tuple2(t2fifo.first(),i2fifo.first()));
							      
  endmethod							      
							      
				      

 
  method Action                                     killToken(token_T t);
    noAction;
  endmethod
   



endmodule


typedef union tagged
{
 struct {RName dest; RName src1; RName src2;} IAdd;
 struct {RName dest; RName src1; RName src2;} ISub;
 struct {RName cond; RName addr;} IBz;
 struct {RName dest; RName idx; Bit#(5) offset;} ILoad;
 struct {RName dest; Bit#(10) imm;} ILoadImm;
 struct {RName src;  RName idx; Bit#(5) offset;} IStore;
 RName ITerminate;		     //Terminate and return value in RName
}
 Inst deriving (Eq,Bits);
		     

typedef union tagged
{
 struct {RName dest;  Value op1; Value op2;} DAdd;
 struct {RName dest;  Value op1; Value op2;} DSub;
 struct {Value cond;  Addr addr;} DBz;
 struct {RName dest;  Value idx; Value offset;} DLoad;
 struct {RName dest;  Value value;} DLoadImm;
 struct {Value value; Value idx; Value offset;} DStore;
 Value DTerminate;
}
  DecodedInst deriving (Eq,Bits);

/*  
module mkDecoder#(RegFile#(RName, Value) rf) (Tuple2#(Put#(Inst), Get#(DecodedInst)));

   Wire#(Inst) w <- mkWire();
   
   interface Put fst;
     method Action put(Inst i);
       w <= i;
     endmethod
   endinterface
   
   interface Get snd;
     method ActionValue#(DecodedInst) get();
       return decodeInst(w);
     endmethod
   endinterface
   
endmodule
*/

module mkFM_decode#(Tuple2#(Put#(Inst), Get#(DecodedInst)) decoder, 
                    RegFile#(RName, Value) rf)                                    
		                 (FM_Unit#(Tick, Inst, Token, Token, DecodedInst, Tuple2#(Inst, DecodedInst)));

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

  
  RegFile#(Token, Tuple2#(Inst, DecodedInst)) tbl <- mkRegFileFull();
  Wire#(Tuple2#(Token, Tuple2#(Inst, DecodedInst))) next <- mkWire();
  
  
  method Action putPrevFM(Tuple2#(Token, Inst) x);
  
    tbl.upd(x.fst, tuple2(x.snd, decodeInst(x.snd)));
    
  endmethod 
  
  method ActionValue#(Token) putTM(Tuple2#(Token, Tick) t);
    let tok = t.fst;
    next <= tuple2(tok, tbl.sub(tok));
    return tok;
  endmethod
  
  method ActionValue#(Tuple2#(Token, DecodedInst)) getTM(Tick tick);
    match {.t, {.i, .dec}} = next;
    
    return tuple2(t, dec);
    
  endmethod
  
  method ActionValue#(Tuple2#(Token, Tuple2#(Inst, DecodedInst))) getNextFM();
    
    return next;
    
  endmethod
  
  method Action killToken(Token t);
    noAction;
  endmethod
  
endmodule



module mkFM_execute (FM_Unit#(Tick, 
                              Tuple2#(Inst, DecodedInst), 
			      Token, 
			      Token, 
			      Value, 
			      Tuple3#(Inst, DecodedInst, Value)));

   function ActionValue#(Value) execInst(DecodedInst dec);
   actionvalue
     case (dec) matches
       tagged DAdd {dest: .rd, op1: .ra, op2: .rb}:
       begin
	 $display("Add:        R%0d := 0x%0h + 0x%0h", rd, ra, rb);
         return ra + rb;
       end

       tagged DSub {dest: .rd, op1: .ra, op2: .rb}:
       begin
	 $display("Sub:        R%0d := 0x%0h - 0x%0h", rd, ra, rb);
	 return ra - rb;
       end

       tagged DBz  {cond: .c , addr: .a} &&& (c != 0):
       begin
	 $display("Not taken:  GOTO 0x%0h IF 0x%0h == 0", a, c);
	 return 0;
       end

       tagged DBz {cond: .c , addr: .a} &&& (c == 0):
       begin
	 $display("Taken:      GOTO 0x%0h IF 0x%0h == 0", a, c);
	 return 1;
       end

       tagged DLoad {dest: .rd, idx: .idx, offset: .o}:
       begin
	 $display("LoadReq:       R%0d := [0x%0h + 0x%0h]", rd, idx, o);
	 return idx + o;
       end

       tagged DLoadImm {dest: .rd, value: .val}:
       begin
	 $display("LoadImm:    R%0d := 0x%0h", rd, val);
	 return val;
       end

       tagged DStore {value: .v, idx: .idx, offset: .o}:
       begin
	 $display("StoreReq:      [0x%0h + 0x%0h] := 0x%0h", idx, o, v);
	 return idx + o;
       end

       tagged DTerminate .v:
       begin
	 $display("Halt:       0x%0h", v);
	 return v;
       end
       
     endcase
  endactionvalue
  endfunction


  RegFile#(Token, Tuple3#(Inst, DecodedInst, Value)) tbl <- mkRegFileFull();
  Wire#(Tuple2#(Token, Tuple3#(Inst, DecodedInst, Value))) next <- mkWire();


  method Action putPrevFM(Tuple2#(Token, Tuple2#(Inst, DecodedInst)) x);
  
    Value res <- execInst(x.snd.snd);
  
    tbl.upd(x.fst, tuple3(x.snd.fst, x.snd.snd, res));
    
  endmethod 
  
  method ActionValue#(Token) putTM(Tuple2#(Token, Tick) t);
    let tok = t.fst;
    next <= tuple2(tok, tbl.sub(tok));
    return tok;
  endmethod
  
  method ActionValue#(Tuple2#(Token, Value)) getTM(Tick tick);
    match {.t, {.i, .dec, .v}} = next;
    
    return tuple2(t, v);
    
  endmethod
  
  method ActionValue#(Tuple2#(Token, Tuple3#(Inst, DecodedInst, Value))) getNextFM();
    
    return next;
    
  endmethod
  
  method Action killToken(Token t);
    noAction;
  endmethod
  
endmodule

