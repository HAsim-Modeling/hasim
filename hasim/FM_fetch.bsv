import GetPut::*;
import RegFile::*;
import FIFO::*;
import RWire::*;

import Interfaces::*;


typedef UInt#(64) Token;
typedef UInt#(64) EM_CLK;
typedef Bit#(32)  Addr;
typedef Bit#(5) RName;
typedef Bit#(32) Value;

interface Memory#(type addr_T, type data_T);
  method Action request(addr_T a);
  method ActionValue#(data_T) response();
endinterface



module [Module] mkFM_fetch(FM_Unit#(EM_CLK, void, Token, Addr, Inst, Inst));

  Memory#(Addr, Inst)  mem <- ?; //mkMemory(); //XXX
  FIFO#(Token)       tfifo <- mkSizedFIFO(10); //parameterize
  FIFO#(Token)      t2fifo <- mkSizedFIFO(10); //parameterize
			   
  FIFO#(EM_CLK)      cfifo <- mkSizedFIFO(10);
  FIFO#(EM_CLK)     c2fifo <- mkSizedFIFO(10);

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

  method ActionValue#(Token)                        putTM(Tuple2#(Addr, EM_CLK) x);
    match {.a, .clk} = x;

    cfifo.enq(clk);
    mem.request(a);
    return ?;
  endmethod
							      
  method ActionValue#(Tuple2#(Inst, Token))     getTM(EM_CLK t);

    c2fifo.enq(t);							      

    ififo.deq();			
    tfifo.deq();
		      
    return(tuple2(ififo.first(), tfifo.first()));
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

  
module mkDecoder#(RegFile#(RName, Value) rf) (Tuple2#(Put#(Inst), Get#(DecodedInst)));

   Wire#(Inst) w <- mkWire();
   
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

/*
module mkFM_decode#(Tuple2#(Put#(Inst), Get#(DecodedInst)) decoder, 
                    RegFile rf)                                    
		                 (FM_Unit#(EM_CLK, Inst, Token, Token));

  

   
  method Action putPrevFM(Tuple2#(Token, Inst) x);
  
    tbl.upd(x._1, decodeInst(x._2));
    
  endmethod 
  
  method ActionValue#(Token) putTM(Tuple2#(Token, EM_CLK) t);
    let tok = t._1;
    next.wset(tuple2(tok, tbl.sub(tok)));
    return tok;
  endmethod
  
  method ActionValue#(Tuple2#(Inst, Token)) getTM(EM_CLK tick);
    match {.t, .i, .dec} = next.wget();
    
    return tuple2(t, i);
  endmethod
  
  method ActionValue#(Tuple2#(Token, DecodedInst)) getNextFM();
    return ?;
  endmethod
  
  method Action killToken(Token t);
    noAction;
  endmethod
  
endmodule
*/
