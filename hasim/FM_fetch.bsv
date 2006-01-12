
typedef UInt#(64) Token;
typedef UInt#(64) EM_CLK;
typedef Bit#(32)  Addr;
typedef Bit#(64)  Inst;

interface Memory#(type addr, type data);
  method Action request(addr);
  method ActionValue#(data) response();
endinterface



module [Module] mkFM_fetch(FM_Unit#(EM_CLK, void, Token, Addr, Inst, Tuple2#(Token,Inst));

  Memory#(Addr, Inst)  mem <- mkMemory();
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
			   
  method Action                                     putPrevFM(Tuple2#(Token, void)
    match {.tok, .junk} = x;
    tfifo.enq(tok);
    t2fifo.enq(tok);							      
  endmethod

  method ActionValue#(Token)                        putTM(Tuple2#(Addr, EM_CLK) x);
    match {.a, .clk} = x;

    cfifo.enq(clk);
    mem.request(a);
  endmethod
							      
  method ActionValue#(Tuple2#(Inst, Token))     getTM(EM_CLK t);

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

