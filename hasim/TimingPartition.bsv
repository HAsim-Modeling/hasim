import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import Interfaces::*;
import Types::*;


typedef enum
{
  FETCH, DECODE, EXECUTE, MEMORY, LCOMMIT, GCOMMIT
}
  Phase;

typedef struct
{
  Phase phase;
  Tick tick;
  Inst inst;
  DepInfo depinfo;
  InstResult instresult;
}
  InstInfo deriving (Eq, Bits);

(* synthesize *)
module [Module] mkTP_Test();
  
  //Latencies
  Tick fet_L = 1;
  Tick dec_L = 1;
  Tick exec_L = 1;
  Tick mem_L = 1;
  Tick wb_L = 1;
  

  Reg#(Addr) pc <- mkReg(0);
  Reg#(Tick) baseTick <- mkReg(0);
  
  RegFile#(Token, Maybe#(InstInfo)) <- mkRegFileFull();
  

  FunctionalPartition#(Tick, Token,
                       Addr, Inst,
		       void, DepInfo,
		       void, InstResult,
		       void, void,
		       void, void,
		       void, void) func <- mkFP_Test();
 
 
  function InstInfo newInstInfo(Token t);
  
    return InstInfo
           {
	     token: t,
	     tick: ?,
	     inst: ?,
	     depinfo: ?,
	     instresult: ?
	   };
	   
  endfunction
  
  rule tok_req (True);
    func.tokgen.request.put(?, ?, baseTick);
  endrule
  
  rule tok_resp (True);
    match {.tok, .*} <- func.tokgen.response.get();
    newtokens.enq(newInstInfo(tok));
  endrule
  
  rule fet_req (True);
    
    func.fetch.request.put(newtokens.first(), pc, baseTick);
    
    pipe_fet.enq(newtokens.first());
    newtokens.deq();
    
    pc <= pc + 1;
    baseTick <= baseTick + 1;
    
  endrule
  
  rule fet_resp (True);
  
    match {.tok, .inst} <- func.fetch.response.get();
    
    InstInfo ii = pipe_fet.first();
    
    if (ii.token != tok) $display("ERROR: Out of order return in Fetch");

endmodule
