//BSV library imports
import FIFO::*;
import BypassFIFO::*;
import GetPut::*;
import ClientServer::*;

//HASim library imports
import HASim::*;
import FUNCP_Base::*;

//HASim model-specific imports
import ISA::*;

//-------------------------------------------------------------------------//
// Token Generation Unit                                                   //
//-------------------------------------------------------------------------//

// The first (and last) stage in the Functional Partition pipeline. 

// The Timing Partition uses it as the first stage to generate a token to
// flow through the pipeline. This module is pretty general and can be reused
// across many functional partitions

// It's also the last stage because Global Commit should report back when
// a token has completed and can be reused.

// TODO: This could be a bit smarter. Currently it assumes that tokens commit
// inorder. Actually, this is probably a pretty reasonable assumption.

module [HASim_Module] mkFUNCP_Stage_TOK ();
  
  RegFile#(TokIndex, Bool) valids <- mkReg(minBound);
  Reg#(TokIndex) next <- mkReg(0);
  Reg#(Bool) nextValid <- mkReg(False);
  
  
  //Links
  Connection_Server#(Tuple2#(Bit#(8), Tick), Token)
  //...
  link_from_tp <- mkConnection_Server("fp_tok");
  
  Connection_Receive#(Tuple2#(Token, void))
  //...
  link_from_prev <- mkConnection_Receive("gco_to_tok");
  
  Connection_Send#(Tuple2#(Token, void))
  //...
  link_to_next <- mkConnection_Send("tok_to_fet");
  
  Connection_Receive#(Token)
  //...
  link_killToken <- mkConnection_Receive("tok_kill");


  //handleReq
  
  rule handleReq (nextValid);
  
    match {.x, .tick} <- link_from_tp.getReq();

    if (x == 17)
    begin
    //allocate a new token
    let tok = Token
      {
	index: valids.sub(next),
	info: ?
      };
    link_from_tp.makeResp(tok);
    link_to_next.send(tuple2(tok, ?));
    end
    nextValid <= valids.sub(next + 1);
    next <= next + 1;
  endrule
  
  //calcNext
  
  rule calcNext (!nextValid);
  
    if (valids.sub(next))
      nextValid <= True;
    else
      next <= next + 1;
  
  endrule
 
  //recycle
 
  rule recycle (True);

    match {.t, .*} <- link_from_prev.receive();

    //complete token t
    if (!valids.sub(t.idx))
      $display("Tokgen error. Completing unallocated token %h", t);
      
    valids.upd(t.idx, False);

  endrule
   
  //killToken
  
  rule killToken (True);
  
    let tok <- link_killToken.receive();
  
    if (!valids.sub(t.idx))
      $display("Tokgen error. Killing unallocated token %h", t);
      
    valids.upd(t.idx, False);
  
  endrule
  
endmodule
