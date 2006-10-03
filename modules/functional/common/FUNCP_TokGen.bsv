//BSV library imports
import FIFO::*;
import GetPut::*;
import ClientServer::*;
import RegFile::*;

//HASim library imports

import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_funcp_base::*;

//HASim model-specific imports
import hasim_isa::*;

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
  
  BRAM#(TokIndex, Bool) valids <- mkBRAM_Full();
  Reg#(TokIndex) next <- mkReg(0);
  Reg#(Bool)    ready <- mkReg(True);
  
  
  //Links
  Connection_Server#(Bit#(8), Token)
  //...
  link_from_tp <- mkConnection_Server("fp_tok");
  
  Connection_Receive#(Tuple2#(Token, void))
  //...
  link_from_prev <- mkConnection_Receive("fp_gco_to_tok");
  
  Connection_Send#(Tuple2#(Token, void))
  //...
  link_to_next <- mkConnection_Send("fp_tok_to_fet");
  
  Connection_Receive#(Token)
  //...
  link_killToken <- mkConnection_Receive("fp_tok_kill");


  //handleReq
  
  rule handleReq (ready);
  
    //bug workaround
    let x <- link_from_tp.getReq();

    if (x == 17)
    begin
    //allocate a new token
    let tok = Token
      {
	index: next,
	info: ?
      };
    link_from_tp.makeResp(tok);
    link_to_next.send(tuple2(tok, ?));
    valids.upd(next, True);
    let n = next + 1;
    valids.read_req(n);
    next <= n;
    ready <= False;
    end
  endrule
  
  //calcNext

  rule calcNext (!ready);
  
    Bool isFree <- valids.read_resp();
    if (!isFree)
      ready <= True;
    else
    begin
      let n = next + 1;
      next <= n;
      valids.read_req(n);
    end
  
  endrule
  
 
  //recycle
 
  rule recycle (True);

    //complete token t
    match {.t, .*} <- link_from_prev.receive();

    //Bool isFree = valids.sub(t.index);
    //if (!isFree)
      //$display("Tokgen error. Completing unallocated token %h", t);
      
    valids.upd(t.index, False);

  endrule
   
  //killToken
  
  rule killToken (True);
  
    let tok <- link_killToken.receive();
  
    //Bool isFree = valids.sub(tok.index);
  
    //if (!isFree)
      //$display("Tokgen error. Killing unallocated token %h", tok);
      
    valids.upd(tok.index, False);
  
  endrule
  
endmodule
