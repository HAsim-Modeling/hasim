//BSV library imports
import FIFO::*;

//HASim library imports
import HASim::*;
import FunctionalPartitionBase::*;

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


module [Module] mkFUNCP_Stage_TOK
    //interface:
                (FP_Stage#(tick_T,   //Tick type
		           token_T,  //Token type
			   void,     //Type from previous stage
			   void,     //Request Type
			   void,     //Response Type
			   void))    //Type to next stage
        provisos
	        (Bits#(token_T, token_SZ),
		 Bounded#(token_T),
		 Arith#(token_T),
		 Bits#(tick_T, tick_SZ),
		 Eq#(token_T));
		         

  Reg#(token_T) r_first <- mkReg(minBound);
  Reg#(token_T) r_free <- mkReg(minBound);
  
  //Killing tokens can never result in free tokens becoming taken.
  //Therefore we never need to worry about the responses being invalid.
  FIFO#(token_T) respQ <- mkFIFO();
  FIFO#(token_T) nextQ <- mkBypassFIFO();
 
  //From Previous Stage (Global Commit)
  
  interface Put in;
           
    method Action put(Tuple2#(token_T, void) tup);
    
      match {.t, .*} = tup;
      
      //complete token_T t
      
      if (r_first != t) 
        $display("TGen ERROR: token_Ts completing out of order");
     
      r_first <= r_first + 1;
      
    endmethod
  endinterface

  interface Server server;

    //From Timing Partition
  
    interface Put request;
  
      method Action put(Tuple3#(token_T, void, tick_T) tup);
       
        match {.*, .*, .tick} = tup;
        
        //allocate a new token
        respQ.enq(r_free);
        nextQ.enq(r_free);
        r_free <= r_free + 1;

      endmethod

    endinterface

    //To Timing Partition

    interface Get response;

      method ActionValue#(Tuple2#(token_T, void)) get();

        //return allocated token
	respQ.deq();
	return tuple2(respQ.first(), ?);
      endmethod 

    endinterface
    
  endinterface
  
  //To Next Stage (Fetch)
  
  interface Get out;

    method ActionValue#(Tuple2#(token_T, void)) get();
      nextQ.deq();
      return tuple2(nextQ.first, ?); //This Does Not Exist.
    endmethod

  endinterface

  //killToken

  method Action killToken(token_T tok);
    //free tok and all tokens after it
    r_free <= tok;
  endmethod
  
endmodule
