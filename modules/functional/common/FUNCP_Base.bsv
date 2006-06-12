import HASim::*;
import BypassFIFO::*;

import ISA::*;

import Connectable::*;
import GetPut::*;
import ClientServer::*;
import FIFO::*;
import RegFile::*;

//************ Functional Partition Stage Interface ************//

// This is a generalized wrapper which wraps around a stage in the functional
// partition. It handles interfacing that stage with the next stage,
// the previous stage, and decodes Timing Partition requests.

//              Functional                :             Timing
//                                        :
//                 req +----------+       :  req    +---------------+
//     +---------+<====|          |<================|		    |
//     | FP_Unit |     | FP_Stage |       :         |		    |       
//     +---------+====>|          |================>|		    |
//                resp +----------+       :  resp   |		    |
//                          |             :         |		    |
//                          V next        :         |		    |
//                 req +----------+       :  req    |	Timing 	    |
//     +---------+<====|          |<================|	Partition   |
//     | FP_Unit |     | FP_Stage |       :         |		    |
//     +---------+====>|          |================>|		    |
//                resp +----------+       :  resp   |               |
//                          |                       |               |
//                          V                       |               |

interface FP_Stage#(type init_T,
		    type req_T,
		    type resp_T,
		    type next_T);

  interface Put#(Tuple2#(Token, init_T)) in;
  
  interface Server#(Tuple3#(Token, req_T, Tick), Tuple2#(Token, resp_T)) server;
 
  interface Get#(Tuple2#(Token, next_T)) out;
 
  method Action killToken(Token t);

endinterface


/************* Functional Partition Unit Interface *************/

// A Unit is a bundled computation. It can be multicycle, and out-of-order.
// Standard units like Decode and Execute can be wrapped by the above FP_Stage.

//  Token    DataFromPrevStage     RequestFromTP
//    |              |  	      |
//    V              V  	      V
//  Token    DataForNextStage      ResponseToTP


typedef Server#(Tuple3#(Token, init_T, req_T), 
                Tuple3#(Token, resp_T, next_T))
        FUNCP_Alg#(type init_T, 
		   type req_T, 
		   type resp_T, 
		   type next_T);

/************* Bypass Unit Interface *************/

interface BypassUnit;
		      
  // first is new pointer, second is old. if no new pointer, the first will be undefined
  method ActionValue#(Tuple2#(PRName,PRName)) makeMapping(Maybe#(RName) x, Token tok, Bool snapshot); //token is the ref name
  method PRName lookup1(RName v);
  method PRName lookup2(RName v);

  method Maybe#(Value) read1(PRName i);
  method Maybe#(Value) read2(PRName i);
  method Maybe#(Value) read3(PRName i);
  method Maybe#(Value) read4(PRName i);

  method Action write1(PRName i, Value v);
  method Action write2(PRName i, Value v);

  method Action freePReg(Token tok, PRName x);
  method Action rewindtoToken(Token tok); // exception
endinterface

                 
//---------------------------------------------------------------------//
// General FP_Stage                                                    //
//---------------------------------------------------------------------//

// A generalized stage module. Wraps the module in a generalized 
// request/response framework that decodes the requests from the Timing
// Partition and sends them on to the associated FP_Unit for processing.

// Essentially this module adds a table of data from the previous stage,
// and a FIFO of requests/responses.

// TODO: This model currently returns responses to the timing partition
// in the order the FP_Unit returns them. For out-of-order FP_Units a
// FP_Stage with a completion buffer may be desirable.

// mkFP_Stage :: StageName -> FP_Unit -> TableSize -> FP_Stage

interface FUNCP_Stage#(type init_T,
		       type req_T,
		       type resp_T,
		       type next_T);

endinterface

module [Connected_Module] mkFUNCP_Stage#(String stagename,
                                         String linkname, 
                                         String servername,
				         String prevname,
				         String nextname,
				         Integer sz) 
    //interface:
               (FUNCP_Stage#(init_T, 
			     req_T, 
			     resp_T, 
			     next_T))
        provisos
          (Bits#(init_T, init_SZ),
           Bits#(req_T, req_SZ),
           Bits#(resp_T, resp_SZ),
           Bits#(next_T, next_SZ),
	   Transmittable#(Tuple3#(Token, init_T, req_T)),
	   Transmittable#(Tuple3#(Token, resp_T, next_T)),
	   Transmittable#(Tuple3#(Token, Tick, req_T)),
	   Transmittable#(Tuple2#(Token, resp_T)),
	   Transmittable#(Tuple2#(Token, init_T)),
	   Transmittable#(Tuple2#(Token, next_T)),
	   Transmittable#(Token));

  //Local definitions
  Token tableMin = minBound;
  Token tableMax = maxBound; //fromInteger(sz - 1);

  //Links
  Connection_Client#(Tuple3#(Token, init_T, req_T),
                     Tuple3#(Token, resp_T, next_T))
  //...
  link_to_unit   <- mkConnection_Client(linkname);
  
  Connection_Server#(Tuple3#(Token, Tick, req_T),
                     Tuple2#(Token, resp_T))
  //...
  link_from_tp   <- mkConnection_Server(servername);
  
  Connection_Receive#(Tuple2#(Token, init_T))
  //...
  link_from_prev <- mkConnection_Receive(prevname);
  
  Connection_Send#(Tuple2#(Token, next_T))
  //...
  link_to_next   <- mkConnection_Send(nextname);
  
  //Connection_Receive#(Token)
  //...
  //link_killToken <- mkConnection_Receive("fp_killToken");

  		
  //SRAM tables
  RegFile#(Token, init_T) values <- mkRegFile(tableMin, tableMax);
  RegFile#(Token, Bool)   valids <- mkRegFile(tableMin, tableMax); 
  RegFile#(Token, Bool)   dones  <- mkRegFile(tableMin, tableMax); 

  //Rules
  
  //insert

  rule insert (True);
  
    match {.tok,.iVal} <- link_from_prev.receive();
    
    Bool valid = valids.sub(tok);
    
    if (valid)
      begin        
	$display("%s ERROR: reinserting allocated token %h", stagename, tok);
      end
    else
      begin
	//Set valid to true and done to false
	valids.upd(tok,True);
	dones.upd(tok,False);
	values.upd(tok, iVal);
      end
  
  endrule


  //handleReq
  
  rule handleReq (True);

    match {.tok, .tick, .req} <- link_from_tp.getReq();
   
    Bool done   =  dones.sub(tok);
    Bool valid  =  valids.sub(tok);  

    init_T iVal = values.sub(tok);

    if (!valid)
       $display("%s ERROR: requesting unallocated token %h", stagename, tok);
     else if (done)
       $display("%s ERROR: re-requesting finished token %h", stagename, tok);            
     else // !done
       link_to_unit.makeReq(tuple3(tok, iVal, req));
  endrule

  //getResponse
  
  rule getResponse (True);
  
    match {.tok, .resp, .next} <- link_to_unit.getResp();
    
    Bool valid = valids.sub(tok);
    
    if (valid) // don't insert if it was killed
      begin
        dones.upd(tok, True);
	link_from_tp.makeResp(tuple2(tok, resp));
	link_to_next.send(tuple2(tok, next));
      end
      
  endrule
  
  //killToken
  
  //rule killToken (True);
    
    //let tok <- link_killToken.receive();
  
    //valids.upd(tok, False);
  
  //endrule

endmodule


