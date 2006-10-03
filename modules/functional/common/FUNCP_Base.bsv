import Connectable::*;
import GetPut::*;
import ClientServer::*;
import FIFO::*;
import RegFile::*;

import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_isa::*;


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
				         String killname) 
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
	   Transmittable#(Tuple2#(Token, req_T)),
	   Transmittable#(Tuple2#(Token, resp_T)),
	   Transmittable#(Tuple2#(Token, init_T)),
	   Transmittable#(Tuple2#(Token, next_T)),
	   Transmittable#(Token));


  //Links
  Connection_Client#(Tuple3#(Token, init_T, req_T),
                     Tuple3#(Token, resp_T, next_T))
  //...
  link_to_unit   <- mkConnection_Client(linkname);
  
  Connection_Server#(Tuple2#(Token, req_T),
                     Tuple2#(Token, resp_T))
  //...
  link_from_tp   <- mkConnection_Server(servername);
  
  Connection_Receive#(Tuple2#(Token, init_T))
  //...
  link_from_prev <- mkConnection_Receive(prevname);
  
  Connection_Send#(Tuple2#(Token, next_T))
  //...
  link_to_next   <- mkConnection_Send(nextname);
  
  Connection_Receive#(Token)
  //...
  link_killToken <- mkConnection_Receive(killname);

  		
  //BRAM tables
  BRAM#(TokIndex, init_T)     values <- mkBRAM_Full();
  BRAM#(TokIndex, TokInfo)     infos <- mkBRAM_Full();
  BRAM#(TokIndex, Bool)       valids <- mkBRAM_Full();
  BRAM#(TokIndex, Bool)    starteds  <- mkBRAM_Full(); 

  //FIFOs
  //FIFO#(Tuple2#(Token, init_T))         insertQ <- mkFIFO();
  FIFO#(Tuple2#(Token, req_T))          reqQ    <- mkFIFO();
  FIFO#(Tuple3#(Token, resp_T, next_T)) respQ   <- mkFIFO();
  
  //Rules
  
  //insert
  (* descending_urgency = "killToken, resp_finish, req_finish, insert" *)
  
  rule insert (True);
  
    match {.tok, .iVal} <- link_from_prev.receive();
    
    //Note: We avoid a correctness check to reduce latency
    
    //Set valid to true and started to false
    valids.upd(tok.index, True);
    starteds.upd(tok.index, False);
    values.upd(tok.index, iVal);
    infos.upd(tok.index, tok.info);
    
  endrule


  //handleReq
  
  rule req_start (True);

    match {.tok, .req} <- link_from_tp.getReq();
   
    starteds.read_req(tok.index);
    valids.read_req(tok.index);
    values.read_req(tok.index);
    
    reqQ.enq(tuple2(tok, req));
  
  endrule
    
  rule req_finish (True);
   
    match {.tok, .req} = reqQ.first();
    reqQ.deq();
   
    Bool started   <- starteds.read_resp();
    Bool valid     <-   valids.read_resp();  

    init_T iVal    <-   values.read_resp();

    if (!valid)
       $display("%s ERROR: requesting unallocated token %h", stagename, tok.index);
     else if (started)
       $display("%s ERROR: re-requesting token %h", stagename, tok.index);            
     else
     begin
       link_to_unit.makeReq(tuple3(tok, iVal, req));
       starteds.upd(tok.index, True);
    end
    
  endrule

  //getResponse
  
  rule resp_start (True);
  
    match {.tok, .resp, .next} <- link_to_unit.getResp();
    
    valids.read_req3(tok.index);
    
    respQ.enq(tuple3(tok, resp, next));
  
  endrule
  
  rule resp_finish (True);
    
    Bool valid <- valids.read_resp3();
    match {.tok, .resp, .next} = respQ.first();
    respQ.deq();
    
    if (valid) // don't insert if it was killed
      begin
        valids.upd(tok.index, False);
	link_from_tp.makeResp(tuple2(tok, resp));
	link_to_next.send(tuple2(tok, next));
      end
      
  endrule
  
  //killToken
  
  rule killToken (True);
    
    let tok <- link_killToken.receive();
  
    valids.upd(tok.index, False);
  
  endrule

endmodule


