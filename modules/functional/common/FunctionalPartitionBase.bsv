import HASim::*;
import BypassFIFO::*;
import Mem::*;

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

interface FP_Stage#(type tick_T, 
                    type token_T,
		    type init_T,
		    type req_T,
		    type resp_T,
		    type next_T);

  interface Put#(Tuple2#(token_T, init_T)) in;
  
  interface Server#(Tuple3#(token_T, req_T, tick_T), Tuple2#(token_T, resp_T)) server;
 
  interface Get#(Tuple2#(token_T, next_T)) out;
 
  method Action killToken(token_T t);

endinterface


/************* Functional Partition Unit Interface *************/

// A Unit is a bundled computation. It can be multicycle, and out-of-order.
// Standard units like Decode and Execute can be wrapped by the above FP_Stage.

//  Token    DataFromPrevStage     RequestFromTP
//    |              |  	      |
//    V              V  	      V
//  Token    DataForNextStage      ResponseToTP


typedef Server#(Tuple3#(token_T, init_T, req_T), 
                Tuple3#(token_T, resp_T, next_T))
        FP_Unit#(type token_T, 
		 type init_T, 
		 type req_T, 
		 type resp_T, 
		 type next_T);

/************* Bypass Unit Interface *************/

interface BypassUnit#(type vreg_T, 
                      type preg_T, 
		      type value_T, 
		      type token_T,
		      type shapshotptr_T);
		      
  // first is new pointer, second is old. if no new pointer, the first will be undefined
  method ActionValue#(Tuple2#(preg_T,preg_T)) makeMapping(Maybe#(vreg_T) x, token_T tok,Bool snapshot); //token is the ref name
  method preg_T lookup1(vreg_T v);
  method preg_T lookup2(vreg_T v);

  method Maybe#(value_T) read1(preg_T i);
  method Maybe#(value_T) read2(preg_T i);
  method Maybe#(value_T) read3(preg_T i);
  method Maybe#(value_T) read4(preg_T i);

  method Action write1(preg_T i, value_T v);
  method Action write2(preg_T i, value_T v);

  method Action freePReg(token_T tok, preg_T x);
  method Action rewindtoToken(token_T tok); // exception
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

interface FP_Stage_Con#(type tick_T, 
                	type token_T,
			type init_T,
			type req_T,
			type resp_T,
			type next_T);

endinterface

module [Connected_Module] mkFP_Stage_Con#(String stagename,
                                	  String linkname, 
                                	  String servername,
					  String prevname,
					  String nextname,
					  Integer sz) 
    //interface:
               (FP_Stage_Con#(tick_T, 
	                      token_T, 
			      init_T, 
			      req_T, 
			      resp_T, 
			      next_T))
        provisos
          (Bits#(token_T, token_SZ), 
	   Bounded#(token_T),
	   Eq#(token_T),
	   Literal#(token_T),
           Bits#(tick_T, tick_SZ), 
           Bits#(init_T, init_SZ),
           Bits#(req_T, req_SZ),
           Bits#(resp_T, resp_SZ),
           Bits#(next_T, next_SZ),
	   Transmittable#(Tuple3#(token_T, init_T, req_T)),
	   Transmittable#(Tuple3#(token_T, resp_T, next_T)),
	   Transmittable#(Tuple3#(token_T, tick_T, req_T)),
	   Transmittable#(Tuple2#(token_T, resp_T)),
	   Transmittable#(Tuple2#(token_T, init_T)),
	   Transmittable#(Tuple2#(token_T, next_T)),
	   Transmittable#(token_T));

  //Local definitions
  token_T tableMin = minBound;
  token_T tableMax = fromInteger(sz - 1);

  //Links
  Connection_Client#(Tuple3#(token_T, init_T, req_T),
                     Tuple3#(token_T, resp_T, next_T))
  //...
  link_to_unit   <- mkConnection_Client(linkname);
  
  Connection_Server#(Tuple3#(token_T, tick_T, req_T),
                     Tuple2#(token_T, resp_T))
  //...
  link_from_tp   <- mkConnection_Server(servername);
  
  Connection_Receive#(Tuple2#(token_T, init_T))
  //...
  link_from_prev <- mkConnection_Receive(prevname);
  
  Connection_Send#(Tuple2#(token_T, next_T))
  //...
  link_to_next   <- mkConnection_Send(nextname);
  
  Connection_Receive#(token_T)
  //...
  link_killToken <- mkConnection_Receive("link_killToken");

  		
  //SRAM tables
  RegFile#(token_T, init_T) values <- mkRegFile(tableMin, tableMax);
  RegFile#(token_T, Bool)   valids <- mkRegFile(tableMin, tableMax); 
  RegFile#(token_T, Bool)   dones  <- mkRegFile(tableMin, tableMax); 

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
  
  rule killToken (True);
    
    let tok <- link_killToken.receive();
  
    valids.upd(tok, False);
  
  endrule

endmodule

module [Module] mkFP_Stage#(String stagename,
                            FP_Unit#(token_T, init_T, req_T, resp_T, next_T) unit,
                            Integer sz)
   //interface:
               (FP_Stage#(tick_T, 
	                  token_T, 
			  init_T, 
			  req_T, 
			  resp_T, 
			  next_T))
        provisos
          (Bits#(token_T, token_SZ), 
	   Bounded#(token_T),
	   Eq#(token_T),
	   Literal#(token_T),
           Bits#(tick_T, tick_SZ), 
           Bits#(init_T, init_SZ),
           Bits#(req_T, req_SZ),
           Bits#(resp_T, resp_SZ),
           Bits#(next_T, next_SZ));

  //Local definitions
  token_T tableMin = minBound;
  token_T tableMax = fromInteger(sz - 1);

  //FIFOs
  FIFO#(Tuple3#(token_T, req_T, tick_T))  reqQ      <- mkFIFO(); // YYY: ndave make Bypass
  FIFO#(Tuple3#(token_T, resp_T, next_T)) unitRespQ <- mkSizedFIFO(sz);
  FIFO#(Tuple2#(token_T, next_T))	  nextQ     <- mkBypassSizedFIFO(sz);
  		
  //SRAM tables
  RegFile#(token_T, init_T)		  values    <- mkRegFile(tableMin, tableMax);
  RegFile#(token_T, Bool)		  valids    <- mkRegFile(tableMin, tableMax); 
  RegFile#(token_T, Bool)		  dones     <- mkRegFile(tableMin, tableMax); 

  match {.respQToken, .*, .*} = unitRespQ.first();
  Bool respValid = valids.sub(respQToken);

  match {.reqTok, .reqReq, .reqTick} = reqQ.first();
  Bool reqValid = valids.sub(reqTok);  

  // Schedule should be:
  // in.put < server.req < reqMake < getUnitResponse < tossDeadResps < 
  // server.resp < tossDeadNexts < killToken

  //getUnitResponse
  
  rule getUnitResponse (True);
  
    Tuple3#(token_T, resp_T, next_T) tup <- unit.response.get();
    match {.tok, .resp, .next} = tup;

    Bool valid = valids.sub(tok);
   
    if (valid)
      begin // don't insert it was killed
        unitRespQ.enq(tup);
        dones.upd(tok, True);
      end       
  endrule
    
  //tossDeadResps
  
  rule tossDeadResps(respValid == False);
    unitRespQ.deq();
  endrule

  //reqMake
  
  rule reqMake (reqValid);

    reqQ.deq();
   
    Bool done  =  dones.sub(reqTok); 

    init_T iVal = values.sub(reqTok);

    if (!reqValid)
       $display("%s ERROR: requesting unallocated token %h", stagename, reqTok);
     else if (done)
       $display("%s ERROR: re-requesting finished token %h", stagename, reqTok);            
     else // !done
       unit.request.put(tuple3(reqTok, iVal, reqReq));
  endrule

  //From Previous Stage

  interface Put in;
    
    method Action put(Tuple2#(token_T, init_T) tup);

     match {.tok,.iVal} = tup;

     Bool valid = valids.sub(tok);

     if(valid)
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
    endmethod
  endinterface

  interface Server server;
  
    //To FP_Unit
  
    interface Put request;
    
      method Action put(Tuple3#(token_T, req_T, tick_T) tup);
        reqQ.enq(tup);
      endmethod

    endinterface

    //From FP_Unit

    interface Get response;

      method ActionValue#(Tuple2#(token_T, resp_T)) get() if (respValid);

        match {.tok, .resp, .next} = unitRespQ.first();
	
        unitRespQ.deq();
        nextQ.enq(tuple2(tok, next));
	
        return tuple2(tok, resp);
	
      endmethod

    endinterface
    
  endinterface
  
  //To Next Stage
  
  interface Get out;

    method ActionValue#(Tuple2#(token_T, next_T)) get();
    
     nextQ.deq();
     return nextQ.first();
     
    endmethod

  endinterface

  //killToken
  
  method Action killToken(token_T tok);
  
    valids.upd(tok, False);
  
  endmethod

endmodule

//-------------------------------------------------------------------------//
// Token Generation Unit                                                   //
//-------------------------------------------------------------------------//

// The first (and last) stage in the Functional Partition pipeline. 

// The Timing Partition uses it as the first stage to generate a token to
// flow through the pipeline. This module is pretty general and can be reused
// across many functional partitions

// It's also the last stage because Global Committ should report back when
// a token has completed and can be reused.

// TODO: This could be a bit smarter. Currently it assumes that tokens commit
// inorder. Actually, this is probably a pretty reasonable assumption.


module [Module] mkFP_TokGen
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
              

//-------------------------------------------------------------------------//
// Top-Level Functional Partition                                          //
//-------------------------------------------------------------------------//

// Given all the appropriate units (already instantiated), connect them
// into a functional partition.

// Currently the TokenGen stage is separate enough that we pass it in differently

// Note: This instantiates all stages to have the same table size.
// A desirable capability would be to set the size per-stage.

//mkTOY_FP :: FP_Stage a b ->      #TokGen
//	      FP_Unit b c  ->      #Fetch
//	      FP_Unit c d  ->      #Decode
//	      FP_Unit d e  ->      #Execute
//	      FP_Unit e f  ->      #Mem
//	      FP_Unit f g  ->      #Local Commit
//	      FP_Unit g a  ->      #Global Commit
//            BypassUnit   ->      #Bypass unit and map table
//            Connection_Client  ->      #Link to IMem
//            Connection_Client  ->      #Link to DMem
//            Connection_Send    ->      #Link to DMem commit
//            Connection_Send    ->      #Link to DMem killRange
//            Integer      ->      #Table size
//	      FunctionalPartition
/*

module [Module] mkFunctionalPartition#(FP_Stage#(tick_T, token_T, tok_data_T, tok_req_T, tok_resp_T, fet_data_T) tok,
                                       FP_Unit#(token_T, fet_data_T, fet_req_T, fet_resp_T, dec_data_T) fet,
				       FP_Unit#(token_T, dec_data_T, dec_req_T, dec_resp_T, exe_data_T) dec,
				       FP_Unit#(token_T, exe_data_T, exe_req_T, exe_resp_T, mem_data_T) exe,
				       FP_Unit#(token_T, mem_data_T, mem_req_T, mem_resp_T, lco_data_T) mem,
				       FP_Unit#(token_T, lco_data_T, lco_req_T, lco_resp_T, gco_data_T) lco,
				       FP_Unit#(token_T, gco_data_T, gco_req_T, gco_resp_T, tok_data_T) gco,
				       BypassUnit#(rname_T, prname_T, value_T, token_T, snapshotptr_T) bypass, 
				       Connection_Client#(addr_T, inst_T) link_to_imem,
				       Connection_Client#(MemReq#(token_T, addr_T, value_T), MemResp#(value_T)) link_to_dmem,
				       Connection_Send#(token_T) link_to_mem_commit,
				       Connection_Send#(Tuple2#(token_T, token_T)) link_to_mem_killRange,
				       Integer sz)
    //interface:						       
  		(FunctionalPartition#(tick_T,                  //tick type
		                      token_T,                 //token type
		                      addr_T,                  //address type
				      inst_T,                  //instruction type
				      value_T,                 //value type
		                      tok_req_T, tok_resp_T,   //tokenReq, tokenResp
  				      fet_req_T, fet_resp_T,   //fetchReq, fetchResp
  				      dec_req_T, dec_resp_T,   //decodeReq, decodeResp
  				      exe_req_T, exe_resp_T,   //execReq, execResp
  				      mem_req_T, mem_resp_T,   //memReq, memResp
  				      lco_req_T, lco_resp_T,   //lcommitReq, lcommitResp
  				      gco_req_T, gco_resp_T))  //gcommitReq, gcommitResp
    provisos
            (Bits#(token_T, token_SZ), 
	     Bounded#(token_T),
	     Eq#(token_T),
	     Arith#(token_T),
	     Literal#(token_T),
	     Bits#(addr_T, addr_SZ), 
	     Bits#(value_T, value_SZ), 
	     Bits#(inst_T, inst_SZ),
             Bits#(tick_T,     tick_SZ),
	     Bits#(tok_data_T, tok_data_SZ),
	     Bits#(fet_data_T, fet_data_SZ),
	     Bits#(dec_data_T, dec_data_SZ),
	     Bits#(exe_data_T, exe_data_SZ),
	     Bits#(mem_data_T, mem_data_SZ),
	     Bits#(lco_data_T, lco_data_SZ),
	     Bits#(gco_data_T, gco_data_SZ),
	     Bits#(tok_req_T,  tok_req_SZ),
	     Bits#(fet_req_T,  fet_req_SZ),
	     Bits#(dec_req_T,  dec_req_SZ),
	     Bits#(exe_req_T,  exe_req_SZ),
	     Bits#(mem_req_T,  mem_req_SZ),
	     Bits#(lco_req_T,  lco_req_SZ),
	     Bits#(gco_req_T,  gco_req_SZ),
	     Bits#(tok_resp_T, tok_resp_SZ),
	     Bits#(fet_resp_T, fet_resp_SZ),
	     Bits#(dec_resp_T, dec_resp_SZ),
	     Bits#(exe_resp_T, exe_resp_SZ),
	     Bits#(mem_resp_T, mem_resp_SZ),
	     Bits#(lco_resp_T, lco_resp_SZ),
	     Bits#(gco_resp_T, gco_resp_SZ));
  
  
  //Instantiate the stage wrappers
  
  let tok_stage = tok; //For now this is different.
  let fet_stage <- mkFP_Stage("FET", fet, sz);
  let dec_stage <- mkFP_Stage("DEC", dec, sz);
  let exe_stage <- mkFP_Stage("EXE", exe, sz);
  let mem_stage <- mkFP_Stage("MEM", mem, sz);
  let lco_stage <- mkFP_Stage("LCO", lco, sz);
  let gco_stage <- mkFP_Stage("GCO", gco, sz);

  //Connect up the stages

  mkConnection(tok_stage.out, fet_stage.in);
  mkConnection(fet_stage.out, dec_stage.in);
  mkConnection(dec_stage.out, exe_stage.in);
  mkConnection(exe_stage.out, mem_stage.in);
  mkConnection(mem_stage.out, lco_stage.in);
  mkConnection(lco_stage.out, gco_stage.in);
  mkConnection(gco_stage.out, tok_stage.in);
  
  //Interfaces to Timing Partition
  
  interface tokgen        = tok_stage.server;
  interface fetch         = fet_stage.server;
  interface decode        = dec_stage.server;
  interface execute       = exe_stage.server;
  interface memory        = mem_stage.server;
  interface local_commit  = lco_stage.server;
  interface global_commit = gco_stage.server;
  
  //killToken
  
  //This is broadcast to every stage
  
  interface Put killToken;

    method Action put(token_T t);

      tok_stage.killToken(t);
      fet_stage.killToken(t);
      dec_stage.killToken(t);
      exe_stage.killToken(t);
      mem_stage.killToken(t);
      lco_stage.killToken(t);
      gco_stage.killToken(t);

      bypass.rewindtoToken(t-1); //so we catch the branch
      link_to_mem_killRange.send(tuple2(t, t));

    endmethod

  endinterface

  //interfaces to Memory
  
  interface to_dmem = link_to_dmem.client;
  interface to_imem = link_to_imem.client;
  
  interface commit = link_to_mem_commit.outgoing;
  interface killRange = link_to_mem_killRange.outgoing; 

endmodule
*/
