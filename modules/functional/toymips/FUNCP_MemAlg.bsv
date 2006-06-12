import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import FUNCP_Base::*;
import FUNCP_MemState::*;
import Debug::*;

import ISA::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"


//-------------------------------------------------------------------------//
// Memory Unit                                                             //
//-------------------------------------------------------------------------//


`define MODULE_NAME "mkMem"
module [HASim_Module] mkFUNCP_MemAlg ();

  
  //Links
  Connection_Server#(Tuple3#(Token, ExecedInst, void),
                     Tuple3#(Token, void, ExecedInst)) 
  //...
  link_mem <- mkConnection_Server("link_mem");
	  
  Connection_Client#(MemReq, MemResp) 
  //...
  link_to_dmem <- mkConnection_Client("mem_dmem");

  
  Connection_Client#(PRName, Maybe#(Value)) 
  //...
        link_read3 <- mkConnection_Client("mem_to_bypass_read3");

  Connection_Client#(PRName, Maybe#(Value)) 
  //...
        link_read4 <- mkConnection_Client("mem_to_bypass_read4");

  Connection_Send#(Tuple2#(PRName, Value)) 
  //...
        link_write2 <- mkConnection_Send("mem_to_bypass_write2");

  FIFO#(Tuple2#(Token, ExecedInst)) 
  //...
  lookupQ <- mkFIFO();

  FIFO#(Tuple2#(Token, ExecedInst)) 
  //...
  waitingQ <- mkFIFO();
  //doLookup

  rule doLookup (True);
       
    debug_rule("doLookup");
  
    match {.t, .i, .*} <- link_mem.getReq();

    PRName va = ?;
    PRName vb = ?;

    //Get the address info. XXX why doesn't this happen in exe?
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        va = idx;
      tagged EStore{opdest: .oprd, val: .v, idx: .idx, offset: .o}:
        begin
          va = idx;
          vb = v;
        end
    endcase

    link_read3.makeReq(va);
    link_read4.makeReq(vb);
      
    lookupQ.enq(tuple2(t, i));
  endrule
  
  //doReq
  
  rule doReq (True);
    
    debug_rule("doReq");
    
    let mva <- link_read3.getResp();
    let mvb <- link_read4.getResp();

    match {.t, .i} = lookupQ.first();
    lookupQ.deq();
    
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
      begin
        debug_case("i", "ELoad");
       
        if (isJust(mva))
        begin
	  debug_then("isJust(mva)");
	  
	  Addr a = truncate(unJust(mva)) + zeroExtend(o);
          link_to_dmem.makeReq(Ld {addr: a, token: t});
	  
          debug(2, $display("MEM: [%d] Load Request: 0x%h", t, a));
        end
      end
      tagged EStore{opdest: .oprd, val: .v, idx: .idx, offset: .o}:
      begin
        debug_case("i", "EStore");
	
        let addr = unJust(mva) + zeroExtend(o);
	
        if (isJust(mva) && isJust(mvb))
        begin
	  debug_then("(isJust(mva) && isJust(mvb))");
          link_to_dmem.makeReq(St {val: unJust(mvb), addr: truncate(addr), token: t});
	  
          debug(2, $display("MEM: [%d] Store Request: 0x%h := %d", t, addr, unJust(mvb)));
        end
      end
      default: // push
        debug_case_default("i");
    endcase
    
    waitingQ.enq(tuple2(t, i));
  endrule

  //getResp

  rule getResp(True);
       
    debug_rule("getResp");
  
    match {.tok, .i} = waitingQ.first();
    
    case (i) matches
      tagged ELoad {idx: .idx, offset: .o, pdest: .prd, opdest: .oprd}:
        begin
	
	  debug_case("i", "ELoad");
	  
          let resp <- link_to_dmem.getResp();
	  
          Value v = case (resp) matches
                      tagged LdResp .val: return val;
                      tagged StResp .*  : return 0; // impossible
                    endcase;
		    
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok,?,EWB{pdest: prd, opdest: oprd}));
          link_write2.send(tuple2(prd, v));
	  
	  
        end
      tagged EStore {opdest: .oprd, val: .*, idx: .*, offset: .*}:
        begin
	
	  debug_case("i", "EStore");
	  
          let resp <- link_to_dmem.getResp();
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok, ?, ENop {opdest: oprd}));
	  
          debug(2, $display("MEM: [%d] StResp", tok));
        end
      default:
        begin
	
	  debug_case_default("i");
	  
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok, ?, i));
	  
          debug(2, $display("MEM: [%d] Non-Memory op", tok));
        end
    endcase
  endrule
  
endmodule
`undef MODULE_NAME  
