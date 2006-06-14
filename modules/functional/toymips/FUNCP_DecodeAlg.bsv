import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import FUNCP_Base::*;
import Debug::*;

import ISA::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"

//-------------------------------------------------------------------------//
// Decode Stage                                                            //
//-------------------------------------------------------------------------//

// Also lookup physical register from BypassUnit


`define MODULE_NAME "mkDecode"
module [HASim_Module] mkFUNCP_DecodeAlg ();

  //Ports
  Connection_Server#(Tuple3#(Token, Tuple2#(Addr, Inst), void), 
                     Tuple3#(Token, DepInfo, Tuple2#(Addr, DecodedInst))) 
  //...
  link_dec <- mkConnection_Server("link_dec");

  Connection_Client#(Tuple3#(Maybe#(RName), Token, Bool), 
                     Tuple2#(PRName, PRName))
  //...
  link_mapping <- mkConnection_Client("dec_to_bypass_mapping");
  
  Connection_Client#(RName, PRName) 
  //...
        link_lookup1 <- mkConnection_Client("dec_to_bypass_lookup1");

  Connection_Client#(RName, PRName) 
  //...
        link_lookup2 <- mkConnection_Client("dec_to_bypass_lookup2");

  FIFO#(Tuple3#(Token, Addr, Inst)) 
  //...
  waitingQ <- mkFIFO();
  
  //handleDecode
  
  //Handles the actual decoding and register allocation
  
  rule handleDecode (True);
  
    debug_rule("handleDecode");
    
    Tuple3#(Token, Tuple2#(Addr, Inst), void) 
    //...
    tup <- link_dec.getReq();
    
    match {.t, {.a, .inst}, .*} = tup;
    
    //Get the architectural dest/sources
    match {.mrd, .ara, .arb, .rewind} = case (inst) matches
      tagged IAdd {dest: .rd, src1: .ra, src2: .rb}:
          return tuple4(Valid rd, ra, rb, False);
      tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
          return tuple4(Valid rd, ra, rb, False);
      tagged IBz {cond: .c , addr:  .addr}:
          return tuple4(Invalid, c, addr, True);
      tagged ILoad {dest: .rd, idx: .ri, offset: .off}:
	  return tuple4(Valid rd, ri, ?, False);
      tagged ILoadImm {dest: .rd, imm: .i}:
          return tuple4(Valid rd, ?, ?, False);
      tagged IStore {src: .rsrc, idx: .ri, offset: .off}:
          return tuple4(Invalid, rsrc, ri, False);
      tagged ITerminate: 
          return tuple4(Invalid, ?,?, False);
      endcase;

    //Translate into physical registers
    link_lookup1.makeReq(ara);
    link_lookup2.makeReq(arb);
    
    link_mapping.makeReq(tuple3(mrd, t, rewind));
    
    waitingQ.enq(tuple3(t, a, inst));
        
  endrule
  
  rule handleResponse (True);
  
    debug_rule("handleResponse");
    
    match {.t, .a, .inst} = waitingQ.first();
    waitingQ.deq();
    
    DepInfo depinfo = ?;
    DecodedInst decinst = ?;
    
    PRName pra <- link_lookup1.getResp();
    PRName prb <- link_lookup2.getResp();
    
    
    match {.prd, .oprd} <- link_mapping.getResp();
    
    //Actually do the decode
    case (inst) matches
      tagged IAdd {dest: .rd, src1: .ra, src2: .rb}:
        begin
	  debug_case("inst", "IAdd");

          decinst = DAdd {pdest: prd, opdest: oprd, op1: pra, op2: prb};
          depinfo = DepInfo {dep_dest: Just(tuple2(rd, prd)), dep_src1: Just(tuple2(ra, pra)), dep_src2: Just(tuple2(rb,prb))};
	  
          debug(2, $display("DEC: [%d]: IAdd R%d := R%d + R%d", t, rd, ra, rb));
	end
      tagged ISub {dest: .rd, src1: .ra, src2: .rb}:
        begin
	  debug_case("inst", "ISub");
	  
          decinst = DSub {pdest: prd, opdest: oprd, op1: pra, op2: prb};
          depinfo = DepInfo {dep_dest: Just(tuple2(rd, prd)), dep_src1: Just(tuple2(ra, pra)), dep_src2: Just(tuple2(rb,prb))};

          debug(2, $display("DEC: [%d]: ISub R%d := R%d - R%d", t, rd, ra, rb));
        end
      tagged IBz {cond: .c , addr:  .addr}:
        begin
	  debug_case("inst", "IBz");
	  
          decinst = DBz {opdest: oprd, cond: pra, addr: prb};
          depinfo = DepInfo {dep_dest: Nothing, dep_src1: Just(tuple2(c,pra)), dep_src2: Just(tuple2(addr,prb))};

          debug(2, $display("DEC: [%d]: IBz (R%d == 0)? pc := (R%d)", t, c, addr));
        end
      tagged ILoad {dest: .rd, idx: .ri, offset: .off}:
        begin
	  debug_case("inst", "ILoad");
	  
          decinst = DLoad {pdest: prd, opdest: oprd, idx: pra, offset: zeroExtend(off)};
          depinfo = DepInfo {dep_dest: Just(tuple2(rd,prd)), dep_src1: Just(tuple2(ri,pra)), dep_src2: Nothing};

          debug(2, $display("DEC: [%d]: ILoad R%d := (R%d + %h)", t, rd, ri, off));
        end
      tagged ILoadImm {dest: .rd, imm: .i}:
        begin
	  debug_case("inst", "ILoadImm");
	  
          decinst = DLoadImm {pdest: prd, opdest: oprd, value: signExtend(i)};
          depinfo = DepInfo {dep_dest: Just(tuple2(rd,prd)), dep_src1: Nothing, dep_src2: Nothing};

          debug(2, $display("DEC: [%d]: ILoadImm R%d := %d", t, rd, i));
        end
      tagged IStore {src: .rsrc, idx: .ri, offset: .off}:
        begin
	  debug_case("inst", "IStore");
	  
          decinst = DStore{value: pra, opdest: oprd, idx: prb, offset: zeroExtend(off)};
          depinfo = DepInfo {dep_dest: Nothing, dep_src1: Just(tuple2(ri,prb)), dep_src2: Just(tuple2(rsrc,pra))};
	  
          debug(2, $display("DEC: [%d]: IStore (R%d + %h) := R%d", t, ri, off, rsrc));
        end
      tagged ITerminate:
        begin
	  debug_case("inst", "ITerminate");
	  
          decinst = DTerminate;
          depinfo = DepInfo {dep_dest: Nothing, dep_src1: Nothing, dep_src2: Nothing};

          debug(2, $display("DEC: [%d]: ITerminate", t));
        end
    endcase

    debug(2, $display("DEC: Physical Sources: (PR%d, PR%d)", pra, prb));
    
    link_dec.makeResp(tuple3(t, depinfo, tuple2(a, decinst)));
    
  endrule
  
endmodule
`undef MODULE_NAME  

