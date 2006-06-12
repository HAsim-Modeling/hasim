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
// Execute Unit                                                            //
//-------------------------------------------------------------------------//

// Also reads physical register file

`define MODULE_NAME "mkFUNCP_ExecuteAlg"
module [HASim_Module] mkFUNCP_ExecuteAlg ();
  
  //Ports
  Connection_Server#(Tuple3#(Token, Tuple2#(Addr, DecodedInst), void),
                     Tuple3#(Token, InstResult, ExecedInst)) 
  //...
  link_exe <- mkConnection_Server("link_exe");
  
  Connection_Client#(PRName, Maybe#(Value)) 
  //...
        link_read1 <- mkConnection_Client("exe_to_bypass_read1");

  Connection_Client#(PRName, Maybe#(Value)) 
  //...
        link_read2 <- mkConnection_Client("exe_to_bypass_read2");

  Connection_Send#(Tuple2#(PRName, Value)) 
  //...
        link_write1 <- mkConnection_Send("exe_to_bypass_write1");

  //State elements
  FIFO#(Tuple3#(Token, Tuple2#(Addr, DecodedInst), void)) 
  //...
  waitingQ <- mkFIFO();
  
  //handleExec
  
  //We can't always exec right away, since our operands may not be available.
   
  rule handleExec (True);
  
    debug_rule("handleExec");

    let tup <- link_exe.getReq();
    match {.t, {.addr, .dec}, .*} = tup;

    PRName va = ?;
    PRName vb = ?;

    //Get the registers which hold the values
    case (dec) matches
      tagged DAdd {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
        begin
          va = ra;
          vb = rb;
        end
      tagged DSub {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
        begin
          va = ra;
          vb = rb;
        end       
      tagged DBz {opdest: .oprd, cond: .c, addr: .a}:
        begin
          va = c;
          vb = a;
        end
    endcase

    //Try to get the values from the Bypass unit
    link_read1.makeReq(va);
    link_read2.makeReq(vb);

    waitingQ.enq(tup);

  endrule
  
  //execute

  rule execute (True);
  
    debug_rule("execute");

    match {.t, {.addr, .dec}, .*} = waitingQ.first();

    ExecedInst ei = ?;
    Maybe#(Addr) branchResult = Nothing;

     //Try to get the values from the Bypass unit
     Maybe#(Value) mva <- link_read1.getResp();
     Maybe#(Value) mvb <- link_read2.getResp();

     //Actually do the execute
     case (dec) matches
       tagged DAdd {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
       begin
       
	 debug_case("dec", "DAdd");
	 
         if (isJust(mva) && isJust(mvb))
         begin
	 
	   debug_then("isJust(mva) && isJust(mvb)");
	   
	   let result = unJust(mva) + unJust(mvb);
	   
           link_write1.send(tuple2(prd, result));
           link_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
           waitingQ.deq();

	   debug(2, $display("EXE: [%d] DAdd (old PR%d) PR%d <= 0x%h", t, oprd, prd, result));
	   
         end
       end
       tagged DSub {pdest: .prd, opdest: .oprd, op1: .ra, op2: .rb}:
       begin
       
	 debug_case("dec", "DSub");
	 
         if (isJust(mva) && isJust(mvb))
         begin
	 
	   debug_then("isJust(mva) && isJust(mvb)");
	   
	   let result = unJust(mva) - unJust(mvb);
	   
           link_write1.send(tuple2(prd, result));
           link_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
           waitingQ.deq();
	   
	   debug(2, $display("EXE: [%d] DAdd (old PR%d) PR%d <= 0x%h", t, oprd, prd, result));
	   
         end
       end
       tagged DBz {opdest: .oprd, cond: .c, addr: .a}:
       begin
       
	 debug_case("dec", "DBz");
	 
	 case (mva) matches
	   tagged Valid .cval:
	   begin
	   
	     debug_case("mva", "Valid");
	     
	     if (cval != 0)
	     begin // XXX extra cleverness needed
	     
	       debug_then("cval != 0");
	       
	       link_exe.makeResp(tuple3(t, RBranchNotTaken, ENop {opdest: oprd}));
	       waitingQ.deq();

	       debug(2, $display("EXE: [%d] DBz Not Taken (cval == %d) (old PR%d)", t, cval, oprd));
	       
	     end
	     else   // condition must be zero
	     begin
	     
	       debug_else("cval != 0");
	       
	       case (mvb) matches
		 tagged Valid .dest:
		 begin
		 
		   debug_case("mvb", "Valid");
		   
                   link_exe.makeResp(tuple3(t, RBranchTaken truncate(dest), ENop{opdest: oprd}));
                   waitingQ.deq();
        	 end
		 default:
		   debug_case_default("mvb");
	       endcase
	       
	     end
	   end
	   default:
	     debug_case_default("mva");
	 endcase
       end
       tagged DLoad {pdest: .prd, opdest: .oprd, idx: .idx, offset: .o}: // XXX do offset calc here?
       begin
         
	 debug_case("dec", "DLoad");
	 
         link_exe.makeResp(tuple3(t, RNop, ELoad {idx: idx, offset: o, pdest:prd, opdest: oprd}));
         waitingQ.deq();
	 
	 debug(2, $display("EXE: [%d] DLoad (old PR%d) PR%d := (PR%d + 0x%h)", t, oprd, prd, idx, o));
       end
       tagged DLoadImm {pdest: .prd, opdest: .oprd, value: .val}:
       begin

	 debug_case("dec", "DLoadImm");

         link_write1.send(tuple2(prd, signExtend(val)));
         link_exe.makeResp(tuple3(t, RNop, EWB {pdest: prd, opdest: oprd}));
         waitingQ.deq();
	 
	 debug(2, $display("EXE: [%d] DLoadImm (old PR%d) PR%d := 0x%h", t, oprd, prd, val));
       end
       tagged DStore {value: .v, opdest: .oprd, idx: .idx, offset: .o}:// XXX do offset calc here?
       begin

	 debug_case("dec", "DLoadImm");

         link_exe.makeResp(tuple3(t, RNop, EStore {idx: idx, offset: o, val: v, opdest: oprd}));
         waitingQ.deq();
	 
	 debug(2, $display("EXE: [%d] DStore (old PR%d) (PR%d + 0x%h) := PR%d", t, oprd, idx, o, v));
       end
       tagged DTerminate:
       begin

	 debug_case("dec", "DTerminate");
	 
         link_exe.makeResp(tuple3(t, RTerminate, ETerminate));
         waitingQ.deq();

	 debug(2, $display("EXE: [%d] DTerminate", t)); 
       end
    endcase
  endrule

endmodule
`undef MODULE_NAME  


