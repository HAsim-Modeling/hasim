import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import Interfaces::*;
import Primitive::*;
import ValueVector::*;


typedef Maybe#(Tuple3#(Token, Tick, data_T)) Pipe#(parameter type data_T);

(* synthesize *)
module [Module] mkTimingParition ();
  
  //Latencies
  Tick fet_L = 1;
  Tick dec_L = 1;
  Tick exec_L = 1;
  Tick mem_L = 1;
  Tick wb_L = 1;
  
  Reg#(Token) hd_tok <- mkReg(0);
  Reg#(Token) tl_tok <- mkReg(0);
  
  FM_Test fm <- mkFM();
  
  Reg#(Addr) pc <- mkReg(0);
  
  Reg#(Tick) curTick <- mkReg(0);
  
  Reg#(Pipe#(Inst)) pr_fet <- mkReg(Nothing);
  
  Reg#(Pipe#(DecodedInst)) pr_dec <- mkReg(Nothing);
  
  Reg#(Pipe#(InstResult)) pr_exec <- mkReg(Nothing);
  
  Reg#(Pipe#(InstResult)) pr_mem <- mkReg(Nothing);
  
  Reg#(Pipe#(void)) pr_wb <- mkReg(Nothing);
  
  rule fetchReq (True);
  
    let tok <- fm.fetch.putTM(tuple3(hd_tok, pc, curTick));
    let res <- fm.fetch.getTM(curTick);
    
    match {.*, .inst} = res;
    
    pc <= pc + 1;
    curTick <= curTick + 1;
    hd_tok <= hd_tok + 1;
    
    pr_fet <= Valid tuple3(hd_tok, curTick, inst);
  
  endrule
  
  rule decodeReq (pr_fet matches tagged Valid {.tok, .tck, .inst});
  
    let t2 <- fm.decode.putTM(tuple3(tok, ?, tck));
    let res <- fm.decode.getTM(tck);
    
    match {.*, .dec} = res;
    
    pr_dec <= Valid tuple3(tok, tck + dec_L, dec);
  
  endrule
  
  rule execReq (pr_dec matches tagged Valid {.tok, .tck, .dec});
  
    let t2 <- fm.execute.putTM(tuple3(tok, ?, tck));
    let res <- fm.execute.getTM(tck);
    
    match {.*, .r} = res;
    
    case (r) matches
      tagged RBranchTaken .addr:
        pc <= addr;
      default:
        noAction;
    endcase
    
    pr_exec <= Valid tuple3(tok, tck + exec_L, r);
  
  endrule

  rule memReq (pr_exec matches tagged Valid {.tok, .tck, .r});
  
    let t2 <- fm.memory.putTM(tuple3(tok, ?, tck));
    let res <- fm.memory.getTM(tck);
    
    match {.*, .rf} = res;
    
    
    pr_mem <= Valid tuple3(tok, tck + mem_L, rf);
  
  endrule

  rule wbReq (pr_exec matches tagged Valid {.tok, .tck, .rf});
  
    let t2 <- fm.memory.putTM(tuple3(tok, ?, tck));
    let res <- fm.memory.getTM(tck);
    
    pr_dec <= Valid tuple3(tok, tck + mem_L, ?);
  
    tl_tok <= tl_tok + 1;
    
  endrule



endmodule
