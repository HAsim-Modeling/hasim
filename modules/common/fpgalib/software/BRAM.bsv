//////////////////////////////////////////////////////////////////////
//
// BRAM.bsv
// Copyright (C) 2005 Carnegie Mellon University
//
// Description:
// Dual-ported BRAM Verilog wrapper (see BRAM.v)
//
// Revision History
// Switched to HAsim coding conventions
// Wrapped Syncronous RAM with FIFOs. upd method goes last.
// 9-12-2006, Michael
// File created
// 5-16-2006, Eric Chung
//
//

import FIFO::*;
import RegFile::*;

interface BRAM#(type idx_type, type data_type);

  method Action  read_req(idx_type idx);
  method Action read_req2(idx_type idx);
  method Action read_req3(idx_type idx);

  method ActionValue#(data_type) read_resp();
  method ActionValue#(data_type) read_resp2();
  method ActionValue#(data_type) read_resp3();  
  
  method Action	upd(idx_type idx, data_type data);
  
endinterface

module mkBRAM#(Integer low, Integer high) 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));

  FIFO#(Bit#(0)) q1 <- mkFIFO();
  FIFO#(Bit#(0)) q2 <- mkFIFO();
  FIFO#(Bit#(0)) q3 <- mkFIFO();

  Sync_BRAM#(idx_type, data_type) br <- mkBRAM_Sync(low, high);

  method Action  read_req(idx_type idx);
    q1.enq(?);
    br.read_rq(idx);
  endmethod
    
  method Action read_req2(idx_type idx);
    q2.enq(?);
    br.read_rq2(idx);
  endmethod

  method Action read_req3(idx_type idx);
    q3.enq(?);
    br.read_rq3(idx);
  endmethod

  method ActionValue#(data_type) read_resp();
    q1.deq();
    return br.read_rsp();
  endmethod
  
  method ActionValue#(data_type) read_resp2();
    q2.deq();
    return br.read_rsp2();
  endmethod

  method ActionValue#(data_type) read_resp3(); 
    q3.deq();
    return br.read_rsp3();
  endmethod 
  
  method Action	upd(idx_type idx, data_type data);
    br.upd(idx, data);
  endmethod

endmodule

module mkBRAM_Full 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));


  BRAM#(idx_type, data_type) br <- mkBRAM(0, valueof(TExp#(idx)));

  return br;

endmodule

//Synchronous BRAM primitive

interface Sync_BRAM#(type idx_type, type data_type);

  method Action  read_rq(idx_type idx);
  method Action read_rq2(idx_type idx);
  method Action read_rq3(idx_type idx);

  method data_type read_rsp();
  method data_type read_rsp2();
  method data_type read_rsp3();  
  
  method Action	upd(idx_type idx, data_type data);
  
endinterface


module mkBRAM_Sync#(Integer low, Integer high) 
  //interface:
              (Sync_BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));

  RegFile#(idx_type, data_type) br <- mkRegFile(fromInteger(low), fromInteger(high));
  Reg#(data_type) rd1 <- mkRegU();
  Reg#(data_type) rd2 <- mkRegU();
  Reg#(data_type) rd3 <- mkRegU();
  Reg#(Bool)  rdy_rd1 <- mkReg(False);
  Reg#(Bool)  rdy_rd2 <- mkReg(False);
  Reg#(Bool)  rdy_rd3 <- mkReg(False);

  method Action  read_rq(idx_type idx);
  
    rdy_rd1 <= True;
    rd1 <= br.sub(idx);
    
  endmethod
  
  method Action read_rq2(idx_type idx);
  
    rdy_rd2 <= True;
    rd2 <= br.sub(idx);
    
  endmethod

  method Action read_rq3(idx_type idx);
  
    rdy_rd3 <= True;
    rd3 <= br.sub(idx);
    
  endmethod

  method data_type read_rsp()  if (rdy_rd1);
    return rd1;
  endmethod 

  method data_type read_rsp2() if (rdy_rd2);
    return rd2;
  endmethod 

  method data_type read_rsp3() if (rdy_rd3); 
    return rd3;
  endmethod  
  
  method Action	upd(idx_type idx, data_type data);
  
    br.upd(idx, data);
  
  endmethod
  
endmodule

