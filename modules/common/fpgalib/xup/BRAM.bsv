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
    br.read_req(idx);
  endmethod
    
  method Action read_req2(idx_type idx);
    q2.enq(?);
    br.read_req2(idx);
  endmethod

  method Action read_req3(idx_type idx);
    q3.enq(?);
    br.read_req3(idx);
  endmethod

  method ActionValue#(data_type) read_resp();
    q1.deq();
    return br.read_resp();
  endmethod
  
  method ActionValue#(data_type) read_resp2();
    q2.deq();
    return br.read_resp2();
  endmethod

  method ActionValue#(data_type) read_resp3(); 
    q3.deq();
    return br.read_resp3();
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

  method Action  read_req(idx_type idx);
  method Action read_req2(idx_type idx);
  method Action read_req3(idx_type idx);

  method data_type read_resp();
  method data_type read_resp2();
  method data_type read_resp3();  
  
  method Action	upd(idx_type idx, data_type data);
  
endinterface


import "BVI" BRAM = module mkBRAM_Sync#(Integer low, Integer high) 
  //interface:
              (Sync_BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));

  default_clock clk(CLK);

  parameter addr_width = valueof(idx);
  parameter data_width = valueof(data);
  parameter lo = low;
  parameter hi = high;

  method DOUT1  read_resp()  ready(DOUT1_RDY);
  method DOUT2  read_resp2() ready(DOUT2_RDY);
  method DOUT3  read_resp3() ready(DOUT3_RDY);
  
  method read_req(RD1_ADDR)   enable(RD1_EN);
  method read_req2(RD2_ADDR) enable(RD2_EN);
  method read_req3(RD3_ADDR) enable(RD3_EN);

  method upd(WR_ADDR, WR_VAL) enable(WR_EN);

  schedule (read_req, read_req2, read_req3, read_resp, read_resp2, read_resp3) CF (read_req, read_req2, read_req3, read_resp, read_resp2, read_resp3, upd);
  schedule upd C upd;

endmodule

