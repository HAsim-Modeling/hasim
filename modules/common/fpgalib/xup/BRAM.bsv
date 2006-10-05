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
// Used LFIFO to handle read/write problem. Methods conflict with themselves.
// 10-03-2006, Michael
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
	   
  BRAM#(idx_type, data_type) m <- (valueof(data) == 0) ? 
                                   mkBRAM_Zero() :
				   mkBRAM_NonZero(low, high);

  return m;
endmodule

import "BVI" BRAM = module mkBRAM_NonZero#(Integer low, Integer high) 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));

  default_clock clk(CLK);

  parameter addr_width = valueof(idx);
  parameter data_width = valueof(data);
  parameter lo = low;
  parameter hi = high;

  method RES1 read_resp()  enable(RES1_EN) ready(RES1_RDY);
  method RES2 read_resp2() enable(RES2_EN) ready(RES2_RDY);
  method RES3 read_resp3() enable(RES3_EN) ready(RES3_RDY);
  
  method read_req(RD1_ADDR)  enable(RD1_EN) ready(RD1_RDY);
  method read_req2(RD2_ADDR) enable(RD2_EN) ready(RD2_RDY);
  method read_req3(RD3_ADDR) enable(RD3_EN) ready(RD3_RDY);

  method upd(WR_ADDR, WR_VAL) enable(WR_EN);

  schedule (read_req, read_req2, read_req3) SB upd;
  schedule (read_resp, read_resp2, read_resp3) CF upd;
  schedule (read_req, read_req2, read_req3, read_resp, read_resp2, read_resp3) CF (read_req, read_req2, read_req3, read_resp, read_resp2, read_resp3);
  
  schedule read_req C read_req;
  schedule read_req2 C read_req2;
  schedule read_req3 C read_req3;
  schedule read_resp C read_resp;
  schedule read_resp2 C read_resp2;
  schedule read_resp3 C read_resp3;
  schedule upd C upd;

endmodule

module mkBRAM_Zero
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));

  FIFO#(data_type) q1 <- mkFIFO();
  FIFO#(data_type) q2 <- mkFIFO();
  FIFO#(data_type) q3 <- mkFIFO();
  
  method Action read_req(a);
    q1.enq(?);
  endmethod

  method Action read_req2(a);
    q2.enq(?);
  endmethod

  method Action read_req3(a);
    q3.enq(?);
  endmethod

  method ActionValue#(data_type) read_resp();
    q1.deq();
    return q1.first();
  endmethod

  method ActionValue#(data_type) read_resp2();
    q2.deq();
    return q2.first();
  endmethod

  method ActionValue#(data_type) read_resp3();
    q3.deq();
    return q3.first();
  endmethod

  method Action upd(v, x);
    noAction;
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

