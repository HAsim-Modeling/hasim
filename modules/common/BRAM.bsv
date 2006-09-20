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
// Modified to be ActionValue methods. upd method goes last.
// 9-12-2006, Michael
// File created
// 5-16-2006, Eric Chung
//
//


package BRAM;


interface BRAM #(type idx_type, type data_type);

  method ActionValue#(data_type) sub(idx_type idx);
  method ActionValue#(data_type) sub2(idx_type idx);
  method ActionValue#(data_type) sub3(idx_type idx);

  method Action		upd(idx_type idx, data_type data);
  
endinterface


import "BVI" BRAM = module mkBRAM#(Integer low, Integer high) 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data));

  default_clock clk(CLK);

  parameter addr_width = valueof(idx);
  parameter data_width = valueof(data);
  parameter lo = low;
  parameter hi = high;

  method D_OUT  sub(RD_ADDR)   enable(RE)  ready(RD_RDY);
  method D_OUT2 sub2(RD_ADDR2) enable(RE2) ready(RD_RDY2);
  method D_OUT3 sub3(RD_ADDR3) enable(RE3) ready(RD_RDY3);

  method upd(WR_ADDR, D_IN) enable(WE);

  schedule (sub, sub2, sub3) CF (sub, sub2, sub3);
  schedule (sub, sub2, sub3) SB upd;
endmodule


endpackage
