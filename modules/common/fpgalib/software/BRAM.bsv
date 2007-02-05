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

//One RAM.
interface BRAM#(type idx_type, type data_type);

  method Action read_req(idx_type idx);

  method ActionValue#(data_type) read_resp();

  method Action	write(idx_type idx, data_type data);
  
endinterface


//Two RAMs.
interface BRAM_2#(type idx_type, type data_type);

  method Action read_req1(idx_type idx);
  method Action read_req2(idx_type idx);

  method ActionValue#(data_type) read_resp1();
  method ActionValue#(data_type) read_resp2();

  method Action	write(idx_type idx, data_type data);
  
endinterface

//Three RAMs.
interface BRAM_3#(type idx_type, type data_type);

  method Action read_req1(idx_type idx);
  method Action read_req2(idx_type idx);
  method Action read_req3(idx_type idx);

  method ActionValue#(data_type) read_resp1();
  method ActionValue#(data_type) read_resp2();
  method ActionValue#(data_type) read_resp3();

  method Action	write(idx_type idx, data_type data);
  
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

module mkBRAM_NonZero#(Integer low, Integer high) 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));

  FIFO#(Bit#(0)) processing_read <- mkFIFO();

  Sync_BRAM#(idx_type, data_type) br <- mkBRAM_Sync(low, high);

  method Action  read_req(idx_type idx);
    processing_read.enq(?);
    br.read_req(idx);
  endmethod
    
  method ActionValue#(data_type) read_resp();
    processing_read.deq();
    return br.read_rsp();
  endmethod
  
  method Action	write(idx_type idx, data_type data);
    br.write(idx, data);
  endmethod

endmodule

//Synchronous BRAM primitive
interface Sync_BRAM#(type idx_type, type data_type);

  method Action read_req(idx_type idx);

  method data_type read_rsp();
  
  method Action	write(idx_type idx, data_type data);
  
endinterface


module mkBRAM_Sync#(Integer low, Integer high) 
  //interface:
              (Sync_BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));

  RegFile#(idx_type, data_type) br <- mkRegFile(fromInteger(low), fromInteger(high));
  Reg#(data_type) read_data <- mkRegU();
  Reg#(Bool)  ready <- mkReg(False);

  method Action  read_req(idx_type idx);
  
    ready <= True;
    read_data <= br.sub(idx);
    
  endmethod
  
  method data_type read_rsp()  if (ready);
    return read_data;
  endmethod 

  method Action	write(idx_type idx, data_type data);
  
    br.upd(idx, data);
  
  endmethod
  
endmodule

module mkBRAM_Zero
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));
  
  FIFO#(data_type) q <- mkFIFO();

  method Action read_req(idx_type i);
     q.enq(?);
  endmethod

  method Action write(idx_type i, data_type d);
    noAction;
  endmethod

  method ActionValue#(data_type) read_resp();
    q.deq();
    return q.first();
  endmethod

endmodule

module mkBRAM_Full 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));


  BRAM#(idx_type, data_type) br <- mkBRAM(0, valueof(TExp#(idx)) - 1);

  return br;

endmodule


module mkBRAM_2#(Integer low, Integer high) 
  //interface:
              (BRAM_2#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));
	   
  BRAM#(idx_type, data_type) br1 <- mkBRAM(low, high);
  BRAM#(idx_type, data_type) br2 <- mkBRAM(low, high);
  
  method read_req1(idx) = br1.read_req(idx);
  method read_req2(idx) = br2.read_req(idx);

  method read_resp1() = br1.read_resp();
  method read_resp2() = br2.read_resp();

  method Action	write(idx_type idx, data_type data);
  
    br1.write(idx, data);
    br2.write(idx, data);
  
  endmethod
  
endmodule

module mkBRAM_2_Full 
  //interface:
              (BRAM_2#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));


  BRAM_2#(idx_type, data_type) br <- mkBRAM_2(0, valueof(TExp#(idx)) - 1);

  return br;

endmodule


module mkBRAM_3#(Integer low, Integer high) 
  //interface:
              (BRAM_3#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));
	   
  BRAM#(idx_type, data_type) br1 <- mkBRAM(low, high);
  BRAM#(idx_type, data_type) br2 <- mkBRAM(low, high);
  BRAM#(idx_type, data_type) br3 <- mkBRAM(low, high);
  
  method read_req1(idx) = br1.read_req(idx);
  method read_req2(idx) = br2.read_req(idx);
  method read_req3(idx) = br3.read_req(idx);

  method read_resp1() = br1.read_resp();
  method read_resp2() = br2.read_resp();
  method read_resp3() = br3.read_resp();

  method Action	write(idx_type idx, data_type data);
  
    br1.write(idx, data);
    br2.write(idx, data);
    br3.write(idx, data);
  
  endmethod
  
endmodule

module mkBRAM_3_Full 
  //interface:
              (BRAM_3#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));


  BRAM_3#(idx_type, data_type) br <- mkBRAM_3(0, valueof(TExp#(idx)) - 1);

  return br;

endmodule
