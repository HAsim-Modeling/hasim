import FIFO::*;

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
				  mkBRAM_NonZero(low, high, False, "", False);

  return m;
endmodule


module mkBRAM_Load#(Integer low, Integer high, String fname) 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));
	   
  BRAM#(idx_type, data_type) m <- (valueof(data) == 0) ? 
                                  mkBRAM_Zero() :
				  mkBRAM_NonZero(low, high, True, fname, False);

  return m;
endmodule

module mkBRAM_LoadBin#(Integer low, Integer high, String fname) 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));
	   
  BRAM#(idx_type, data_type) m <- (valueof(data) == 0) ? 
                                  mkBRAM_Zero() :
				  mkBRAM_NonZero(low, high, True, fname, True);

  return m;
endmodule

import "BVI" BRAM = module mkBRAM_NonZero#(Integer low, Integer high, Bool load, String fname, Bool bin) 
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
  parameter loadfile = load;
  parameter filename = fname;
  parameter binary = bin;

  method DOUT read_resp() ready(DOUT_RDY) enable(DOUT_EN);
  
  method read_req(RD_ADDR) ready(RD_RDY) enable(RD_EN);
  method write(WR_ADDR, WR_VAL) enable(WR_EN);

  schedule read_req  CF (read_resp, write);
  schedule read_resp CF (read_req, write);
  schedule write     CF (read_req, read_resp);
  
  schedule read_req  C read_req;
  schedule read_resp C read_resp;
  schedule write     C write;

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


  BRAM#(idx_type, data_type) br <- mkBRAM(0, valueof(TExp#(idx)) - 1, False, "", False);

  return br;

endmodule

module mkBRAM_Full_Load#(String fname); 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));


  BRAM#(idx_type, data_type) br <- mkBRAM(0, valueof(TExp#(idx)) - 1, True, fname, False);

  return br;

endmodule

module mkBRAM_Full_LoadBin#(String fname); 
  //interface:
              (BRAM#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));


  BRAM#(idx_type, data_type) br <- mkBRAM(0, valueof(TExp#(idx)) - 1, True, fname, True);

  return br;

endmodule

module mkBRAM_NonZero_2#(Integer low, Integer high, Bool loadfile, String fname, String bin) 
  //interface:
              (BRAM_2#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));
	   
  BRAM#(idx_type, data_type) br1 <- mkBRAM(low, high, loadfile, fname, bin);
  BRAM#(idx_type, data_type) br2 <- mkBRAM(low, high, loadfile, fname, bin);
  
  method read_req1(idx) = br1.read_req(idx);
  method read_req2(idx) = br2.read_req(idx);

  method read_resp1() = br1.read_resp();
  method read_resp2() = br2.read_resp();

  method Action	write(idx_type idx, data_type data);
  
    br1.write(idx, data);
    br2.write(idx, data);
  
  endmethod
  
endmodule

module mkBRAM_2
  //interface:
              (BRAM_2#(idx_type, data_type)) 
  provisos
          (Bits#(idx_type, idx), 
	   Bits#(data_type, data),
	   Literal#(idx_type));


  BRAM_2#(idx_type, data_type) br <- mkBRAM_2(0, valueof(TExp#(idx)) - 1, False, "", False);

  return br;

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

module mkBRAM_2_Full_Load
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

