
import hasim_common::*;
import hasim_base::*;
import hasim_fpgalib::*;

import hasim_funcp_base::*;
import hasim_isa::*;


interface FreeList;
  
  method Action forward_req();
  method ActionValue#(PRName) forward_resp();
  method Action back();
  method Action free(PRName reg_to_free);
  method Action backTo(PRName r);
  method PRName current();
  
endinterface

module [HASim_Module] mkFreeList
    //interface:
                (FreeList)
    provisos
            (Bits#(RName,       rname_SZ),
 	     Bits#(PRName,      prname_SZ),
	     Bits#(Value,       value_SZ),
	     Bits#(Token,       token_SZ));



  RName maxR = maxBound;
  Bit#(prname_SZ) minInitFL_bits = zeroExtend(pack(maxR)) + 1;
  PRName minInitFL = unpack(minInitFL_bits);
  PRName maxInitFL = maxBound;
  
  Reg#(Bool) initializing <- mkReg(True);
  

  BRAM#(PRName, PRName)       fl  <- mkBRAM_Full();
  Reg#(PRName)  	      fl_read   <- mkReg(minInitFL);
  Reg#(PRName)  	      fl_write  <- mkReg(minInitFL); 
  
  Bool full = fl_read + 1 == fl_write;
  
  rule initialize (initializing);
  
    fl.upd(fl_write, fl_write);
    fl_write <= fl_write + 1;
    if (fl_write == maxInitFL)
      initializing <= False;
  
  endrule
  
  method Action forward_req() if (!full && !initializing);
    fl.read_req(fl_read);
    fl_read <= fl_read + 1;
  endmethod
  
  method ActionValue#(PRName) forward_resp() if (!initializing);
    
    let rsp <- fl.read_resp();
    return rsp;
  
  endmethod
  
  method Action free(PRName reg_to_free) if (!initializing);
  
    fl.upd(fl_write, reg_to_free);
    fl_write <= fl_write + 1;
  
  endmethod
  
  method Action back() if (!initializing);
  
    fl_read <= fl_read - 1;
  
  endmethod
  
  method Action backTo(PRName r) if (!initializing);
  
    fl_read <= r;
  
  endmethod
  
  method PRName current();
    
    return fl_read;
    
  endmethod
endmodule
