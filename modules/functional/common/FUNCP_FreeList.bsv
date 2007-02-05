
import hasim_common::*;
import hasim_base::*;
import hasim_fpgalib::*;

import hasim_funcp_base::*;
import hasim_isa::*;


interface FreeList;
  
  method Action forward_req();
  method ActionValue#(PRName) forward_resp();
  method Action back();
  method Action backTo(PRName r);
  method PRName current();
  method Action setOldPReg(Token t, PRName oldPReg);
  method Action free(Token t);
  
endinterface

module [HASim_Module] mkFreeList
    //interface:
                (FreeList)
    provisos
            (Bits#(RName,       rname_SZ),
 	     Bits#(PRName,      prname_SZ),
	     Bits#(Value,       value_SZ),
	     Bits#(Token,       token_SZ));


  RName maxR = pack(REG_LAST);
  Bit#(prname_SZ) minInitFL_bits = zeroExtend(pack(maxR));
  PRName minInitFL = unpack(minInitFL_bits);
  PRName maxInitFL = maxBound;
  
  Reg#(Bool) initializing <- mkReg(True);
  

  BRAM#(TokIndex, PRName)     old_pregs <- mkBRAM_Full();
  BRAM#(PRName, PRName)       fl        <- mkBRAM_Full();
  Reg#(PRName)  	      fl_read   <- mkReg(minInitFL);
  Reg#(PRName)  	      fl_write  <- mkReg(minInitFL); 
  
  Bool full = fl_read + 1 == fl_write;
  
  rule initialize (initializing);
  
    fl.write(fl_write, fl_write);
    fl_write <= fl_write + 1;
    if (fl_write == maxInitFL)
      initializing <= False;
  
  endrule
  
  rule finish_free (!initializing);
  
    PRName reg_to_free <- old_pregs.read_resp();
    fl.write(fl_write, reg_to_free);
    fl_write <= fl_write + 1;
    $display("FREELIST freelist[%0d]=%0d, fl_write=%0d, fl_read=%0d", fl_write, reg_to_free, fl_write+1, fl_read);

  endrule
  
  method Action forward_req() if (!full && !initializing);
    fl.read_req(fl_read);
    fl_read <= fl_read + 1;
    $display("FREELIST req %0d, fl_write=%0d, fl_read=%0d", fl_read, fl_write, fl_read+1);

  endmethod
  
  method ActionValue#(PRName) forward_resp() if (!initializing);
    
    let rsp <- fl.read_resp();
    $display("FREELIST resp=%0d, fl_write=%0d, fl_read=%0d", rsp, fl_write, fl_read);
    return rsp;
  
  endmethod
  
  method Action setOldPReg(Token tok, PRName oldPReg) if (!initializing);
  
    old_pregs.write(tok.index, oldPReg);
    $display("FREELIST oldPReg[%0d]=%0d, fl_write=%0d, fl_read=%0d", tok.index, oldPReg, fl_write, fl_read);
  
  endmethod
  
  method Action free(Token tok) if (!initializing);
  
    old_pregs.read_req(tok.index);
    $display("FREELIST free oldPReg [%0d], fl_write=%0d, fl_read=%0d", tok.index, fl_write, fl_read);
  
  endmethod
  
  method Action back() if (!initializing);
  
    fl_read <= fl_read - 1;
    $display("FREELIST back, fl_write=%0d, fl_read=%0d", fl_write, fl_read-1);
  
  endmethod
  
  method Action backTo(PRName r) if (!initializing);
  
    fl_read <= r;
    $display("FREELIST back_to, fl_write=%0d, fl_read=%0d", fl_write, r);
  
  endmethod
  
  method PRName current();
    
    return fl_read;
    
  endmethod
endmodule
