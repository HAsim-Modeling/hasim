
import fpga_components::*;
import hasim_common::*;

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



  RName maxR = maxBound;
  Bit#(prname_SZ) minInitFL_bits = zeroExtend(pack(maxR)) + 1;
  PRName minInitFL = 1; //unpack(minInitFL_bits);
  PRName maxInitFL = maxBound;
  
  Reg#(Bool) initializing <- mkReg(True);
  

  BRAM#(TokIndex, PRName)     old_pregs <- mkBRAM_Full();
  BRAM#(PRName, PRName)       fl        <- mkBRAM_Full();
  Reg#(PRName)  	      fl_read   <- mkReg(minInitFL);
  Reg#(PRName)  	      fl_write  <- mkReg(0); 
  
  Bool full = fl_read + 1 == fl_write;
  
  rule initialize (initializing);
  
    fl.write(fl_write, fl_write);
    fl_write <= fl_write + 1;
    if (fl_write == maxInitFL)
      initializing <= False;
  
  endrule
  
  rule finish_free (True);
  
    PRName reg_to_free <- old_pregs.read_resp();
    if (reg_to_free != 0)
    begin
      fl.write(fl_write, reg_to_free);
      fl_write <= fl_write + 1;
      $display("FREELIST: Freeing PR%0d", reg_to_free);
    end

  endrule
  
  method Action forward_req() if (!full && !initializing);
    fl.read_req(fl_read);
    fl_read <= fl_read + 1;
  endmethod
  
  method ActionValue#(PRName) forward_resp() if (!initializing);
    
    let rsp <- fl.read_resp();
    $display("FREELIST: Allocating PR%0d", rsp);
    return rsp;
  
  endmethod
  
  method Action setOldPReg(Token tok, PRName oldPReg) if (!initializing);
  
    old_pregs.write(tok.index, oldPReg);
  
  endmethod
  
  method Action free(Token tok) if (!initializing);
  
    old_pregs.read_req(tok.index);
  
  endmethod
  
  method Action back() if (!initializing);
  
    if (fl_read == fl_write)
    begin
      $display("ERROR: Backed up the freelist too far!");
      $finish(1);
    end
    fl_read <= fl_read - 1;
  
  endmethod
  
  method Action backTo(PRName r) if (!initializing);
  
    fl_read <= r;
  
  endmethod
  
  method PRName current();
    
    return fl_read;
    
  endmethod
endmodule
