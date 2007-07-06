
import fpga_components::*;
import hasim_common::*;

import hasim_isa::*;


interface FreeList;
  
  method Action forward_req();
  method ActionValue#(PRName) forward_resp();
  method Action back();
  method Action backN(PRName n);
  method Action backTo(PRName r);
  method PRName current();
  method Action setOldPReg(Token t, PRName oldPReg);
  method Action free(Token t);
  
endinterface

module [HASim_Module] mkFreeList#(File debug_log, Tick curCC)
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
  

  BRAM#(TokIndex, PRName)     old_pregs <- mkBRAM_Full();
  BRAM#(PRName, PRName)       fl        <- mkBRAM_Full();
  Reg#(PRName)  	      fl_read   <- mkReg(minInitFL);
  Reg#(PRName)  	      fl_write  <- mkReg(0); 

  Reg#(Bit#(2))             reqCount <- mkReg(0);
  
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
      $fdisplay(debug_log, "[%d]: FREELIST: Freeing PR%0d %0d", curCC, reg_to_free, fl_write + 1);
    end

  endrule
  
  method Action forward_req() if (!full && !initializing);
    fl.read_req(fl_read);
    fl_read <= fl_read + 1;
    $fdisplay(debug_log, "[%d]: FREELIST: Requesting %0d", curCC, fl_read);
    reqCount <= reqCount + 1;
  endmethod
  
  method ActionValue#(PRName) forward_resp() if (!initializing);
    
    let rsp <- fl.read_resp();
    $fdisplay(debug_log, "[%d]: FREELIST: Allocating PR%0d %0d", curCC, rsp, fl_read);
    reqCount <= reqCount - 1;
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
  
  method Action backN(PRName n) if (!initializing);
    
    let new_read = fl_read - n;
  
    if (((fl_read > fl_write) && (new_read < fl_write)) || (fl_read < fl_write) && (new_read > fl_write))
    begin
      $display("ERROR: Backed up the freelist too far! (N = %0d)", n);
      $finish(1);
    end
    
    fl_read <= new_read;
  
  endmethod
  
  method Action backTo(PRName r) if (!initializing && reqCount == 0);
  
    $fdisplay(debug_log, "went back to PR%0d", r);
    if(fl_read > fl_write && r < fl_write || fl_read < fl_write && r < fl_write && r > fl_read)
        $display("ERROR: Backed up the freelist too far! (r = %0d)", r);
    fl_read <= r;
  
  endmethod
  
  method PRName current();
    
    return fl_read;
    
  endmethod
endmodule
