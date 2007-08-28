import RegFile::*;
import fpga_components::*;


(* descending_urgency= "deq, enq, clear" *)

module mkSizedFIFO_DRAM#(Integer depth) 
  //
  //interface:
              (FIFO#(data_T))
  provisos
          (Bits#(data_T, data_SZ));

  if (depth > 255)
    error("DRAM FIFO buffering depth cannot currently exceed 255.");
  
  RegFile#(Bit#(8), data_T) rs <- mkRegFile(0, fromInteger(depth));
  
  Reg#(Bit#(8)) head <- mkReg(0);
  Reg#(Bit#(8)) tail <- mkReg(0);
  
  function Bit#(n) overflow_incr(Bit#(n) x);
    
    let tmp = x + 1;
    return (tmp == fromInteger(depth)) ? 0 : tmp;

  endfunction

  Bool full  = head == overflow_incr(tail);
  Bool empty = head == tail;
    
  method Action enq(data_T d) if (!full);
  
    rs.upd(head, d);
    head <= overflow_incr(head);
   
  endmethod  
  
  method data_T first() if (!empty);
    
    return rs.sub(tail);
  
  endmethod   
  
  method Action deq();
  
    tail <= overflow_incr(tail);
    
  endmethod

  method Action clear();
  
    tail <= head;
    
  endmethod

endmodule


module mkSizedFIFO_BRAM#(Integer depth) 
  //
  //interface:
              (FIFO#(data_T))
  provisos
          (Bits#(data_T, data_SZ));
	  
  if (depth > 1024)
    error("BRAM FIFO buffering depth cannot currently exceed 1024.");
  
  BRAM#(Bit#(10), data_T) bram <- mkBRAM(0, depth);
    
  Reg#(Bit#(10)) head <- mkReg(0);
  Reg#(Bit#(10)) tail <- mkReg(0);
  
  PulseWire deqW <- mkPulseWire();
  PulseWire enqW <- mkPulseWire();
  Wire#(data_T) bufW <- mkWire();
  
  function Bit#(n) overflow_incr(Bit#(n) x);
    
    let tmp = x + 1;
    return (tmp == fromInteger(depth)) ? 0 : tmp;
    
  endfunction

  Bool full  = head == overflow_incr(tail);
  Bool empty = head == tail;
  
  (* descending_urgency= "deq, enq, clear, prebuf" *)
   
  rule prebuf_req (!empty || enqW);
    
    if (deqW)
    begin
    
      let newtail = overflow_incr(tail);
      bram.read_req(newtail);
      tail <= newtail;
    
    end
    else
    begin
      bram.read_req(tail);
    end
  
  endrule 
  
  rule prebuf_resp (True);
  
    let p <- bram.read_resp();
    bufW <= p;

  endrule
  
  
  method Action enq(data_T d) if (!full);
  
    enqW.send();
  
    bram.write(head, d);
    head <= overflow_incr(head);
   
  endmethod
  
  method data_T first();
    
    return bufW;
  
  endmethod   
  
  method Action deq() if (!empty);
  
    deqW.send();
    
  endmethod

  method Action clear();
  
    tail <= head;
    
  endmethod

endmodule
