
import hasim_base::*;

interface Port_Send#(type msg_T);
  
  method Action send(Maybe#(msg_T) m);
  
endinterface

interface Port_Receive#(type msg_T);

  method ActionValue#(Maybe#(msg_T)) receive();

endinterface


module [HASim_Module] mkPort_Send#(String portname)
  //interface:
              (Port_Send#(msg_T))
  provisos
          (Bits#(msg_T, msg_SZ),
	   Transmittable#(Maybe#(msg_T)));
	
  Connection_Send#(Maybe#(msg_T)) con <- mkConnection_Send(portname);
    
  method Action send(Maybe#(msg_T) m);
    
    con.send(m);
    
  endmethod
    
endmodule

module [HASim_Module] mkPort_Receive#(String portname, Integer latency)
  //interface:
              (Port_Receive#(msg_T))
      provisos
	        (Bits#(msg_T, msg_SZ),
		 Transmittable#(Maybe#(msg_T)));
  
  let p <- (latency != 1) ? mkPort_Receive_Buffered(portname, latency, 0) : mkPort_Receive_L1(portname);
  return p;

endmodule

module [HASim_Module] mkPort_Receive_Buffered#(String portname, Integer latency, Integer extra_buffering)
    //interface:
                (Port_Receive#(msg_T))
      provisos
	        (Bits#(msg_T, msg_SZ),
		 Transmittable#(Maybe#(msg_T)));

  Connection_Receive#(Maybe#(msg_T)) con <- mkConnection_Receive(portname);
   
  Integer rMax = latency + extra_buffering + 1;
  
  if (rMax > 255)
    error("Total Port buffering cannot currently exceed 255.");
  
  Reg#(Maybe#(msg_T)) rs[rMax];
  
  for (Integer x = 0; x < rMax; x = x + 1)
    rs[x] <- mkReg(Invalid);

  Reg#(Bit#(8)) head <- mkReg(fromInteger(latency));
  Reg#(Bit#(8)) tail <- mkReg(0);
  
  function Bit#(n) overflow_incr(Bit#(n) x);
    
    let tmp = x + 1;
    return (tmp == fromInteger(rMax)) ? 0 : tmp;
  endfunction

  Bool full  = head == overflow_incr(tail);
  Bool empty = head == tail;
  
  
  rule shift (!full);
  
    let d <- con.receive();
    (rs[head._read()]) <= d;
    head <= overflow_incr(head);
   
  endrule
  
  method ActionValue#(Maybe#(msg_T)) receive() if (!empty);
    
    tail <= overflow_incr(tail);
    return rs[tail._read()]._read();
    
  endmethod

endmodule

//Port optimized for latency 1

module [HASim_Module] mkPort_Receive_L1#(String portname)
    //interface:
                (Port_Receive#(msg_T))
      provisos
	        (Bits#(msg_T, msg_SZ),
		 Transmittable#(Maybe#(msg_T)));

  Connection_Receive#(Maybe#(msg_T)) con <- mkConnection_Receive(portname);
  Reg#(Bool) initval <- mkReg(True);
     
  method ActionValue#(Maybe#(msg_T)) receive();
    if (initval)
    begin
      initval <= False;
      return Invalid;
    end
    else
    begin
      let m <- con.receive();
      return m;
    end
  endmethod
endmodule
