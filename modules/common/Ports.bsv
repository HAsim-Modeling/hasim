
interface Port_Control;

  method Bool empty();
  method Bool full();
  method Bool balanced();
  method Bool light();
  method Bool heavy();

endinterface

interface Port_Send#(type msg_T);
  
  method Action send(Maybe#(msg_T) m);
  interface Port_Control ctrl;
  
endinterface

interface Port_Receive#(type msg_T);

  method ActionValue#(Maybe#(msg_T)) receive();
  interface Port_Control ctrl;

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
  
  //XXX a temporary set of control info
    
  interface Port_Control ctrl;

    method Bool empty() = True;
    method Bool full() = False;
    method Bool balanced() = True;
    method Bool light() = False;
    method Bool heavy() = False;

  endinterface

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

  Bool full  = overflow_incr(head) == tail;
  Bool empty = head == tail;
  
  
  rule shift (!full);
  
    let d = con.receive();
    con.deq();
    
    (rs[head._read()]) <= d;
    head <= overflow_incr(head);
   
  endrule
  
  method ActionValue#(Maybe#(msg_T)) receive() if (!empty);
    
    tail <= overflow_incr(tail);
    return rs[tail._read()]._read();
    
  endmethod

  //XXX a temporary set of control info
  interface Port_Control ctrl;

    method Bool empty() = False;
    method Bool full() = True;
    method Bool balanced() = True;
    method Bool light() = False;
    method Bool heavy() = False;

  endinterface

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
      let m = con.receive();
      con.deq();
      return m;
    end
  endmethod

  
  //XXX a temporary set of control info
  interface Port_Control ctrl;

    method Bool empty() = False;
    method Bool full() = True;
    method Bool balanced() = True;
    method Bool light() = False;
    method Bool heavy() = False;

  endinterface

endmodule
