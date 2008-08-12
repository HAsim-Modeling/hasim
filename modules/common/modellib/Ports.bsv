import RegFile::*;
import FIFOF::*;

import hasim_common::*;
import soft_connections::*;

interface Port_Control;

  method Bool empty();
  method Bool full();
  method Bool balanced();
  method Bool light();
  method Bool heavy();

endinterface

interface PortReceive#(type msgT, numeric type latency, numeric type bandwidth);
    method ActionValue#(Maybe#(msgT)) receive();
    interface Port_Control ctrl;
endinterface

interface PortSend#(type msgT);
    method Action send(Maybe#(msgT) m);
    interface Port_Control ctrl;
endinterface

module [HASIM_MODULE] mkPortTimeSharedReceive#(String portName)(PortReceive#(msgT, latency, bandwidth))
    provisos (Bits#(msgT, msgSz),
              Transmittable#(Maybe#(msgT)),
              Add#(latency, 1, l1),
              Mul#(latency, bandwidth, b0),
              Mul#(l1, bandwidth, b),
              Add#(b, 1, b1),
              Log#(b1, bLog));

    Connection_Receive#(Maybe#(msgT)) con <- mkConnection_Receive(portName);

    Reg#(Bit#(bLog)) lat <- mkReg(fromInteger(valueOf(b1)));

    FIFOF#(Maybe#(msgT)) fifo <- mkSizedFIFOF(valueOf(b));

    rule fillFifo(True);
        fifo.enq(con.receive());
        con.deq();
    endrule

    method ActionValue#(Maybe#(msgT)) receive();
        if(lat == 0)
        begin
            fifo.deq();
            return fifo.first();
        end
        else
        begin
            lat <= lat - 1;
            return tagged Invalid;
        end
    endmethod

    interface Port_Control ctrl;
        method Bool empty() = False;
        method Bool full() = True;
        method Bool balanced() = True;
        method Bool light() = False;
        method Bool heavy() = False;
    endinterface
endmodule

module [HASIM_MODULE] mkPortTimeSharedSend#(String portName)(PortSend#(msgT))
    provisos (Bits#(msgT, msgSz),
              Transmittable#(Maybe#(msgT)));

    Connection_Send#(Maybe#(msgT)) con <- mkConnection_Send(portName);

    method Action send(Maybe#(msgT) msg);
        con.send(msg);
    endmethod

    interface Port_Control ctrl;
        method Bool empty() = True;
        method Bool full() = False;
        method Bool balanced() = True;
        method Bool light() = False;
        method Bool heavy() = False;
    endinterface
endmodule

interface Port_Send#(type msg_T);
  
  method Action send(Maybe#(msg_T) m);
  interface Port_Control ctrl;
  
endinterface

interface Port_Receive#(type msg_T);

  method ActionValue#(Maybe#(msg_T)) receive();
  interface Port_Control ctrl;

endinterface


module [HASIM_MODULE] mkPort_Send#(String portname)
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

module [HASIM_MODULE] mkPort_Send_Bypassed#(String portname)
  //interface:
              (Port_Send#(msg_T))
  provisos
          (Bits#(msg_T, msg_SZ),
           Transmittable#(Maybe#(msg_T)));
        
  Connection_Send#(Maybe#(msg_T)) con <- mkConnection_Send_Bypassed(portname);
    
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

module [HASIM_MODULE] mkPort_Receive#(String portname, Integer latency)
  //interface:
              (Port_Receive#(msg_T))
      provisos
                (Bits#(msg_T, msg_SZ),
                 Transmittable#(Maybe#(msg_T)));
  
  let p <- case (latency)
             0: mkPort_Receive_L0(portname);
             1: mkPort_Receive_L1(portname);
             default: mkPort_Receive_Buffered(portname, latency, 0);
           endcase;
 
  return p;

endmodule

module [HASIM_MODULE] mkPort_Receive_Buffered#(String portname, Integer latency, Integer extra_buffering)
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

module [HASIM_MODULE] mkPort_Receive_Waterlevel#(String portname, Integer latency)
    //interface:
                (Port_Receive#(msg_T))
      provisos
	        (Bits#(msg_T, msg_SZ),
		 Transmittable#(Maybe#(msg_T)));

  Connection_Receive#(Maybe#(msg_T)) con <- mkConnection_Receive(portname);
   
  RegFile#(Bit#(10), Maybe#(msg_T)) rs <- mkRegFileFull();
  Integer rMax = 1024;

  Reg#(Bool) initializing <- mkReg(True);
  Reg#(Bit#(10)) head <- mkReg(0);
  Reg#(Bit#(10)) tail <- mkReg(0);
  Reg#(Bit#(10)) waterlevel <- mkReg(fromInteger(latency));
  
  String         stat_name = strConcat(portname, " Maximum Elements");
  Stat           level_stat <- mkStatCounter(0); //XXX this needs to use dictionaries.
  
  Bool full  = (head + 1 == tail);
  Bool empty = (head == tail);
    
  rule initvals (initializing);
  
    rs.upd(head, tagged Invalid);
    let newhead = head + 1;
    if (newhead == fromInteger(latency))
      initializing <= False;
    
    head <= newhead;
  
  endrule
  
  rule shift (!full && !initializing);
  
    let d = con.receive();
    con.deq();
    
    rs.upd(head, d);
    let newhead = head + 1;
    Bit#(10) current_elems = newhead - tail;
    if (current_elems > waterlevel)
    begin
      waterlevel <= waterlevel + 1;
      level_stat.incr();
    end
    head <= newhead;
   
  endrule
  
  method ActionValue#(Maybe#(msg_T)) receive() if (!empty && !initializing);
    
    let newtail = tail + 1;
    tail <= newtail;
    return rs.sub(tail);
    
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

//Port optimized for latency 0

module [HASIM_MODULE] mkPort_Receive_L0#(String portname)
    //interface:
                (Port_Receive#(msg_T))
      provisos
                (Bits#(msg_T, msg_SZ),
                 Transmittable#(Maybe#(msg_T)));

  Connection_Receive#(Maybe#(msg_T)) con <- mkConnection_Receive(portname);
     
  method ActionValue#(Maybe#(msg_T)) receive();
  
    con.deq();
    return con.receive();
    
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

module [HASIM_MODULE] mkPort_Receive_L1#(String portname)
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
