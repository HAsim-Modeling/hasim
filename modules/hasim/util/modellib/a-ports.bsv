//
// Copyright (C) 2008 Massachusetts Institute of Technology
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//
// Tokens are the main way for HAsim to track data across simulator      
// partitions. The token type includes an index for token tables, epochs,
// and scratchpads which partitions can use as they see fit.             

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/fpga_components.bsh"

import FIFOF::*;
import Vector::*;
import ModuleCollect::*;

interface PORT_CONTROL;

  method Bool empty();
  method Bool full();
  method Bool balanced();
  method Bool light();
  method Bool heavy();

endinterface

typedef Vector#(NUM_CONTEXTS, PORT_CONTROL) PORT_CONTROLS;

interface PORT_SEND#(type msg_T);
  
  method Action send(Maybe#(msg_T) m);
  interface PORT_CONTROL ctrl;
  
endinterface

interface PORT_RECV#(type msg_T);

  method ActionValue#(Maybe#(msg_T)) receive();
  interface PORT_CONTROL ctrl;

endinterface

interface PORT_SEND_MULTICTX#(type t_MSG);
  
  method Action send(CONTEXT_ID ctx, Maybe#(t_MSG) m);
  interface PORT_CONTROLS ctrl;
  
endinterface

interface PORT_RECV_MULTICTX#(type t_MSG);

  method ActionValue#(Maybe#(t_MSG)) receive(CONTEXT_ID ctx);
  interface PORT_CONTROLS ctrl;

endinterface

module [HASIM_MODULE] mkPortSend#(String portname)
  //interface:
              (PORT_SEND#(msg_T))
  provisos
          (Bits#(msg_T, msg_SZ),
           Transmittable#(Maybe#(msg_T)));
        
  Connection_Send#(Maybe#(msg_T)) con <- mkConnection_Send(portname);
    
  //XXX a temporary set of control info
    
  interface PORT_CONTROL ctrl;

    method Bool empty() = True;
    method Bool full() = !con.notFull;
    method Bool balanced() = True;
    method Bool light() = False;
    method Bool heavy() = False;

  endinterface
  
  method Action send(Maybe#(msg_T) m);
    
    con.send(m);
    
  endmethod
  
endmodule

module [HASIM_MODULE] mkPortRecv#(String portname, Integer latency)
  //interface:
              (PORT_RECV#(msg_T))
      provisos
                (Bits#(msg_T, msg_SZ),
                 Transmittable#(Maybe#(msg_T)));
  
  let p <- case (latency)
             0: mkPortRecv_L0(portname);
             1: mkPortRecv_L1(portname);
             default: mkPortRecv_Buffered(portname, latency, 0);
           endcase;
 
  return p;

endmodule

module [HASIM_MODULE] mkPortRecv_Buffered#(String portname, Integer latency, Integer extra_buffering)
    //interface:
                (PORT_RECV#(msg_T))
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
  Bit#(8) numElems = head - tail;
  
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
  
  //XXX a temporary set of control info
  interface PORT_CONTROL ctrl;

        method Bool empty() = empty;
        method Bool full() = full;
        method Bool balanced() = True;
        method Bool light() = False;
        method Bool heavy() = False;

  endinterface

  method ActionValue#(Maybe#(msg_T)) receive() if (!empty);
    
    tail <= overflow_incr(tail);
    return rs[tail._read()]._read();
    
  endmethod

endmodule


//Port optimized for latency 0

module [HASIM_MODULE] mkPortRecv_L0#(String portname)
    //interface:
                (PORT_RECV#(msg_T))
      provisos
                (Bits#(msg_T, msg_SZ),
                 Transmittable#(Maybe#(msg_T)));

  Connection_Receive#(Maybe#(msg_T)) con <- mkConnection_Receive(portname);
     
  //XXX a temporary set of control info
  interface PORT_CONTROL ctrl;

    method Bool empty() = empty;
    method Bool full() = True;
    method Bool balanced() = True;
    method Bool light() = False;
    method Bool heavy() = False;

  endinterface

  method ActionValue#(Maybe#(msg_T)) receive();
  
    con.deq();
    return con.receive();
    
  endmethod
  
endmodule

//Port optimized for latency 1

module [HASIM_MODULE] mkPortRecv_L1#(String portname)
    //interface:
                (PORT_RECV#(msg_T))
      provisos
                (Bits#(msg_T, msg_SZ),
                 Transmittable#(Maybe#(msg_T)));

  Connection_Receive#(Maybe#(msg_T)) con <- mkConnection_Receive(portname);
  Reg#(Bool) initval <- mkReg(True);
     

  //XXX a temporary set of control info
  interface PORT_CONTROL ctrl;

    method Bool empty() = con.notEmpty;
    method Bool full() = True;
    method Bool balanced() = True;
    method Bool light() = False;
    method Bool heavy() = False;

  endinterface

  
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

endmodule



module [HASIM_MODULE] mkPortSend_MultiCtx#(String portname)
    //interface:
        (PORT_SEND_MULTICTX#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Transmittable#(Maybe#(t_MSG)));

    Vector#(NUM_CONTEXTS, PORT_SEND#(t_MSG)) ports = newVector();
    Vector#(NUM_CONTEXTS, PORT_CONTROL) portCtrls = newVector();

    for (Integer x = 0; x < valueOf(NUM_CONTEXTS); x = x + 1)
    begin
      ports[x] <- mkPortSendUG(portname + "_" + integerToString(x));
      portCtrls[x] = ports[x].ctrl;
    end

    interface ctrl = portCtrls;

    method Action send(CONTEXT_ID ctx, Maybe#(t_MSG) m);

      ports[ctx].send(m);

    endmethod
  
endmodule

module [HASIM_MODULE] mkPortRecv_MultiCtx#(String portname, Integer latency)
    //interface:
        (PORT_RECV_MULTICTX#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Transmittable#(Maybe#(t_MSG)));


    Vector#(NUM_CONTEXTS, PORT_RECV#(t_MSG)) ports = newVector();
    Vector#(NUM_CONTEXTS, PORT_CONTROL) portCtrls = newVector();

    for (Integer x = 0; x < valueOf(NUM_CONTEXTS); x = x + 1)
    begin
      ports[x] <- mkPortRecvUG(portname + "_" + integerToString(x), latency);
      portCtrls[x] = ports[x].ctrl;
    end

    interface ctrl = portCtrls;

    method ActionValue#(Maybe#(t_MSG)) receive(CONTEXT_ID ctx);

      let res <- ports[ctx].receive();
      return res;

    endmethod

endmodule


module [HASIM_MODULE] mkPortSendGuarded_MultiCtx#(String portname)
    //interface:
        (PORT_SEND_MULTICTX#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Transmittable#(Maybe#(t_MSG)));

    Vector#(NUM_CONTEXTS, PORT_SEND#(t_MSG)) ports = newVector();
    Vector#(NUM_CONTEXTS, PORT_CONTROL) portCtrls = newVector();

    for (Integer x = 0; x < valueOf(NUM_CONTEXTS); x = x + 1)
    begin
      ports[x] <- mkPortSend(portname + "_" + integerToString(x));
      portCtrls[x] = ports[x].ctrl;
    end

    interface ctrl = portCtrls;

    method Action send(CONTEXT_ID ctx, Maybe#(t_MSG) m);

      ports[ctx].send(m);

    endmethod
  
endmodule

module [HASIM_MODULE] mkPortRecvGuarded_MultiCtx#(String portname, Integer latency)
    //interface:
        (PORT_RECV_MULTICTX#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Transmittable#(Maybe#(t_MSG)));


    Vector#(NUM_CONTEXTS, PORT_RECV#(t_MSG)) ports = newVector();
    Vector#(NUM_CONTEXTS, PORT_CONTROL) portCtrls = newVector();

    for (Integer x = 0; x < valueOf(NUM_CONTEXTS); x = x + 1)
    begin
      ports[x] <- mkPortRecv(portname + "_" + integerToString(x), latency);
      portCtrls[x] = ports[x].ctrl;
    end

    interface ctrl = portCtrls;

    method ActionValue#(Maybe#(t_MSG)) receive(CONTEXT_ID ctx);

      let res <- ports[ctx].receive();
      return res;

    endmethod

endmodule

module [HASIM_MODULE] mkPortSendUG#(String portname)
  //interface:
              (PORT_SEND#(t_MSG))
  provisos
          (Bits#(t_MSG, t_MSG_SZ),
           Transmittable#(Maybe#(t_MSG)));
        
  Connection_Send#(Maybe#(t_MSG)) con <- mkConnectionSendUG(portname);
    
  interface PORT_CONTROL ctrl;

        method Bool empty() = True;
        method Bool full() = !con.notFull();
        method Bool balanced() = True;
        method Bool light() = False;
        method Bool heavy() = False;

  endinterface
  
  method Action send(Maybe#(t_MSG) m);
    
    con.send(m);
    if (!con.notFull)
        $display("WARNING: Overflow for unguarded send Port %s. Dropped Message occurred!", portname);
    
  endmethod
  
endmodule

module [HASIM_MODULE] mkPortRecvUG#(String portname, Integer latency)
    //interface:
        (PORT_RECV#(msg_T))
        provisos
                  (Bits#(msg_T, msg_SZ),
                   Transmittable#(Maybe#(msg_T)));

    let p <- case (latency)
               0: mkPortRecvUG_L0(portname);
               1: mkPortRecvUG_L1(portname);
               default: mkPortRecvUG_Buffered(portname, latency, 0);
             endcase;

    return p;

endmodule
module [HASIM_MODULE] mkPortRecvUG_Buffered#(String portname, Integer latency, Integer extra_buffering)
    //interface:
                (PORT_RECV#(msg_T))
      provisos
                (Bits#(msg_T, msg_SZ),
                 Transmittable#(Maybe#(msg_T)));

  Connection_Receive#(Maybe#(msg_T)) con <- mkConnectionRecvUG(portname);
   
  Integer rMax = latency + extra_buffering + 1;
  
  if (rMax > 255)
    error("Total Port buffering cannot currently exceed 255. Port: " + portname);
  
  Reg#(Maybe#(msg_T)) rs[rMax];
  
  for (Integer x = 0; x < rMax; x = x + 1)
    rs[x] <- mkReg(Invalid);

  Reg#(Bit#(8)) head <- mkReg(fromInteger(latency));
  Reg#(Bit#(8)) tail <- mkReg(0);
  Bit#(8) numElems = head - tail;
  
  function Bit#(n) overflow_incr(Bit#(n) x);
    
    let tmp = x + 1;
    return (tmp == fromInteger(rMax)) ? 0 : tmp;
  endfunction

  Bool full  = overflow_incr(head) == tail;
  Bool empty = head == tail;
  
  
  rule shift (!full && !con.notEmpty());
  
    let d = con.receive();
    con.deq();
    
    (rs[head._read()]) <= d;
    head <= overflow_incr(head);
   
  endrule
  
  //XXX a temporary set of control info
  interface PORT_CONTROL ctrl;

        method Bool empty() = empty;
        method Bool full() = full;
        method Bool balanced() = True;
        method Bool light() = False;
        method Bool heavy() = False;

  endinterface

  method ActionValue#(Maybe#(msg_T)) receive();
    
    if (empty)
        $display("WARNING: Underflow on unguarded receive port %s! Junk data added!", portname);

    tail <= overflow_incr(tail);
    return rs[tail._read()]._read();
    
  endmethod

endmodule

//Port optimized for latency 0

module [HASIM_MODULE] mkPortRecvUG_L0#(String portname)
    //interface:
                (PORT_RECV#(msg_T))
      provisos
                (Bits#(msg_T, msg_SZ),
                 Transmittable#(Maybe#(msg_T)));

  Connection_Receive#(Maybe#(msg_T)) con <- mkConnectionRecvUG(portname);
     
  //XXX a temporary set of control info
  interface PORT_CONTROL ctrl;

        method Bool empty() = !con.notEmpty();
        method Bool full() = True;
        method Bool balanced() = True;
        method Bool light() = False;
        method Bool heavy() = False;

  endinterface

  method ActionValue#(Maybe#(msg_T)) receive();
  
    if (!con.notEmpty)
        $display("WARNING: Underflow on unguarded receive port %s! Junk data added!", portname);

    con.deq();
    return con.receive();
    
  endmethod
  
endmodule

//Port optimized for latency 1

module [HASIM_MODULE] mkPortRecvUG_L1#(String portname)
    //interface:
                (PORT_RECV#(msg_T))
      provisos
                (Bits#(msg_T, msg_SZ),
                 Transmittable#(Maybe#(msg_T)));

  Connection_Receive#(Maybe#(msg_T)) con <- mkConnectionRecvUG(portname);
  Reg#(Bool) initval <- mkReg(True);
     

  //XXX a temporary set of control info
  interface PORT_CONTROL ctrl;

        method Bool empty() = !(con.notEmpty || initval);
        method Bool full() = True;
        method Bool balanced() = True;
        method Bool light() = False;
        method Bool heavy() = False;

  endinterface

  method ActionValue#(Maybe#(msg_T)) receive();

    if (initval)
    begin
      initval <= False;
      return Invalid;
    end
    else
    begin
      if (!con.notEmpty)
          $display("WARNING: Underflow on unguarded receive port %s! Junk data added!", portname);
      let m = con.receive();
      con.deq();
      return m;
    end
  endmethod

endmodule

module [Connected_Module] mkConnectionSendUG#(String portname)
    //interface:
                (Connection_Send#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Transmittable#(msg_T));

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.
  
  FIFOF#(msg_T) q <- mkUGFIFOF();
  
  //Bind the interface to a name for convenience
  let outg = (interface CON_Out;
  
	       method CON_Data try() if (q.notEmpty) = marshall(q.first());
	       
	       method Action success = q.deq();

	     endinterface);

  //Figure out my type for typechecking
  msg_T msg = ?;
  String mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let info = CSend_Info {cname: portname, ctype: mytype, conn: outg};
  addToCollection(tagged LSend info);

  method Bool notFull();
    return q.notFull();
  endmethod
  
  method Action send(msg_T data);
    q.enq(data);
  endmethod

endmodule

module [Connected_Module] mkConnectionRecvUG#(String portname)
    //interface:
                (Connection_Receive#(msg_T))
    provisos
            (Bits#(msg_T, msg_SZ),
	     Transmittable#(msg_T));

  PulseWire      en_w    <- mkPulseWire();
  RWire#(msg_T)  data_w  <- mkRWire();
  
  //Bind the interface to a name for convenience
  let inc = (interface CON_In;
  
	       method Action get_TRY(CON_Data x);
	         data_w.wset(unmarshall(x));
	       endmethod
	       
	       method Bool get_SUCCESS();
	         return en_w;
	       endmethod

	     endinterface);

  //Figure out my type for typechecking
  msg_T msg = ?;
  String mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let info = CRecv_Info {cname: portname, ctype: mytype, conn: inc};
  addToCollection(tagged LRecv info);
  
  method msg_T receive();
    return validValue(data_w.wget());
  endmethod

  method Bool notEmpty();
    return isValid(data_w.wget());
  endmethod

  method Action deq();
    en_w.send();
  endmethod

endmodule
