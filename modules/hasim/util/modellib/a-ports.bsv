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

interface INSTANCE_CONTROL_OUT#(numeric type t_NUM_INSTANCES);

  method Bool full();
  method Bool balanced();
  method Bool heavy();

endinterface

interface INSTANCE_CONTROL_IN#(numeric type t_NUM_INSTANCES);

    method Bool empty();
    method Bool balanced();
    method Bool light();

    method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance;
    method Action drop();

endinterface

interface INSTANCE_CONTROL_IN_OUT#(numeric type t_NUM_INSTANCES);

    interface INSTANCE_CONTROL_IN#(t_NUM_INSTANCES) in;
    interface INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES) out;

endinterface


interface PORT_SEND#(type t_MSG);
  
  method Action send(Maybe#(t_MSG) m);
  interface INSTANCE_CONTROL_OUT#(1) ctrl;
  
endinterface

interface PORT_RECV#(type t_MSG);

  method ActionValue#(Maybe#(t_MSG)) receive();
  interface INSTANCE_CONTROL_IN#(1) ctrl;

endinterface

interface PORT_SEND_MULTIPLEXED#(type t_NUM_INSTANCES, type t_MSG);
  
  method Action send(INSTANCE_ID#(t_NUM_INSTANCES) iid, Maybe#(t_MSG) m);
  interface INSTANCE_CONTROL_OUT#(t_NUM_INSTANCES) ctrl;
  
endinterface

interface PORT_RECV_MULTIPLEXED#(type t_NUM_INSTANCES, type t_MSG);

  method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) iid);
  interface INSTANCE_CONTROL_IN#(t_NUM_INSTANCES) ctrl;

endinterface

module [HASIM_MODULE] mkPortSend#(String portname)
    //interface:
        (PORT_SEND#(t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Transmittable#(Maybe#(t_MSG)));
        
    Connection_Send#(Maybe#(t_MSG)) con <- mkConnection_Send(portname);

    //A temporary set of control info
    interface INSTANCE_CONTROL_OUT ctrl;

        method Bool full() = !con.notFull;
        method Bool balanced() = True;
        method Bool heavy() = False;

    endinterface

    method Action send(Maybe#(t_MSG) m);

        con.send(m);

    endmethod
  
endmodule

module [HASIM_MODULE] mkPortRecv#(String portname, Integer latency)
  //interface:
              (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ),
                 Transmittable#(Maybe#(t_MSG)));
  
  let p <- case (latency)
             0: mkPortRecv_L0(portname);
             1: mkPortRecv_L1(portname, tagged Invalid);
             default: mkPortRecv_Buffered(portname, latency, 0, tagged Invalid);
           endcase;
 
  return p;

endmodule

module [HASIM_MODULE] mkPortRecv_Buffered#(String portname, Integer latency, Integer extra_buffering, Maybe#(t_MSG) init_value)
    //interface:
                (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ),
                 Transmittable#(Maybe#(t_MSG)));

  Connection_Receive#(Maybe#(t_MSG)) con <- mkConnection_Receive(portname);
   
  Integer rMax = latency + extra_buffering + 1;
  
  if (rMax > 255)
    error("Total Port buffering cannot currently exceed 255.");
  
  Reg#(Maybe#(t_MSG)) rs[rMax];
  
  for (Integer x = 0; x < rMax; x = x + 1)
    rs[x] <- mkReg(init_value);

  Reg#(Bit#(8)) head <- mkReg(fromInteger(latency));
  Reg#(Bit#(8)) tail <- mkReg(0);
  Bit#(8) numElems = head - tail;
  
  function Bit#(n) overflow_incr(Bit#(n) x);
    
    let tmp = x + 1;
    return (tmp == fromInteger(rMax)) ? 0 : tmp;
  endfunction

  Bool fullQ  = overflow_incr(head) == tail;
  Bool emptyQ = head == tail;
  
  
  rule shift (!fullQ);
  
    let d = con.receive();
    con.deq();
    
    (rs[head._read()]) <= d;
    head <= overflow_incr(head);
   
  endrule
  
  //A temporary set of control info
  interface INSTANCE_CONTROL_IN ctrl;

        method Bool empty() = emptyQ;
        method Bool balanced() = True;
        method Bool light() = False;
        method Maybe#(INSTANCE_ID#(1)) nextReadyInstance = tagged Valid (?);
        method Action drop;
            tail <= overflow_incr(tail);
        endmethod

  endinterface

  method ActionValue#(Maybe#(t_MSG)) receive() if (!emptyQ);
    
    tail <= overflow_incr(tail);
    return rs[tail._read()]._read();
    
  endmethod

endmodule


//Port optimized for latency 0

module [HASIM_MODULE] mkPortRecv_L0#(String portname)
    //interface:
                (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ),
                 Transmittable#(Maybe#(t_MSG)));

  Connection_Receive#(Maybe#(t_MSG)) con <- mkConnection_Receive(portname);
     
  //A temporary set of control info
  interface INSTANCE_CONTROL_IN ctrl;

    method Bool empty() = !con.notEmpty();
    method Bool balanced() = True;
    method Bool light() = False;
    method Maybe#(INSTANCE_ID#(1)) nextReadyInstance() = tagged Valid (?);
    method Action drop() = con.deq();

  endinterface

  method ActionValue#(Maybe#(t_MSG)) receive();
  
    con.deq();
    return con.receive();
    
  endmethod
  
endmodule

//Port optimized for latency 1

module [HASIM_MODULE] mkPortRecv_L1#(String portname, Maybe#(t_MSG) init_value)
    //interface:
                (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ),
                 Transmittable#(Maybe#(t_MSG)));

  Connection_Receive#(Maybe#(t_MSG)) con <- mkConnection_Receive(portname);
  Reg#(Bool) initializing <- mkReg(True);
     

  // A temporary set of control info
  interface INSTANCE_CONTROL_IN ctrl;

    method Bool empty() = con.notEmpty;
    method Bool balanced() = True;
    method Bool light() = False;
    method Maybe#(INSTANCE_ID#(1)) nextReadyInstance = tagged Valid (?);
    method Action drop() = con.deq();

  endinterface

  
  method ActionValue#(Maybe#(t_MSG)) receive();
    if (initializing)
    begin
      initializing <= False;
      return init_value;
    end
    else
    begin
      let m = con.receive();
      con.deq();
      return m;
    end
  endmethod

endmodule



module [HASIM_MODULE] mkPortSend_Multiplexed#(String portname)
    //interface:
        (PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Transmittable#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))));

    Connection_Send#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))) con <- mkConnection_Send(portname);

    interface INSTANCE_CONTROL_OUT ctrl;

        method Bool full() = !con.notFull();
        method Bool balanced() = True;
        method Bool heavy() = False;

    endinterface

    method Action send(INSTANCE_ID#(t_NUM_INSTANCES) iid, Maybe#(t_MSG) m);

        con.send(tuple2(iid, m));

    endmethod

endmodule

module [HASIM_MODULE] mkPortRecv_Multiplexed#(String portname, Integer latency)
    //interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
        provisos
                  (Bits#(t_MSG, t_MSG_SZ),
                   Add#(TLog#(t_NUM_INSTANCES), t_TMP, 6),
                   Transmittable#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))));

    let p <- case (latency)
               0: mkPortRecvL0_Multiplexed(portname);
               default: mkPortRecvBuffered_Multiplexed(portname, latency);
             endcase;

    return p;

endmodule

module [HASIM_MODULE] mkPortRecvBuffered_Multiplexed#(String portname, Integer latency)
    //interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, 6),
         Transmittable#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))));

    Connection_Receive#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))) con <- mkConnection_Receive(portname);

    Integer rMax = (latency * valueof(t_NUM_INSTANCES)) + 1;

    if (rMax > 64)
        error("Total Port buffering cannot currently exceed 64. Port: " + portname);

    function Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG)) initfunc(Bit#(6) idx);
        INSTANCE_ID#(t_NUM_INSTANCES) iid = truncate(idx);
        return tuple2(iid, tagged Invalid);
    endfunction

    LUTRAM#(Bit#(6), Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))) rs <- mkLUTRAMWith(initfunc);

    COUNTER#(6) head <- mkLCounter(0);
    COUNTER#(6) tail <- mkLCounter((fromInteger(latency * valueof(t_NUM_INSTANCES))));

    Bool fullQ  = tail.value() + 1 == head.value();
    Bool emptyQ = head.value() == tail.value();


    rule shift (!fullQ && con.notEmpty());

        let d = con.receive();
        con.deq();

        rs.upd(tail.value(), d);
        tail.up();

    endrule
    
    interface INSTANCE_CONTROL_IN ctrl;


        method Bool empty() = emptyQ;
        method Bool balanced() = True;
        method Bool light() = False;
        
        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
        
            match {.iid, .m} = rs.sub(head.value());
            return (emptyQ) ? tagged Invalid : tagged Valid iid;
        
        endmethod
        
        method Action drop();
        
            head.up();

        endmethod

    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) dummy) if (!emptyQ);

        // TEMPORARILY COMMENT OUT TO REMOVE READ OF tail. This introduced spurious conflicts.
        //if (emptyQ)
        //    $display("WARNING: Underflow on unguarded receive port %s! Junk data added!", portname);

        match {.iid, .m} = rs.sub(head.value());
        head.up();
        return m;

    endmethod

endmodule

module [HASIM_MODULE] mkPortRecvL0_Multiplexed#(String portname)
    //interface:
                (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ),
                 Transmittable#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))));

    Connection_Receive#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))) con <- mkConnection_Receive(portname);
     
    interface INSTANCE_CONTROL_IN ctrl;

        method Bool empty() = !con.notEmpty();
        method Bool balanced() = True;
        method Bool light() = False;

        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
        
            match {.iid, .m} = con.receive();
            return (con.notEmpty) ? tagged Valid iid : tagged Invalid;
        
        endmethod
        
        method Action drop();
        
            con.deq();

        endmethod

    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) dummy);

        if (!con.notEmpty)
            $display("WARNING: Underflow on unguarded receive port %s! Junk data added!", portname);

        con.deq();
        match {.iid, .m} = con.receive();
        return m;


    endmethod
  
endmodule

module [Connected_Module] mkPortRecvDependent#(String portname)
    // interface:
                (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ),
                 Transmittable#(Maybe#(t_MSG)));
    
    let m <- mkPortRecv_L0(portname);
    return m;
    
endmodule

module [Connected_Module] mkPortRecvDependent_Multiplexed#(String portname)
    // interface:
                (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ),
                 Transmittable#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))));
    
    let m <- mkPortRecvL0_Multiplexed(portname);
    return m;
    
endmodule

module [Connected_Module] mkConnectionSendUG#(String portname)
    // interface:
                (Connection_Send#(t_MSG))
    provisos
            (Bits#(t_MSG, t_MSG_SZ),
	     Transmittable#(t_MSG));

  //This queue is here for correctness until the system is confirmed to work
  //Later it could be removed or turned into a BypassFIFO to reduce latency.
  
  FIFOF#(t_MSG) q <- mkUGFIFOF();
  
  //Bind the interface to a name for convenience
  let outg = (interface CON_Out;
  
	       method CON_Data try() if (q.notEmpty) = marshall(q.first());
	       
	       method Action success = q.deq();

	     endinterface);

  //Figure out my type for typechecking
  t_MSG msg = ?;
  String mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let info = CSend_Info {cname: portname, ctype: mytype, conn: outg};
  addToCollection(tagged LSend info);

  method Bool notFull();
    return q.notFull();
  endmethod
  
  method Action send(t_MSG data);
    q.enq(data);
  endmethod

endmodule

module [Connected_Module] mkConnectionRecvUG#(String portname)
    //interface:
                (Connection_Receive#(t_MSG))
    provisos
            (Bits#(t_MSG, t_MSG_SZ),
	     Transmittable#(t_MSG));

  PulseWire      en_w    <- mkPulseWire();
  RWire#(t_MSG)  data_w  <- mkRWire();
  
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
  t_MSG msg = ?;
  String mytype = printType(typeOf(msg));

  //Add our interface to the ModuleCollect collection
  let info = CRecv_Info {cname: portname, ctype: mytype, conn: inc};
  addToCollection(tagged LRecv info);
  
  method t_MSG receive();
    return validValue(data_w.wget());
  endmethod

  method Bool notEmpty();
    return isValid(data_w.wget());
  endmethod

  method Action deq();
    en_w.send();
  endmethod

endmodule
