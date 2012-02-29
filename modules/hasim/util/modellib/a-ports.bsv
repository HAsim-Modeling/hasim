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
`include "asim/provides/hasim_modellib.bsh"

import FIFOF::*;
import Vector::*;
import ModuleCollect::*;

typedef `PORT_MAX_LATENCY PORT_MAX_LATENCY;

interface INSTANCE_CONTROL_OUT#(numeric type t_NUM_INSTANCES);

  method Bool full();
  method Bool balanced();
  method Bool heavy();
  method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);

endinterface

interface INSTANCE_CONTROL_IN#(numeric type t_NUM_INSTANCES);

    method Bool empty();
    method Bool balanced();
    method Bool light();

    method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance;
    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);

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
        (Bits#(t_MSG, t_MSG_SZ));
        
    Connection_Send#(Maybe#(t_MSG)) con <- mkConnection_Send(portname);

    //A temporary set of control info
    interface INSTANCE_CONTROL_OUT ctrl;

        method Bool full() = !con.notFull;
        method Bool balanced() = True;
        method Bool heavy() = False;
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid) = noAction;

    endinterface

    method Action send(Maybe#(t_MSG) m);

        con.send(m);

    endmethod
  
endmodule

module [HASIM_MODULE] mkPortRecv#(String portname, Integer latency)
  //interface:
              (PORT_RECV#(t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ));
  
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
                (Bits#(t_MSG, t_MSG_SZ));

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
        method Maybe#(INSTANCE_ID#(1)) nextReadyInstance = tagged Valid 0;
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);
            noAction;
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
                (Bits#(t_MSG, t_MSG_SZ));

  Connection_Receive#(Maybe#(t_MSG)) con <- mkConnection_Receive(portname);
     
  //A temporary set of control info
  interface INSTANCE_CONTROL_IN ctrl;

    method Bool empty() = !con.notEmpty();
    method Bool balanced() = True;
    method Bool light() = False;
    method Maybe#(INSTANCE_ID#(1)) nextReadyInstance() = tagged Valid 0;
    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        noAction;
    endmethod

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
                (Bits#(t_MSG, t_MSG_SZ));

  Connection_Receive#(Maybe#(t_MSG)) con <- mkConnection_Receive(portname);
  Reg#(Bool) initializing <- mkReg(True);
     

  // A temporary set of control info
  interface INSTANCE_CONTROL_IN ctrl;

    method Bool empty() = !con.notEmpty;
    method Bool balanced() = True;
    method Bool light() = False;
    method Maybe#(INSTANCE_ID#(1)) nextReadyInstance = tagged Valid 0;
    method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid) = noAction;

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
        (Bits#(t_MSG, t_MSG_SZ));

    Connection_Send#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))) con <- mkConnection_Send(portname);

    interface INSTANCE_CONTROL_OUT ctrl;

        method Bool full() = !con.notFull();
        method Bool balanced() = True;
        method Bool heavy() = False;
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid) = noAction; // Handled on the receive side only.

    endinterface

    method Action send(INSTANCE_ID#(t_NUM_INSTANCES) iid, Maybe#(t_MSG) m);

        con.send(tuple2(iid, m));

    endmethod

endmodule

module [HASIM_MODULE] mkPortRecv_Multiplexed#(String portname, Integer latency)
    //interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));

    let p <- case (latency)
               //0: mkPortRecvL0_Multiplexed(portname);
               0: mkPortRecvBuffered_Multiplexed(portname, latency);
               default: mkPortRecvBuffered_Multiplexed(portname, latency);
             endcase;

    return p;

endmodule

module [HASIM_MODULE] mkPortRecvBuffered_Multiplexed#(String portname, Integer latency)
    //interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(TMul#(TMax#(t_NUM_INSTANCES, 1), PORT_MAX_LATENCY), n_SLOTS),
         Alias#(Bit#(TLog#(n_SLOTS)), t_SLOT_IDX),
         Alias#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG)), t_BUFFERED_MSG),
         Bits#(t_BUFFERED_MSG, t_BUFFERED_MSG_SZ));

    Connection_Receive#(t_BUFFERED_MSG) con <- mkConnection_Receive(portname);

    if (latency > valueOf(PORT_MAX_LATENCY))
    begin
        error("Latency exceeds current maximum. Port: " + portname);
    end

    //
    // The internal buffer is a FIFO.  Here we attempt to pick a reasonable
    // FIFO implementation depending on the size of the buffer.  Large buffers
    // are stored in block RAM.
    //
    // Requiring the number of slots to be 512 or greater would map optimally
    // to block RAM.  We accept inefficiencies in block RAM usage and accept
    // buffers with only 256 entries because it has been important
    // for some of our models.  In addition to the number of entries, the
    // total amount of buffered data must be close to the size of an 18Kb RAM.
    //
    // The size of the FIFO (doubling the number of slots relative to the
    // minimimum requirement of one slot per instance) allows extra buffering
    // for greater cycle variation among pipeline stages.  This flexibility
    // results in significant simulator performance gains, especially for
    // multi-FPGA designs.
    //
    FIFOF#(t_BUFFERED_MSG) rs;
    Integer total_slots = max(1, latency) * max(1, valueOf(TMul#(2, t_NUM_INSTANCES)));
    if ((total_slots >= 256) &&
        (total_slots * valueOf(t_BUFFERED_MSG_SZ) > 14000))
    begin
        // Large buffer.  Use block RAM.
        rs <- mkSizedBRAMFIFOF(total_slots);
    end
    else
    begin
        // Small buffer.  Use distributed memory.
        rs <- mkSizedFIFOF(total_slots);
    end

    Reg#(Bool) initialized <- mkReg(False);
    Reg#(t_SLOT_IDX) initIdx <- mkReg(0);
    Reg#(Maybe#(t_SLOT_IDX)) initMax <- mkReg(tagged Invalid);
    
    //
    // doInit --
    //   Fill the FIFO with no message corresponding to the initial buffering.
    //
    rule doInit (! initialized &&& initMax matches tagged Valid .max_idx);
        INSTANCE_ID#(t_NUM_INSTANCES) iid = truncateNP(initIdx);
        rs.enq(tuple2(iid, tagged Invalid));

        initialized <= (initIdx == max_idx);
        initIdx <= initIdx + 1;
    endrule

    //
    // shift --
    //   Pass received messages to the FIFO buffer.
    //
    rule shift (initialized && con.notEmpty());
        let d = con.receive();
        con.deq();

        rs.enq(d);
    endrule


    interface INSTANCE_CONTROL_IN ctrl;

        method Bool empty() = ! rs.notEmpty();
        method Bool balanced() = True;
        method Bool light() = False;
        
        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();

            if (rs.notEmpty())
            begin
                match {.iid, .m} = rs.first();
                return tagged Valid iid;
            end
            else
            begin
                return tagged Invalid;
            end
        
        endmethod
        
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        
            t_SLOT_IDX l = fromInteger(latency);
            t_SLOT_IDX k = zeroExtendNP(iid) + 1;

            if (l == 0)
            begin    
                // 0-latency port has no initial fill.
                initialized <= True;
            end
            else
            begin    
                // Write k * l no-messages to initialize the channel.
                initMax <= tagged Valid (k * l - 1);
            end

        endmethod

    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) dummy);

        match {.iid, .m} = rs.first();
        rs.deq();

        return m;

    endmethod

endmodule

module [HASIM_MODULE] mkPortRecvL0_Multiplexed#(String portname)
    //interface:
                (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ));

    Connection_Receive#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))) con <- mkConnection_Receive(portname);
     
    interface INSTANCE_CONTROL_IN ctrl;

        method Bool empty() = !con.notEmpty();
        method Bool balanced() = True;
        method Bool light() = False;

        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
        
            match {.iid, .m} = con.receive();
            return (con.notEmpty) ? tagged Valid iid : tagged Invalid;
        
        endmethod
        
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);

            noAction;

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
                (Bits#(t_MSG, t_MSG_SZ));
    
    let m <- mkPortRecv_L0(portname);
    return m;
    
endmodule

module [Connected_Module] mkPortRecvDependent_Multiplexed#(String portname)
    // interface:
                (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
      provisos
                (Bits#(t_MSG, t_MSG_SZ));
    
    let m <- mkPortRecvL0_Multiplexed(portname);
    return m;
    
endmodule
