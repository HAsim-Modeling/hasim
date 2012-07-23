//
// Copyright (C) 2010 Massachusetts Institute of Technology
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

module [HASIM_MODULE] mkPortRecv_Multiplexed_ReorderSideBuffer
    #(
        String portname, 
        Integer latency, 
        INSTANCE_ID#(t_NUM_INSTANCES) period,
        function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid),
        function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid),
        function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid),
        function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid)
    )
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(TMul#(t_NUM_INSTANCES, PORT_MAX_LATENCY), n_SLOTS),
         Alias#(Bit#(TLog#(n_SLOTS)), t_SLOT_IDX),
         Bits#(t_SLOT_IDX, t_SLOT_IDX_SZ));

    CONNECTION_RECV#(PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)) con <- mkPortRecv_MaybeCompressed(portname);

    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) maxInstance <- mkReg(fromInteger(valueof(t_NUM_INSTANCES) - 1));

    Integer rMax = (latency * valueof(t_NUM_INSTANCES)) + 1;

    if (latency > valueOf(PORT_MAX_LATENCY))
    begin
        error("Latency exceeds current maximum. Port: " + portname);
    end

    function PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG) initfunc(t_SLOT_IDX idx);
        INSTANCE_ID#(t_NUM_INSTANCES) iid = truncateNP(idx);
        return tuple2(iid, tagged Invalid);
    endfunction

    LUTRAM#(t_SLOT_IDX, PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)) rs <- mkLUTRAMWith(initfunc);
    LUTRAM#(t_SLOT_IDX, PORT_MULTIPLEXED_MSG#(t_NUM_INSTANCES, t_MSG)) sideBuffer <- mkLUTRAMWith(initfunc);

    COUNTER#(t_SLOT_IDX_SZ) head <- mkLCounter(0);
    COUNTER#(t_SLOT_IDX_SZ) tail <- mkLCounter((fromInteger(latency * (valueof(t_NUM_INSTANCES) - 1))));
    COUNTER#(t_SLOT_IDX_SZ) sideHead <- mkLCounter(0);
    COUNTER#(t_SLOT_IDX_SZ) sideTail <- mkLCounter((fromInteger(latency)));
    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) curEnq <- mkReg(0);
    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) curDeq <- mkReg(0);

    Bool fullQ  = tail.value() + 1 == head.value();
    Bool emptyQ = head.value() == tail.value();
    Bool sideFull  = sideTail.value() + 1 == sideHead.value();
    Bool sideEmpty = sideHead.value() == sideTail.value();
    Bool canDeq = deqFromSide(curDeq, maxInstance) ? !sideEmpty : !emptyQ;
    Bool canEnq = enqToSide(curEnq, maxInstance)   ? !sideFull  : !fullQ;

    Reg#(Bool) initialized <- mkReg(False);

    rule shift (initialized && canEnq && con.notEmpty());

        match {.iid, .msg} = con.receive();
        con.deq();

        if (enqToSide(curEnq, maxInstance))
        begin
            
            sideBuffer.upd(sideTail.value(), tuple2(iid, msg));
            sideTail.up();
        
        end
        else
        begin
        
            rs.upd(tail.value(), tuple2(iid, msg));
            tail.up();
        
        end
        
        if (resetEnq(curEnq, maxInstance))
        begin
        
            curEnq <= 0;
        
        end
        else
        begin

            curEnq <= curEnq + 1;

        end

    endrule
    
    interface INSTANCE_CONTROL_IN ctrl;


        method Bool empty() = !canDeq;
        method Bool balanced() = True;
        method Bool light() = False;
        
        method Maybe#(INSTANCE_ID#(t_NUM_INSTANCES)) nextReadyInstance();
        
            match {.iid, .m} = deqFromSide(curDeq, maxInstance) ? sideBuffer.sub(sideHead.value()) : rs.sub(head.value());
            return (!canDeq || !initialized) ? tagged Invalid : tagged Valid iid;
        endmethod
        
        method Action setMaxRunningInstance(INSTANCE_ID#(t_NUM_INSTANCES) iid);
        
            t_SLOT_IDX l = fromInteger(latency);
            t_SLOT_IDX k = zeroExtendNP(iid) + 1;
            t_SLOT_IDX n = zeroExtendNP(period);
            tail.setC((k-n) * l);
            sideTail.setC(n * l);
            maxInstance <= iid;
            initialized <= True;
        
        endmethod
        
    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(t_NUM_INSTANCES) dummy) if (canDeq);

        Maybe#(t_MSG) res = tagged Invalid;

        if (deqFromSide(curDeq, maxInstance))
        begin
        
            // Return the side buffer.
            match {.iid, .m} = sideBuffer.sub(sideHead.value());
            res = m;
            sideHead.up();
        
        end
        else
        begin
        
            // Return the main buffer.
            match {.iid, .m} = rs.sub(head.value());
            res = m;
            head.up();
        
        end

        if (resetDeq(curDeq, maxInstance))
        begin
            curDeq <= 0;
        end
        else
        begin
            curDeq <= curDeq + 1;
        end

        return res;

    endmethod

endmodule

module [HASIM_MODULE] mkPortRecv_Multiplexed_ReorderFirstToLast#(String portname, Integer latency)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == 0;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, 1, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


module [HASIM_MODULE] mkPortRecv_Multiplexed_ReorderLastToFirst#(String portname, Integer latency)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == 0;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, 1, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule

module [HASIM_MODULE] mkPortRecv_Multiplexed_ReorderFirstToLastEveryN#(String portname, Integer latency, INSTANCE_ID#(t_NUM_INSTANCES) period)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == 0;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == period - 1;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == (period - 1);
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == (period - 1);
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, period, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


module [HASIM_MODULE] mkPortRecv_Multiplexed_ReorderLastToFirstEveryN#(String portname, Integer latency, INSTANCE_ID#(t_NUM_INSTANCES) period)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == period - 1;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == 0;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == (period - 1);
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == (period - 1);
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, period, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


module [HASIM_MODULE] mkPortRecv_Multiplexed_ReorderFirstNToLastN#(String portname, Integer latency, INSTANCE_ID#(t_NUM_INSTANCES) period)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq < period;
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq > (max_iid - period);
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, period, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


module [HASIM_MODULE] mkPortRecv_Multiplexed_ReorderLastNToFirstN#(String portname, Integer latency, INSTANCE_ID#(t_NUM_INSTANCES) period)
    // interface:
        (PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ));
         
    function Bool enqToSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq > (max_iid - period);
    endfunction

    function Bool deqFromSide(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq < period;
    endfunction
    
    function Bool resetEnq(INSTANCE_ID#(t_NUM_INSTANCES) cur_enq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_enq == max_iid;
    endfunction
    
    function Bool resetDeq(INSTANCE_ID#(t_NUM_INSTANCES) cur_deq, INSTANCE_ID#(t_NUM_INSTANCES) max_iid);
        return cur_deq == max_iid;
    endfunction
    
    let p <- mkPortRecv_Multiplexed_ReorderSideBuffer(portname, latency, period, enqToSide, resetEnq, deqFromSide, resetDeq);
    return p;

endmodule


module [HASIM_MODULE] mkPortRecv_Multiplexed_Join#(
        PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG) p1,
        PORT_RECV#(t_MSG) p2,
        t_JOIN_ID insertion_point)
    // interface:
    (PORT_RECV_MULTIPLEXED#(TAdd#(t_NUM_INSTANCES, 1), t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(INSTANCE_ID_BITS#(TAdd#(t_NUM_INSTANCES, 1)), n_JOIN_ID_BITS),
         Alias#(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1)), t_JOIN_ID));

    COUNTER#(n_JOIN_ID_BITS) cur <- mkLCounter(0);
    Reg#(t_JOIN_ID) maxInstance <- mkReg(0);
    Reg#(Bool) initialized <- mkReg(False);
    
    Bool canDeq = (cur.value() == insertion_point) ? !p2.ctrl.empty() : !p1.ctrl.empty();
    
    interface INSTANCE_CONTROL_IN ctrl;


        method Bool empty() = !canDeq;
        method Bool balanced() = True;
        method Bool light() = False;
        
        method Maybe#(t_JOIN_ID) nextReadyInstance();
        
            return (!canDeq || !initialized) ? tagged Invalid : tagged Valid cur.value();
        
        endmethod
        
        method Action setMaxRunningInstance(t_JOIN_ID iid);
            maxInstance <= iid; // Local Controller has already added 1 to this number.
            initialized <= True;
        endmethod
        
    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(t_JOIN_ID dummy) if (canDeq);
        
        if (cur.value() == maxInstance)
        begin
            cur.setC(0);
        end
        else
        begin
            cur.up();
        end
        
        if (cur.value() == insertion_point)
        begin
            let msg <- p2.receive();
            return msg;
        end
        else
        begin
            let msg <- p1.receive(truncateNP(dummy));
            return msg;
        end
        
    endmethod

endmodule

module [HASIM_MODULE] mkPortSend_Multiplexed_Split#(
        PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG) p1,
        PORT_SEND#(t_MSG) p2,
        t_SPLIT_ID split_point)
    // interface:
        (PORT_SEND_MULTIPLEXED#(TAdd#(t_NUM_INSTANCES, 1), t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         NumAlias#(INSTANCE_ID_BITS#(TAdd#(t_NUM_INSTANCES, 1)), n_SPLIT_ID_BITS),
         Alias#(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1)), t_SPLIT_ID));


    COUNTER#(n_SPLIT_ID_BITS) cur <- mkLCounter(0);
    Reg#(t_SPLIT_ID) maxRunningInstance <- mkReg(0);
    Reg#(Bool) initialized <- mkReg(False);
    
    Bool canEnq = (cur.value() == split_point) ? !p2.ctrl.full() : !p1.ctrl.full();
    
    interface INSTANCE_CONTROL_OUT ctrl;

        method Bool full() = !canEnq;
        method Bool balanced() = True;
        method Bool heavy() = False;
        method Action setMaxRunningInstance(t_SPLIT_ID iid);
            maxRunningInstance <= iid; // Local controller has already added one to this number
            initialized <= True;
        endmethod

    endinterface

    method Action send(t_SPLIT_ID dummy, Maybe#(t_MSG) msg) if (initialized && canEnq);
        
        if (cur.value() == maxRunningInstance)
        begin
            cur.setC(0);
        end
        else
        begin
            cur.up();
        end
        
        if (cur.value() == split_point)
        begin
            p2.send(msg);
        end
        else
        begin
            p1.send(truncateNP(dummy), msg);
        end
        
    endmethod

endmodule


