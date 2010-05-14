
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
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, 6));

    Connection_Receive#(Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))) con <- mkConnection_Receive(portname);
    
    Reg#(INSTANCE_ID#(t_NUM_INSTANCES)) maxInstance <- mkReg(fromInteger(valueof(t_NUM_INSTANCES) - 1));

    Integer rMax = (latency * valueof(t_NUM_INSTANCES)) + 1;

    if (rMax > 64)
        error("Total Port buffering cannot currently exceed 64. Port: " + portname);

    function Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG)) initfunc(Bit#(6) idx);
        INSTANCE_ID#(t_NUM_INSTANCES) iid = truncate(idx);
        return tuple2(iid, tagged Invalid);
    endfunction

    LUTRAM#(Bit#(6), Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))) rs <- mkLUTRAMWith(initfunc);
    LUTRAM#(Bit#(6), Tuple2#(INSTANCE_ID#(t_NUM_INSTANCES), Maybe#(t_MSG))) sideBuffer <- mkLUTRAMWith(initfunc);

    COUNTER#(6) head <- mkLCounter(0);
    COUNTER#(6) tail <- mkLCounter((fromInteger(latency * (valueof(t_NUM_INSTANCES) - 1))));
    COUNTER#(6) sideHead <- mkLCounter(0);
    COUNTER#(6) sideTail <- mkLCounter((fromInteger(latency)));
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
        
            Bit#(6) l = fromInteger(latency);
            Bit#(6) k = zeroExtendNP(iid) + 1;
            Bit#(6) n = zeroExtendNP(period);
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
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, 6));
         
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
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, 6));
         
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
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, 6));
         
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
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, 6));
         
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
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, 6));
         
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
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, 6));

         
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


module [HASIM_MODULE] mkPortRecv_Multiplexed_Join#(PORT_RECV_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG) p1, PORT_RECV#(t_MSG) p2, Bit#(TLog#(TAdd#(t_NUM_INSTANCES, 1))) insertion_point)
    // interface:
        (PORT_RECV_MULTIPLEXED#(TAdd#(t_NUM_INSTANCES, 1), t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, TLog#(TAdd#(t_NUM_INSTANCES, 1))));


    COUNTER#(TLog#(TAdd#(t_NUM_INSTANCES, 1))) cur <- mkLCounter(0);
    Reg#(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1))) maxInstance <- mkReg(0);
    Reg#(Bool) initialized <- mkReg(False);
    
    Bool canDeq = (cur.value() == insertion_point) ? !p2.ctrl.empty() : !p1.ctrl.empty();
    
    interface INSTANCE_CONTROL_IN ctrl;


        method Bool empty() = !canDeq;
        method Bool balanced() = True;
        method Bool light() = False;
        
        method Maybe#(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1))) nextReadyInstance();
        
            return (!canDeq || !initialized) ? tagged Invalid : tagged Valid cur.value();
        
        endmethod
        
        method Action setMaxRunningInstance(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1)) iid);
            maxInstance <= iid; // Local Controller has already added 1 to this number.
            initialized <= True;
        endmethod
        
    endinterface

    method ActionValue#(Maybe#(t_MSG)) receive(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1)) dummy) if (canDeq);
        
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
            let msg <- p1.receive(truncate(dummy));
            return msg;
        end
        
    endmethod

endmodule

module [HASIM_MODULE] mkPortSend_Multiplexed_Split#(PORT_SEND_MULTIPLEXED#(t_NUM_INSTANCES, t_MSG) p1, PORT_SEND#(t_MSG) p2, Bit#(TLog#(TAdd#(t_NUM_INSTANCES, 1))) split_point)
    // interface:
        (PORT_SEND_MULTIPLEXED#(TAdd#(t_NUM_INSTANCES, 1), t_MSG))
    provisos
        (Bits#(t_MSG, t_MSG_SZ),
         Add#(TLog#(t_NUM_INSTANCES), t_TMP, TLog#(TAdd#(t_NUM_INSTANCES, 1))));


    COUNTER#(TLog#(TAdd#(t_NUM_INSTANCES, 1))) cur <- mkLCounter(0);
    Reg#(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1))) maxRunningInstance <- mkReg(0);
    Reg#(Bool) initialized <- mkReg(False);
    
    Bool canEnq = (cur.value() == split_point) ? !p2.ctrl.full() : !p1.ctrl.full();
    
    interface INSTANCE_CONTROL_OUT ctrl;

        method Bool full() = !canEnq;
        method Bool balanced() = True;
        method Bool heavy() = False;
        method Action setMaxRunningInstance(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1)) iid);
            maxRunningInstance <= iid; // Local controller has already added one to this number
            initialized <= True;
        endmethod

    endinterface

    method Action send(INSTANCE_ID#(TAdd#(t_NUM_INSTANCES, 1)) dummy, Maybe#(t_MSG) msg) if (initialized && canEnq);
        
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
            p1.send(truncate(dummy), msg);
        end
        
    endmethod

endmodule


