import FIFO::*;

import hasim_common::*;
import soft_connections::*;

// The StallPorts model a ping-pong buffer/double buffer.
//
// A ping-pong buffer is an implementation of a pipeline latch which has
// storage for two cycles worth of information. If at the beginning of a cycle,
// both entries in the buffer are occupied, the producer will not do any work,
// regardless of whether the consumer plans to consume an entry in that cycle.
// The advantage of this implementation is that there is no combinational path
// from the consumer to the producer. For those familiar with bluespec, this is
// exactly the bluespec mkFIFO.
//
// StallPorts guard the producer from enq-ing valid data when the modelled
// buffering is fully occupied. Only when canSend is True, the producer can
// _send_ valid data. When canSend is False, the producer must _pass_ instead.
// A pass is equivalent to a send-Invalid, but can only be called when the
// buffers are full. A producer must perform exactly one of the following
// actions every model cycle: (a) send valid data, (b) send invalid, or (c)
// pass.
//
// The consumer side can choose to either receive data for a cycle, or pass.
// Since a module may wish to base this decision on the contents of the first
// entry in the buffer, there is also a peek method which will show the data
// without removing it from the buffer. The receive method, on the other hand,
// and also deq it from the fifo.

interface StallPort_Send#(type a);
    method Action send(Maybe#(a) x);
    method Action pass();
    method Bool   canSend();
endinterface

interface StallPort_Receive#(type a);
    method ActionValue#(Maybe#(a)) receive();
    method Maybe#(a)               peek();
    method Action                  pass();
endinterface

module [HASIM_MODULE] mkStallPort_Send#(String s)
                       (StallPort_Send#(a))
            provisos (Bits#(a, sa),
                      Transmittable#(Maybe#(a)));

    Connection_Send#(Maybe#(a)) con_data <- mkConnection_Send   (s + ":data");
    Connection_Receive#(Bool)   con_cred <- mkConnection_Receive(s + ":cred");

    let can_send = con_cred.receive();

    method Action send (Maybe#(a) x) if (can_send);
        con_cred.deq();
        con_data.send(x);
    endmethod

    method Action pass() if (!can_send);
        con_cred.deq();
        con_data.send(Invalid);
    endmethod

    method Bool canSend() = can_send;
endmodule

module [HASIM_MODULE] mkStallPort_Receive#(String s)
        (StallPort_Receive#(a))
            provisos (Bits#(a, sa),
                      Transmittable#(Maybe#(a)));

    Connection_Receive#(Maybe#(a)) con_data <- mkConnection_Receive(s + ":data");
    Connection_Send#(Bool)         con_cred <- mkConnection_Send   (s + ":cred");

    FIFOF#(a) fifo <- mkUGSizedFIFOF(2);
    
    Reg#(Bool) pC <- mkReg(True);
    Reg#(Bool) cC <- mkReg(True);

    Reg#(Bool) cN <- mkReg(?);

    rule work (pC && cC);
        con_cred.send(fifo.notFull());
        cN <= fifo.notEmpty();
        pC <= False;
        cC <= False;
    endrule

    rule enq;
        con_data.deq();
        let mx = con_data.receive();
        if (mx matches tagged Valid .x)
            fifo.enq(x);
        pC <= True;
    endrule

    method ActionValue#(Maybe#(a)) receive() if (!cC);
        fifo.deq();
        cC <= True;
        return cN ? Valid (fifo.first) : Invalid;
    endmethod

    method Maybe#(a) peek() if (!cC);
        return cN ? Valid (fifo.first) : Invalid;
    endmethod

    method Action pass() if (!cC);
        cC <= True;
    endmethod

endmodule

