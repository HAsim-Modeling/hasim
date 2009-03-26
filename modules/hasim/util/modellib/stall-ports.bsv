import FIFO::*;

import hasim_common::*;
import soft_connections::*;

/* -------------------------------------------------------------------------- */
// The StallPorts model a ping-pong buffer/double buffer.
//
// Description of a ping-pong buffer
// A ping-pong buffer is an implementation of a pipeline latch which has
// storage for two cycles worth of information. If at the beginning of a cycle,
// both entries in the buffer are occupied, the producer will not do any work,
// regardless of whether the consumer plans to consume an entry in that cycle.
// The advantage of this implementation is that there is no combinational path
// from the consumer to the producer. For those familiar with bluespec, this is
// exactly the bluespec mkFIFO.
//
// Here is an example that demonstrates the operation of a ping pong buffer as
// well as bubble squishing. A-F are instructions, FDXMW are pipeline stages.
// || is the ping-pong buffer and S represents a stall. Note that unlike
// conventional pipeline diagrams, I am showing the instructions as sitting
// inside the buffer as opposed to inside a stage.
//
// time       F  ||  D  ||  X  ||  M  ||  W
//    0
//    1           A
//    2           B      A
//    3           C      B      A  S            # S = stall due to cache-miss
//    4           D      C     BA  S
//    5           E     DC     BA
//    6          FE     DC      B      A
//    7          FE  S1  D      C      B        # S1 = stall due to dependency on B
//    8          FE  S1         D  S2  C        # S2 = stall due to cache miss
//    9          FE             D  S2
//   10           F      E      D  S2           # bubble squished
//

/* -------------------------------------------------------------------------- */
// StallPorts Interface:
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

/* -------------------------------------------------------------------------- */
// Implementation Notes:
//
// The pC & cC are state for a 4 stage FSM as shown:
//
// State pC  cC        Description                       Action
//   A    0   0  Beginning of model cycle     None.
//   B    0   1  Consumer model cycle ended   Block consumer from consuming more data.
//   C    1   0  Producer model cycle ended   Block producer from producing more data.
//   D    1   1  Both P&C model cycles ended  Determine canSend & canReceive for next cycle.
//
// Legal paths: A -> B -> D -> A and A -> C -> D -> A
//
// Note that the implementation allows the producer and consumer to "finish"
// their model cycle in any order, but prevents them from going ahead for more
// than 1 model cycle.

interface PORT_STALL_SEND#(type a);
    method Action doEnq(a x);
    method Action noEnq();
    method Bool   canEnq();
    interface PORT_CONTROL ctrl;

endinterface

interface PORT_STALL_RECV#(type a);
    method Bool   canDeq();
    method Action noDeq();
    method Action doDeq();
    method a      peek();
    interface PORT_CONTROL ctrl;
endinterface


interface PORT_STALL_SEND_MULTIPLEXED#(type ni, type a);
    method Action doEnq(INSTANCE_ID#(ni) iid, a x);
    method Action noEnq(INSTANCE_ID#(ni) iid);
    method Bool   canEnq(INSTANCE_ID#(ni) iid);
    interface Vector#(ni, PORT_CONTROL) ctrl;

endinterface

interface PORT_STALL_RECV_MULTIPLEXED#(type ni, type a);
    method Bool   canDeq(INSTANCE_ID#(ni) iid);
    method Action noDeq(INSTANCE_ID#(ni) iid);
    method Action doDeq(INSTANCE_ID#(ni) iid);
    method a      peek(INSTANCE_ID#(ni) iid);
    interface Vector#(ni, PORT_CONTROL) ctrl;
endinterface


module [HASIM_MODULE] mkPortStallSend#(String s)
                       (PORT_STALL_SEND#(a))
            provisos (Bits#(a, sa),
                      Transmittable#(Maybe#(a)));

    Connection_Receive#(Bool) creditFromQueue <- mkConnectionRecvUG(s + "__cred");

    PORT_SEND#(a) enqToQueue <- mkPortSendUG(s + "__portDataEnq");

    Reg#(Bool) initCredit <- mkReg(True);

    method Action doEnq (a x);
        if (!initCredit)
        begin
            creditFromQueue.deq();
        end
        initCredit <= False;
        enqToQueue.send(tagged Valid x);
    endmethod

    method Action noEnq();
        if (!initCredit)
        begin
            creditFromQueue.deq();
        end
        initCredit <= False;
        enqToQueue.send(tagged Invalid);
    endmethod

    method Bool canEnq() = initCredit ? True : creditFromQueue.receive();

    interface PORT_CONTROL ctrl;
        method Bool empty() = !(creditFromQueue.notEmpty() || initCredit); // This is that we have a credit token.
        method Bool full() = enqToQueue.ctrl.full(); // This is if the output port is full.
        method Bool balanced() = True;
        method Bool light() = False;
        method Bool heavy() = False;
    endinterface
endmodule

module [HASIM_MODULE] mkPortStallRecv#(String s)
        (PORT_STALL_RECV#(a))
            provisos (Bits#(a, sa),
                      Transmittable#(Maybe#(a)));

    Connection_Send#(Bool) creditToProducer <- mkConnectionSendUG(s + "__cred");

    PORT_RECV#(a) enqFromProducer <- mkPortRecvUG_L0(s + "__portDataEnq");

    FIFOF#(a) fifo <- mkUGSizedFIFOF(2);

    Reg#(Bool) producerCompleted <- mkReg(False); // producer model cycle completed
    Reg#(Bool) consumerCompleted <- mkReg(False); // consumer model cycle completed

    rule endModelCycle (producerCompleted && consumerCompleted);

        creditToProducer.send(fifo.notFull());
        producerCompleted <= False;
        consumerCompleted <= False;
        
    endrule

    rule processProducer (!producerCompleted && !enqFromProducer.ctrl.empty() && creditToProducer.notFull());

        let m_val <- enqFromProducer.receive();

        if (m_val matches tagged Valid .val)
        begin
            fifo.enq(val);
        end
        producerCompleted <= True;

    endrule
    
    // NOTE: These depend on higher-level guard checking by local controller
    //       and the consumer module.
    //       !ctrl.empty -> canDeq ? 
    //                          (peek* -> (doDeq | noDeq)) :
    //                          noDeq
    method Bool canDeq();
        return fifo.notEmpty();
    endmethod

    method Action doDeq();
        fifo.deq();
        consumerCompleted <= True;
    endmethod

    method a peek();
        return fifo.first(); 
    endmethod

    method Action noDeq();
        consumerCompleted <= True;
    endmethod

    interface PORT_CONTROL ctrl;
        method Bool empty() = consumerCompleted; // This is that we calculated the next state of the FIFO.
        method Bool full() = !creditToProducer.notFull(); // This is that the credit port is full.
        method Bool balanced() = True;
        method Bool light() = False;
        method Bool heavy() = False;
    endinterface
endmodule


module [HASIM_MODULE] mkPortStallSend_Multiplexed#(String s)
                       (PORT_STALL_SEND_MULTIPLEXED#(ni, a))
            provisos (Bits#(a, sa),
                      Transmittable#(Maybe#(a)));

    Vector#(ni, PORT_STALL_SEND#(a)) ports = newVector();
    Vector#(ni, PORT_CONTROL) portCtrls = newVector();

    for (Integer x = 0; x < valueOf(ni); x = x + 1)
    begin
      ports[x] <- mkPortStallSend(s + "_" + integerToString(x));
      portCtrls[x] = ports[x].ctrl;
    end

    interface ctrl = portCtrls;

    method Action doEnq(INSTANCE_ID#(ni) iid, a x);
        ports[iid].doEnq(x);
    endmethod

    method Action noEnq(INSTANCE_ID#(ni) iid);
        ports[iid].noEnq();
    endmethod

    method Bool   canEnq(INSTANCE_ID#(ni) iid);
        return ports[iid].canEnq();
    endmethod
  
endmodule

module [HASIM_MODULE] mkPortStallRecv_Multiplexed#(String s)
        (PORT_STALL_RECV_MULTIPLEXED#(ni, a))
            provisos (Bits#(a, sa),
                      Transmittable#(Maybe#(a)));

    Vector#(ni, PORT_STALL_RECV#(a)) ports = newVector();
    Vector#(ni, PORT_CONTROL) portCtrls = newVector();

    for (Integer x = 0; x < valueOf(ni); x = x + 1)
    begin
      ports[x] <- mkPortStallRecv(s + "_" + integerToString(x));
      portCtrls[x] = ports[x].ctrl;
    end

    interface ctrl = portCtrls;

    method Bool   canDeq(INSTANCE_ID#(ni) iid);
        return ports[iid].canDeq();
    endmethod

    method Action noDeq(INSTANCE_ID#(ni) iid);
        ports[iid].noDeq();
    endmethod

    method Action doDeq(INSTANCE_ID#(ni) iid);
        ports[iid].doDeq();
    endmethod

    method a      peek(INSTANCE_ID#(ni) iid);
        return ports[iid].peek();
    endmethod

endmodule
